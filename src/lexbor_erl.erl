%% @doc Erlang wrapper for Lexbor HTML parser via port interface.
%%
%% This module provides a high-level API for parsing, querying, and manipulating
%% HTML documents using the Lexbor C library. It supports both stateless operations
%% (for one-shot parsing) and stateful operations (for multiple queries on the same
%% document).
%%
%% The implementation uses a pool of worker processes, each managing an independent
%% C port to the Lexbor library. This provides:
%% <ul>
%%   <li>True parallelism across CPU cores</li>
%%   <li>Fault isolation - worker crashes don't affect other workers</li>
%%   <li>Thread safety through message passing</li>
%%   <li>Automatic recovery from crashes</li>
%% </ul>
%%
%% == Example Usage ==
%%
%% ```
%% % Start the application
%% ok = lexbor_erl:start().
%%
%% % Stateless: parse and normalize HTML
%% {ok, CleanHtml} = lexbor_erl:parse_serialize(<<"<p>Hello</p>">>).
%%
%% % Stateless: parse and select elements
%% {ok, Elements} = lexbor_erl:select_html(<<"<div><p>A</p><p>B</p></div>">>, <<"p">>).
%%
%% % Stateful: parse once, query multiple times
%% {ok, Doc} = lexbor_erl:parse(<<"<html><body><p class='a'>Hello</p></body></html>">>),
%% {ok, Nodes} = lexbor_erl:select(Doc, <<"p.a">>),
%% {ok, Html} = lexbor_erl:outer_html(Doc, hd(Nodes)),
%% ok = lexbor_erl:release(Doc).
%% '''
%%
%% @end
-module(lexbor_erl).

%% Types
-type doc_id()   :: integer().
-type node_ref() :: {node, integer()}.
-type selector() :: binary().
-type html_bin() :: binary() | iolist().
-type result(T)  :: {ok, T} | {error, term()}.

-export_type([doc_id/0, node_ref/0, selector/0, html_bin/0, result/1]).

-export([
    start/0, stop/0, alive/0,
    %% stateless
    parse_serialize/1, select_html/2,
    %% stateful
    parse/1, release/1,
    select/2, outer_html/2
]).

%% --- Lifecycle ---

%% @doc Start the lexbor_erl application and its dependencies.
%%
%% This starts the worker pool and all necessary processes. The pool size
%% can be configured via application environment or defaults to the number
%% of scheduler threads.
%%
%% @returns `ok' on success, `{error, Reason}' on failure
-spec start() -> ok | {error, term()}.
start() ->
    application:ensure_all_started(lexbor_erl), ok.

%% @doc Stop the lexbor_erl application.
%%
%% This shuts down all workers and releases all resources. Any documents
%% held by the application will be lost.
%%
%% @returns `ok'
-spec stop() -> ok.
stop() ->
    application:stop(lexbor_erl).

%% @doc Check if the lexbor_erl service is alive and ready.
%%
%% Returns true if at least one worker is alive and ready to accept requests.
%%
%% @returns `true' if service is available, `false' otherwise
-spec alive() -> boolean().
alive() ->
    lexbor_erl_pool:alive().

%% --- Stateless API ---

%% @doc Parse HTML and serialize it back (stateless operation).
%%
%% This is a minimal round-trip operation that parses HTML and returns
%% the normalized/serialized form. Useful for cleaning up malformed HTML.
%%
%% This is a stateless operation - the document is not retained in memory
%% after the call completes. Use {@link parse/1} if you need to perform
%% multiple operations on the same document.
%%
%% == Example ==
%% ```
%% {ok, Clean} = lexbor_erl:parse_serialize(<<"<p>Hello<br>World">>).
%% % Returns: {ok, <<"<html><head></head><body><p>Hello<br>World</p></body></html>">>}
%% '''
%%
%% @param Html The HTML content as binary or iolist
%% @returns `{ok, Binary}' with normalized HTML, or `{error, Reason}'
-spec parse_serialize(html_bin()) -> result(binary()).
parse_serialize(Html) when is_list(Html) ; is_binary(Html) ->
    lexbor_erl_pool:call(undefined, {<<"PARSE_SERIALIZE">>, iolist_to_binary(Html)}).

%% @doc Parse HTML and select elements by CSS selector (stateless operation).
%%
%% Single-shot operation that parses HTML, selects matching elements using
%% a CSS selector, and returns the outer HTML of each match.
%%
%% This is stateless - the document is not retained after the call.
%% Use {@link parse/1} + {@link select/2} + {@link outer_html/2} for
%% stateful operations with better performance for multiple queries.
%%
%% == Example ==
%% ```
%% Html = <<"<div><p class='a'>First</p><p class='b'>Second</p></div>">>,
%% {ok, Elements} = lexbor_erl:select_html(Html, <<"p.a">>).
%% % Returns: {ok, [<<"<p class=\"a\">First</p>">>]}
%% '''
%%
%% @param Html The HTML content as binary or iolist
%% @param Selector CSS selector string (e.g., `&lt;&lt;"p.class"&gt;&gt;', `&lt;&lt;"#id"&gt;&gt;')
%% @returns `{ok, [Binary]}' with list of matched elements' HTML, or `{error, Reason}'
-spec select_html(html_bin(), selector()) -> result([binary()]).
select_html(Html, Css) ->
    Payload = << (iolist_size(Css)):32/big, (iolist_to_binary(Css))/binary,
                 (iolist_to_binary(Html))/binary >>,
    case lexbor_erl_pool:call(undefined, {<<"SELECT_HTML">>, Payload}) of
        {ok, Bin} ->
            decode_bin_list(Bin);
        Error ->
            Error
    end.

%% --- Stateful API ---

%% @doc Parse HTML and return a document handle (stateful operation).
%%
%% Parses HTML and stores the document in a worker's memory. Returns an
%% opaque document ID that can be used for subsequent operations like
%% {@link select/2} and {@link outer_html/2}.
%%
%% <b>Important:</b> You must call {@link release/1} when done with the
%% document to free resources. Documents are stored in C memory and are
%% not garbage collected automatically.
%%
%% The document is assigned to a worker and all subsequent operations
%% on this document will be routed to the same worker automatically.
%%
%% == Example ==
%% ```
%% {ok, Doc} = lexbor_erl:parse(<<"<html><body><p>Hello</p></body></html>">>),
%% % ... perform operations on Doc ...
%% ok = lexbor_erl:release(Doc).  % Don't forget to release!
%% '''
%%
%% @param Html The HTML content as binary or iolist
%% @returns `{ok, DocId}' with document handle, or `{error, Reason}'
-spec parse(html_bin()) -> result(doc_id()).
parse(Html) ->
    %% Request: payload = Html
    %% Generate a worker ID for this new document (round-robin)
    PoolSize = lexbor_erl_pool:get_pool_size(),
    WorkerId = (erlang:system_time(microsecond) rem PoolSize) + 1,
    case lexbor_erl_pool:call({new_doc, WorkerId}, {<<"PARSE_DOC">>, iolist_to_binary(Html)}) of
        {ok, <<0, DocId:64/big-unsigned-integer>>} ->
            {ok, DocId};
        {ok, <<1, Err/binary>>} ->
            {error, binary_to_list(Err)};
        Other -> Other
    end.

%% @doc Release a document and free its resources.
%%
%% Frees the memory associated with a parsed document. After calling this,
%% the DocId becomes invalid and any further operations on it will fail.
%%
%% This MUST be called for every document created with {@link parse/1}
%% to avoid memory leaks in the C layer.
%%
%% == Example ==
%% ```
%% {ok, Doc} = lexbor_erl:parse(<<"<p>Hello</p>">>),
%% % ... use Doc ...
%% ok = lexbor_erl:release(Doc).
%% % Doc is now invalid
%% '''
%%
%% @param DocId Document handle returned by {@link parse/1}
%% @returns `ok' on success, `{error, Reason}' if document not found
-spec release(doc_id()) -> ok | {error, term()}.
release(DocId) ->
    Req = <<DocId:64/big-unsigned-integer>>,
    %% Route by DocId to the worker that has this document
    case lexbor_erl_pool:call(DocId, {<<"RELEASE_DOC">>, Req}) of
        {ok, <<0>>} -> ok;
        {ok, <<1, Err/binary>>} -> {error, binary_to_list(Err)};
        Other -> Other
    end.

%% @doc Select nodes from a document using a CSS selector.
%%
%% Queries a parsed document using a CSS selector and returns handles
%% to all matching nodes. The node handles can be used with
%% {@link outer_html/2} to retrieve the HTML content.
%%
%% Supports most CSS3 selectors including:
%% <ul>
%%   <li>Element selectors: "p", "div"</li>
%%   <li>Class selectors: ".class", "p.class"</li>
%%   <li>ID selectors: "#id", "div#id"</li>
%%   <li>Attribute selectors: "[attr]", "[attr=value]"</li>
%%   <li>Combinators: "div p" (descendant), "div &gt; p" (child)</li>
%%   <li>Pseudo-classes: ":first-child", ":nth-child(n)"</li>
%% </ul>
%%
%% == Example ==
%% ```
%% {ok, Doc} = lexbor_erl:parse(<<"<div><p class='a'>A</p><p class='b'>B</p></div>">>),
%% {ok, Nodes} = lexbor_erl:select(Doc, <<"p.a">>),
%% % Nodes: [{node, NodeHandle}]
%% ok = lexbor_erl:release(Doc).
%% '''
%%
%% @param DocId Document handle from {@link parse/1}
%% @param Selector CSS selector string
%% @returns `{ok, [NodeRef]}' with list of matching node handles, or `{error, Reason}'
-spec select(doc_id(), selector()) -> result([node_ref()]).
select(DocId, Css) ->
    CS = iolist_to_binary(Css),
    Req = <<DocId:64/big-unsigned-integer, (byte_size(CS)):32/big, CS/binary>>,
    %% Route by DocId to the worker that has this document
    case lexbor_erl_pool:call(DocId, {<<"SELECT_NODES">>, Req}) of
        {ok, <<0, Cnt:32/big, Rest/binary>>} ->
            {ok, read_handles(Cnt, Rest, [])};
        {ok, <<1, Err/binary>>} ->
            {error, binary_to_list(Err)};
        Other -> Other
    end.

%% @doc Get the outer HTML of a node.
%%
%% Returns the HTML representation of a node, including the node itself
%% and all its descendants. The node handle must be from a {@link select/2}
%% operation on the same document.
%%
%% == Example ==
%% ```
%% {ok, Doc} = lexbor_erl:parse(<<"<div><p>Hello</p></div>">>),
%% {ok, [Node]} = lexbor_erl:select(Doc, <<"p">>),
%% {ok, Html} = lexbor_erl:outer_html(Doc, Node),
%% % Html: <<"<p>Hello</p>">>
%% ok = lexbor_erl:release(Doc).
%% '''
%%
%% @param DocId Document handle from {@link parse/1}
%% @param NodeRef Node handle from {@link select/2}
%% @returns `{ok, Binary}' with the node's HTML, or `{error, Reason}'
-spec outer_html(doc_id(), node_ref()) -> result(binary()).
outer_html(DocId, {node, Handle}) ->
    Req = <<DocId:64/big-unsigned-integer, Handle:64/big-unsigned-integer>>,
    %% Route by DocId to the worker that has this document
    case lexbor_erl_pool:call(DocId, {<<"OUTER_HTML">>, Req}) of
        {ok, <<0, Len:32/big, Bin:Len/binary>>} -> {ok, Bin};
        {ok, <<1, Err/binary>>} -> {error, binary_to_list(Err)};
        Other -> Other
    end.

%% --- Internal helpers ---

%% Binary list decoder: <<N:32, [L:32, B:L]*>>
decode_bin_list(Bin) when is_binary(Bin) ->
    try decode_bin_list_(Bin, []) of
        {ok, Rev} -> {ok, lists:reverse(Rev)}
    catch
        _:Reason -> {error, {bad_reply, Reason}}
    end.

decode_bin_list_(<<Count:32/big, Rest/binary>>, Acc) ->
    decode_items_(Count, Rest, Acc).

decode_items_(0, Rest, Acc) when Rest =:= <<>> ->
    {ok, Acc};
decode_items_(N, <<Len:32/big, Item:Len/binary, Tail/binary>>, Acc) when N > 0 ->
    decode_items_(N-1, Tail, [Item|Acc]);
decode_items_(_N, _Rest, _Acc) ->
    erlang:error(invalid_payload).

%% Read node handles from binary
read_handles(0, <<>>, Acc) -> lists:reverse(Acc);
read_handles(N, <<Handle:64/big-unsigned-integer, Rest/binary>>, Acc) when N > 0 ->
    read_handles(N-1, Rest, [{node, Handle} | Acc]).

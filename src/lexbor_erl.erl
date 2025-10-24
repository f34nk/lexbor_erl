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
    select/2, outer_html/2,
    %% DOM manipulation - attributes
    get_attribute/3, set_attribute/4, remove_attribute/3,
    %% DOM manipulation - text and HTML
    get_text/2, set_text/3,
    inner_html/2, set_inner_html/3,
    serialize/1,
    %% DOM manipulation - node creation and tree manipulation
    create_element/2, append_child/3, insert_before/4, remove_node/2
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

%% --- DOM Manipulation API ---

%% @doc Get an attribute value from an element node.
%%
%% Retrieves the value of a specified attribute from an element node.
%% Returns `{ok, undefined}' if the attribute does not exist.
%%
%% Only works on element nodes - other node types will return an error.
%%
%% == Example ==
%% ```
%% {ok, Doc} = lexbor_erl:parse(<<"<a href='/home' class='link'>Home</a>">>),
%% {ok, [Link]} = lexbor_erl:select(Doc, <<"a">>),
%% {ok, Href} = lexbor_erl:get_attribute(Doc, Link, <<"href">>),
%% % Href: <<"/home">>
%% {ok, Title} = lexbor_erl:get_attribute(Doc, Link, <<"title">>),
%% % Title: undefined (attribute doesn't exist)
%% '''
%%
%% @param DocId Document handle from {@link parse/1}
%% @param NodeRef Node handle from {@link select/2}
%% @param AttrName Attribute name as binary
%% @returns `{ok, Binary}' with attribute value, `{ok, undefined}' if not found, or `{error, Reason}'
-spec get_attribute(doc_id(), node_ref(), binary()) -> 
    {ok, binary() | undefined} | {error, term()}.
get_attribute(DocId, {node, Handle}, AttrName) when is_binary(AttrName) ->
    AttrLen = byte_size(AttrName),
    Payload = <<DocId:64/big-unsigned-integer,
                Handle:64/big-unsigned-integer,
                AttrLen:32/big,
                AttrName/binary>>,
    case lexbor_erl_pool:call(DocId, {<<"GET_ATTRIBUTE">>, Payload}) of
        {ok, <<0, ValLen:32/big, Value:ValLen/binary>>} ->
            {ok, Value};
        {ok, <<2>>} ->
            {ok, undefined};  % Attribute not found
        {ok, <<1, Err/binary>>} ->
            {error, binary_to_list(Err)};
        Other ->
            Other
    end.

%% @doc Set an attribute value on an element node.
%%
%% Sets the specified attribute to the given value. If the attribute already
%% exists, its value is updated. If it doesn't exist, it's created.
%%
%% Only works on element nodes - other node types will return an error.
%%
%% == Example ==
%% ```
%% {ok, Doc} = lexbor_erl:parse(<<"<a href='/old'>Link</a>">>),
%% {ok, [Link]} = lexbor_erl:select(Doc, <<"a">>),
%% ok = lexbor_erl:set_attribute(Doc, Link, <<"href">>, <<"/new">>),
%% ok = lexbor_erl:set_attribute(Doc, Link, <<"target">>, <<"_blank">>),
%% {ok, Html} = lexbor_erl:outer_html(Doc, Link),
%% % Html: <<"<a href=\"/new\" target=\"_blank\">Link</a>">>
%% '''
%%
%% @param DocId Document handle from {@link parse/1}
%% @param NodeRef Node handle from {@link select/2}
%% @param AttrName Attribute name as binary
%% @param Value Attribute value as binary
%% @returns `ok' on success, `{error, Reason}' on failure
-spec set_attribute(doc_id(), node_ref(), binary(), binary()) -> 
    ok | {error, term()}.
set_attribute(DocId, {node, Handle}, AttrName, Value) 
        when is_binary(AttrName), is_binary(Value) ->
    AttrLen = byte_size(AttrName),
    ValLen = byte_size(Value),
    Payload = <<DocId:64/big-unsigned-integer,
                Handle:64/big-unsigned-integer,
                AttrLen:32/big,
                AttrName/binary,
                ValLen:32/big,
                Value/binary>>,
    case lexbor_erl_pool:call(DocId, {<<"SET_ATTRIBUTE">>, Payload}) of
        {ok, <<0>>} -> ok;
        {ok, <<1, Err/binary>>} -> {error, binary_to_list(Err)};
        Other -> Other
    end.

%% @doc Remove an attribute from an element node.
%%
%% Removes the specified attribute from the element if it exists.
%% Returns success even if the attribute didn't exist.
%%
%% Only works on element nodes - other node types will return an error.
%%
%% == Example ==
%% ```
%% {ok, Doc} = lexbor_erl:parse(<<"<a href='/' target='_blank'>Link</a>">>),
%% {ok, [Link]} = lexbor_erl:select(Doc, <<"a">>),
%% ok = lexbor_erl:remove_attribute(Doc, Link, <<"target">>),
%% {ok, Html} = lexbor_erl:outer_html(Doc, Link),
%% % Html: <<"<a href=\"/\">Link</a>">>
%% '''
%%
%% @param DocId Document handle from {@link parse/1}
%% @param NodeRef Node handle from {@link select/2}
%% @param AttrName Attribute name as binary
%% @returns `ok' on success, `{error, Reason}' on failure
-spec remove_attribute(doc_id(), node_ref(), binary()) -> 
    ok | {error, term()}.
remove_attribute(DocId, {node, Handle}, AttrName) when is_binary(AttrName) ->
    AttrLen = byte_size(AttrName),
    Payload = <<DocId:64/big-unsigned-integer,
                Handle:64/big-unsigned-integer,
                AttrLen:32/big,
                AttrName/binary>>,
    case lexbor_erl_pool:call(DocId, {<<"REMOVE_ATTRIBUTE">>, Payload}) of
        {ok, <<0>>} -> ok;
        {ok, <<1, Err/binary>>} -> {error, binary_to_list(Err)};
        Other -> Other
    end.

%% @doc Get the text content of a node.
%%
%% Extracts all text content from the node and its descendants, without any HTML tags.
%% This recursively collects all text nodes within the element.
%%
%% == Example ==
%% ```
%% {ok, Doc} = lexbor_erl:parse(<<"<div>Hello <b>World</b>!</div>">>),
%% {ok, [Div]} = lexbor_erl:select(Doc, <<"div">>),
%% {ok, Text} = lexbor_erl:get_text(Doc, Div),
%% % Text: <<"Hello World!">>
%% '''
%%
%% @param DocId Document handle from {@link parse/1}
%% @param NodeRef Node handle from {@link select/2}
%% @returns `{ok, Binary}' with text content, or `{error, Reason}'
-spec get_text(doc_id(), node_ref()) -> {ok, binary()} | {error, term()}.
get_text(DocId, {node, Handle}) ->
    Payload = <<DocId:64/big-unsigned-integer,
                Handle:64/big-unsigned-integer>>,
    case lexbor_erl_pool:call(DocId, {<<"GET_TEXT">>, Payload}) of
        {ok, <<0, TextLen:32/big, Text:TextLen/binary>>} ->
            {ok, Text};
        {ok, <<1, Err/binary>>} ->
            {error, binary_to_list(Err)};
        Other ->
            Other
    end.

%% @doc Set the text content of a node.
%%
%% Replaces all children of the node with a single text node containing the specified text.
%% Any existing child elements will be removed.
%%
%% == Example ==
%% ```
%% {ok, Doc} = lexbor_erl:parse(<<"<div>Old <b>text</b></div>">>),
%% {ok, [Div]} = lexbor_erl:select(Doc, <<"div">>),
%% ok = lexbor_erl:set_text(Doc, Div, <<"New text">>),
%% {ok, Html} = lexbor_erl:outer_html(Doc, Div),
%% % Html: <<"<div>New text</div>">>
%% '''
%%
%% @param DocId Document handle from {@link parse/1}
%% @param NodeRef Node handle from {@link select/2}
%% @param Text New text content as binary
%% @returns `ok' on success, `{error, Reason}' on failure
-spec set_text(doc_id(), node_ref(), binary()) -> ok | {error, term()}.
set_text(DocId, {node, Handle}, Text) when is_binary(Text) ->
    TextLen = byte_size(Text),
    Payload = <<DocId:64/big-unsigned-integer,
                Handle:64/big-unsigned-integer,
                TextLen:32/big,
                Text/binary>>,
    case lexbor_erl_pool:call(DocId, {<<"SET_TEXT">>, Payload}) of
        {ok, <<0>>} -> ok;
        {ok, <<1, Err/binary>>} -> {error, binary_to_list(Err)};
        Other -> Other
    end.

%% @doc Get the inner HTML of a node.
%%
%% Returns the HTML content of all children, excluding the element's own tags.
%% Similar to {@link outer_html/2} but without the container element.
%%
%% == Example ==
%% ```
%% {ok, Doc} = lexbor_erl:parse(<<"<div><p>Hello</p><p>World</p></div>">>),
%% {ok, [Div]} = lexbor_erl:select(Doc, <<"div">>),
%% {ok, Inner} = lexbor_erl:inner_html(Doc, Div),
%% % Inner: <<"<p>Hello</p><p>World</p>">>
%% '''
%%
%% @param DocId Document handle from {@link parse/1}
%% @param NodeRef Node handle from {@link select/2}
%% @returns `{ok, Binary}' with inner HTML, or `{error, Reason}'
-spec inner_html(doc_id(), node_ref()) -> {ok, binary()} | {error, term()}.
inner_html(DocId, {node, Handle}) ->
    Payload = <<DocId:64/big-unsigned-integer,
                Handle:64/big-unsigned-integer>>,
    case lexbor_erl_pool:call(DocId, {<<"INNER_HTML">>, Payload}) of
        {ok, <<0, HtmlLen:32/big, Html:HtmlLen/binary>>} ->
            {ok, Html};
        {ok, <<1, Err/binary>>} ->
            {error, binary_to_list(Err)};
        Other ->
            Other
    end.

%% @doc Set the inner HTML of a node.
%%
%% Parses the HTML string and replaces all children of the element with the parsed content.
%% The HTML is parsed in the context of the element's tag.
%%
%% <b>Warning:</b> Be careful with untrusted input as this directly parses HTML.
%% Consider using {@link set_text/3} for plain text content.
%%
%% == Example ==
%% ```
%% {ok, Doc} = lexbor_erl:parse(<<"<div>Old</div>">>),
%% {ok, [Div]} = lexbor_erl:select(Doc, <<"div">>),
%% ok = lexbor_erl:set_inner_html(Doc, Div, <<"<p>New</p><p>Content</p>">>),
%% {ok, Html} = lexbor_erl:outer_html(Doc, Div),
%% % Html: <<"<div><p>New</p><p>Content</p></div>">>
%% '''
%%
%% @param DocId Document handle from {@link parse/1}
%% @param NodeRef Node handle from {@link select/2}
%% @param Html HTML content as binary
%% @returns `ok' on success, `{error, Reason}' on failure
-spec set_inner_html(doc_id(), node_ref(), binary()) -> ok | {error, term()}.
set_inner_html(DocId, {node, Handle}, Html) when is_binary(Html) ->
    HtmlLen = byte_size(Html),
    Payload = <<DocId:64/big-unsigned-integer,
                Handle:64/big-unsigned-integer,
                HtmlLen:32/big,
                Html/binary>>,
    case lexbor_erl_pool:call(DocId, {<<"SET_INNER_HTML">>, Payload}) of
        {ok, <<0>>} -> ok;
        {ok, <<1, Err/binary>>} -> {error, binary_to_list(Err)};
        Other -> Other
    end.

%% @doc Serialize the entire document to HTML.
%%
%% Returns the complete HTML representation of the document, including the
%% doctype and all elements. Use this after making modifications to get the
%% final HTML output.
%%
%% == Example ==
%% ```
%% {ok, Doc} = lexbor_erl:parse(<<"<div>Hello</div>">>),
%% {ok, [Div]} = lexbor_erl:select(Doc, <<"div">>),
%% ok = lexbor_erl:set_attribute(Doc, Div, <<"id">>, <<"main">>),
%% {ok, FinalHtml} = lexbor_erl:serialize(Doc),
%% % FinalHtml: <<"<html><head></head><body><div id=\"main\">Hello</div></body></html>">>
%% '''
%%
%% @param DocId Document handle from {@link parse/1}
%% @returns `{ok, Binary}' with complete HTML document, or `{error, Reason}'
-spec serialize(doc_id()) -> {ok, binary()} | {error, term()}.
serialize(DocId) ->
    Payload = <<DocId:64/big-unsigned-integer>>,
    case lexbor_erl_pool:call(DocId, {<<"SERIALIZE_DOC">>, Payload}) of
        {ok, <<0, HtmlLen:32/big, Html:HtmlLen/binary>>} ->
            {ok, Html};
        {ok, <<1, Err/binary>>} ->
            {error, binary_to_list(Err)};
        Other ->
            Other
    end.

%% @doc Create a new element node.
%%
%% Creates a new element with the specified tag name. The element is created but
%% not attached to the document tree. Use {@link append_child/3} or {@link insert_before/4}
%% to add it to the document.
%%
%% == Example ==
%% ```
%% {ok, Doc} = lexbor_erl:parse(<<"<html><body></body></html>">>),
%% {ok, NewDiv} = lexbor_erl:create_element(Doc, <<"div">>),
%% ok = lexbor_erl:set_attribute(Doc, NewDiv, <<"id">>, <<"new">>),
%% ok = lexbor_erl:set_text(Doc, NewDiv, <<"New content">>),
%% {ok, [Body]} = lexbor_erl:select(Doc, <<"body">>),
%% ok = lexbor_erl:append_child(Doc, Body, NewDiv),
%% '''
%%
%% @param DocId Document handle from {@link parse/1}
%% @param TagName Element tag name as binary (e.g., `<<"div">>',  `<<"span">>')
%% @returns `{ok, NodeRef}' with new node handle, or `{error, Reason}'
-spec create_element(doc_id(), binary()) -> {ok, node_ref()} | {error, term()}.
create_element(DocId, TagName) when is_binary(TagName) ->
    TagLen = byte_size(TagName),
    Payload = <<DocId:64/big-unsigned-integer,
                TagLen:32/big,
                TagName/binary>>,
    case lexbor_erl_pool:call(DocId, {<<"CREATE_ELEMENT">>, Payload}) of
        {ok, <<0, Handle:64/big-unsigned-integer>>} ->
            {ok, {node, Handle}};
        {ok, <<1, Err/binary>>} ->
            {error, binary_to_list(Err)};
        Other ->
            Other
    end.

%% @doc Append a child node to a parent element.
%%
%% Adds the child node as the last child of the parent. If the child was previously
%% attached elsewhere, it will be moved to the new location.
%%
%% == Example ==
%% ```
%% {ok, Doc} = lexbor_erl:parse(<<"<ul id='list'></ul>">>),
%% {ok, [List]} = lexbor_erl:select(Doc, <<"#list">>),
%% {ok, Item} = lexbor_erl:create_element(Doc, <<"li">>),
%% ok = lexbor_erl:set_text(Doc, Item, <<"Item 1">>),
%% ok = lexbor_erl:append_child(Doc, List, Item),
%% '''
%%
%% @param DocId Document handle from {@link parse/1}
%% @param Parent Parent node handle
%% @param Child Child node handle to append
%% @returns `ok' on success, `{error, Reason}' on failure
-spec append_child(doc_id(), node_ref(), node_ref()) -> ok | {error, term()}.
append_child(DocId, {node, ParentHandle}, {node, ChildHandle}) ->
    Payload = <<DocId:64/big-unsigned-integer,
                ParentHandle:64/big-unsigned-integer,
                ChildHandle:64/big-unsigned-integer>>,
    case lexbor_erl_pool:call(DocId, {<<"APPEND_CHILD">>, Payload}) of
        {ok, <<0>>} -> ok;
        {ok, <<1, Err/binary>>} -> {error, binary_to_list(Err)};
        Other -> Other
    end.

%% @doc Insert a node before a reference node.
%%
%% Inserts the new node as a child of parent, positioned before the reference node.
%% The reference node must be a child of the parent.
%%
%% == Example ==
%% ```
%% {ok, Doc} = lexbor_erl:parse(<<"<ul><li>Second</li></ul>">>),
%% {ok, [List]} = lexbor_erl:select(Doc, <<"ul">>),
%% {ok, [Second]} = lexbor_erl:select(Doc, <<"li">>),
%% {ok, First} = lexbor_erl:create_element(Doc, <<"li">>),
%% ok = lexbor_erl:set_text(Doc, First, <<"First">>),
%% ok = lexbor_erl:insert_before(Doc, List, First, Second),
%% % Now: <ul><li>First</li><li>Second</li></ul>
%% '''
%%
%% @param DocId Document handle from {@link parse/1}
%% @param Parent Parent node handle
%% @param NewNode Node to insert
%% @param RefNode Reference node to insert before
%% @returns `ok' on success, `{error, Reason}' on failure
-spec insert_before(doc_id(), node_ref(), node_ref(), node_ref()) -> 
    ok | {error, term()}.
insert_before(DocId, {node, ParentHandle}, {node, NewNodeHandle}, {node, RefNodeHandle}) ->
    Payload = <<DocId:64/big-unsigned-integer,
                ParentHandle:64/big-unsigned-integer,
                NewNodeHandle:64/big-unsigned-integer,
                RefNodeHandle:64/big-unsigned-integer>>,
    case lexbor_erl_pool:call(DocId, {<<"INSERT_BEFORE">>, Payload}) of
        {ok, <<0>>} -> ok;
        {ok, <<1, Err/binary>>} -> {error, binary_to_list(Err)};
        Other -> Other
    end.

%% @doc Remove a node from its parent.
%%
%% Removes the node from its parent in the tree. The node is not destroyed and
%% can potentially be reinserted elsewhere.
%%
%% == Example ==
%% ```
%% {ok, Doc} = lexbor_erl:parse(<<"<div><p>Remove me</p><p>Keep</p></div>">>),
%% {ok, Paragraphs} = lexbor_erl:select(Doc, <<"p">>),
%% [ToRemove|_] = Paragraphs,
%% ok = lexbor_erl:remove_node(Doc, ToRemove),
%% % Now only one <p> remains
%% '''
%%
%% @param DocId Document handle from {@link parse/1}
%% @param NodeRef Node to remove
%% @returns `ok' on success, `{error, Reason}' on failure
-spec remove_node(doc_id(), node_ref()) -> ok | {error, term()}.
remove_node(DocId, {node, Handle}) ->
    Payload = <<DocId:64/big-unsigned-integer,
                Handle:64/big-unsigned-integer>>,
    case lexbor_erl_pool:call(DocId, {<<"REMOVE_NODE">>, Payload}) of
        {ok, <<0>>} -> ok;
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

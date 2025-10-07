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

-spec start() -> ok | {error, term()}.
start() ->
    application:ensure_all_started(lexbor_erl), ok.

-spec stop() -> ok.
stop() ->
    application:stop(lexbor_erl).

-spec alive() -> boolean().
alive() ->
    lexbor_erl_port:alive().

%% --- Stateless API ---

%% Minimal round-trip: send HTML, get "serialized" HTML back
-spec parse_serialize(html_bin()) -> result(binary()).
parse_serialize(Html) when is_list(Html) ; is_binary(Html) ->
    lexbor_erl_port:call(<<"PARSE_SERIALIZE">>, iolist_to_binary(Html)).

%% Single-shot parse -> select -> serialize(each node outerHTML)
-spec select_html(html_bin(), selector()) -> result([binary()]).
select_html(Html, Css) ->
    Payload = << (iolist_size(Css)):32/big, (iolist_to_binary(Css))/binary,
                 (iolist_to_binary(Html))/binary >>,
    case lexbor_erl_port:call(<<"SELECT_HTML">>, Payload) of
        {ok, Bin} ->
            decode_bin_list(Bin);
        Error ->
            Error
    end.

%% --- Stateful API ---

-spec parse(html_bin()) -> result(doc_id()).
parse(Html) ->
    %% Request: payload = Html
    case lexbor_erl_port:call(<<"PARSE_DOC">>, iolist_to_binary(Html)) of
        {ok, <<0, DocId:64/big-unsigned-integer>>} ->
            {ok, DocId};
        {ok, <<1, Err/binary>>} ->
            {error, binary_to_list(Err)};
        Other -> Other
    end.

-spec release(doc_id()) -> ok | {error, term()}.
release(DocId) ->
    Req = <<DocId:64/big-unsigned-integer>>,
    case lexbor_erl_port:call(<<"RELEASE_DOC">>, Req) of
        {ok, <<0>>} -> ok;
        {ok, <<1, Err/binary>>} -> {error, binary_to_list(Err)};
        Other -> Other
    end.

-spec select(doc_id(), selector()) -> result([node_ref()]).
select(DocId, Css) ->
    CS = iolist_to_binary(Css),
    Req = <<DocId:64/big-unsigned-integer, (byte_size(CS)):32/big, CS/binary>>,
    case lexbor_erl_port:call(<<"SELECT_NODES">>, Req) of
        {ok, <<0, Cnt:32/big, Rest/binary>>} ->
            {ok, read_handles(Cnt, Rest, [])};
        {ok, <<1, Err/binary>>} ->
            {error, binary_to_list(Err)};
        Other -> Other
    end.

-spec outer_html(doc_id(), node_ref()) -> result(binary()).
outer_html(DocId, {node, Handle}) ->
    Req = <<DocId:64/big-unsigned-integer, Handle:64/big-unsigned-integer>>,
    case lexbor_erl_port:call(<<"OUTER_HTML">>, Req) of
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

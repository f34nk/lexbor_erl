-module(lexbor_erl_pool).
-behaviour(gen_server).

-export([start_link/1, call/2, alive/0, get_pool_size/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    workers :: [pid()],
    size :: pos_integer()
}).

%% ===================================================================
%% Public API
%% ===================================================================

-spec start_link(pos_integer()) -> {ok, pid()} | {error, term()}.
start_link(PoolSize) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [PoolSize], []).

-spec alive() -> boolean().
alive() ->
    whereis(?SERVER) =/= undefined andalso
    case catch gen_server:call(?SERVER, alive, 1000) of
        true -> true;
        _ -> false
    end.

-spec get_pool_size() -> pos_integer().
get_pool_size() ->
    gen_server:call(?SERVER, get_pool_size).

%% Route calls to appropriate worker
%% Key can be:
%%   - undefined (stateless ops) -> round-robin
%%   - {new_doc, WorkerId} -> specific worker for new document
%%   - DocId (integer) -> extract worker from DocId and decode for payload
-spec call(undefined | {new_doc, pos_integer()} | non_neg_integer(), {binary(), binary()}) -> 
    {ok, binary()} | {error, term()}.
call(Key, {CmdTag, Payload}) ->
    {Worker, MaybeWorkerId, MaybeDecodedPayload} = get_worker_and_payload(Key, Payload),
    % Call the worker with potentially decoded payload
    ActualPayload = case MaybeDecodedPayload of
        undefined -> Payload;
        Decoded -> Decoded
    end,
    % Call the worker and potentially rewrite the DocId in the response
    case lexbor_erl_worker:call(Worker, CmdTag, ActualPayload) of
        {ok, <<0, DocId:64/big-unsigned-integer>>} when MaybeWorkerId =/= undefined ->
            % This is a PARSE_DOC response - encode worker ID into the high bits
            EncodedDocId = encode_doc_id(MaybeWorkerId, DocId),
            {ok, <<0, EncodedDocId:64/big-unsigned-integer>>};
        Other ->
            Other
    end.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([PoolSize]) when PoolSize > 0 ->
    process_flag(trap_exit, true),
    
    % Start worker processes
    Workers = lists:map(fun(I) ->
        {ok, Pid} = lexbor_erl_worker:start_link(I),
        link(Pid),
        Pid
    end, lists:seq(1, PoolSize)),
    
    {ok, #state{workers=Workers, size=PoolSize}}.

handle_call(get_workers, _From, #state{workers=Workers}=State) ->
    {reply, Workers, State};

handle_call(get_pool_size, _From, #state{size=Size}=State) ->
    {reply, Size, State};

handle_call(alive, _From, #state{workers=Workers}=State) ->
    % Check if at least one worker is alive
    Alive = lists:any(fun(Pid) -> is_process_alive(Pid) end, Workers),
    {reply, Alive, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, #state{workers=Workers}=State) ->
    case lists:member(Pid, Workers) of
        true ->
            error_logger:error_msg("lexbor_erl worker ~p died: ~p~n", [Pid, Reason]),
            % Worker died - this is bad, let the supervisor restart us
            {stop, {worker_died, Pid, Reason}, State};
        false ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{workers=Workers}) ->
    % Shutdown all workers gracefully
    [begin
        unlink(Pid),
        exit(Pid, shutdown)
    end || Pid <- Workers],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% Get worker for a given key and potentially decode payload
%% Returns {Worker, MaybeWorkerId, MaybeDecodedPayload}
-spec get_worker_and_payload(undefined | {new_doc, pos_integer()} | non_neg_integer(), binary()) -> 
    {pid(), pos_integer() | undefined, binary() | undefined}.
get_worker_and_payload(Key, Payload) ->
    Workers = gen_server:call(?SERVER, get_workers),
    {Index, ReturnWorkerId, DecodedPayload} = case Key of
        undefined ->
            % Stateless operation - round-robin based on current time
            {erlang:phash2(erlang:monotonic_time(), length(Workers)), undefined, undefined};
        {new_doc, WorkerId} ->
            % New document on specific worker
            {WorkerId - 1, WorkerId, undefined};
        DocId when is_integer(DocId) ->
            % Stateful operation - extract worker ID from DocId
            % and decode the DocId in the payload
            {WorkerId, RealDocId} = decode_doc_id(DocId, length(Workers)),
            NewPayload = decode_payload_docid(Payload, RealDocId),
            {WorkerId - 1, undefined, NewPayload}
    end,
    {lists:nth(Index + 1, Workers), ReturnWorkerId, DecodedPayload}.

%% Decode DocId in payload (replace encoded DocId with real DocId)
decode_payload_docid(<<_EncodedDocId:64/big-unsigned-integer, Rest/binary>>, RealDocId) ->
    % Payload starts with DocId - replace it
    <<RealDocId:64/big-unsigned-integer, Rest/binary>>;
decode_payload_docid(Payload, _RealDocId) ->
    % Payload doesn't start with DocId or has different format
    Payload.

%% Encode worker ID into the high 8 bits of DocId
encode_doc_id(WorkerId, DocId) ->
    (WorkerId bsl 56) bor (DocId band 16#00FFFFFFFFFFFFFF).

%% Decode worker ID from DocId
%% Returns {WorkerId, RealDocId}
decode_doc_id(EncodedDocId, _PoolSize) ->
    WorkerId = (EncodedDocId bsr 56) band 16#FF,
    RealDocId = EncodedDocId band 16#00FFFFFFFFFFFFFF,
    {WorkerId, RealDocId}.

-module(lexbor_erl_worker).
-behaviour(gen_server).

-export([start_link/1, call/3, worker_name/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    port :: port(),
    worker_id :: pos_integer()
}).

%% ===================================================================
%% Public API
%% ===================================================================

-spec start_link(pos_integer()) -> {ok, pid()} | {error, term()}.
start_link(WorkerId) ->
    gen_server:start_link({local, worker_name(WorkerId)}, ?MODULE, [WorkerId], []).

%% Generate registered name for worker
worker_name(WorkerId) ->
    list_to_atom("lexbor_erl_worker_" ++ integer_to_list(WorkerId)).

%% Public call: send {CmdTag, PayloadBin} framed with {packet,4}
-spec call(pid(), binary(), binary()) -> {ok, binary()} | {error, term()}.
call(Worker, CmdTag, Payload) ->
    gen_server:call(Worker, {call, CmdTag, Payload}, infinity).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([WorkerId]) ->
    process_flag(trap_exit, true),
    
    % Get the port command path
    PortCmd = case application:get_env(lexbor_erl, port_cmd) of
        {ok, Cmd} ->
            Cmd;
        undefined ->
            % Find the priv directory for this application
            case code:priv_dir(lexbor_erl) of
                {error, bad_name} ->
                    % Fallback for development/test scenarios
                    BeamDir = filename:dirname(code:which(?MODULE)),
                    filename:join([BeamDir, "..", "priv", "lexbor_port"]);
                PrivDir ->
                    filename:join(PrivDir, "lexbor_port")
            end
    end,
    
    % Verify the port executable exists before trying to open it
    case filelib:is_file(PortCmd) of
        false ->
            error_logger:error_msg("Port executable not found at: ~p~n", [PortCmd]),
            {stop, {port_not_found, PortCmd}};
        true ->
            Port = open_port({spawn_executable, PortCmd},
                           [{packet, 4}, binary, exit_status, use_stdio, stderr_to_stdout]),
            {ok, #state{port=Port, worker_id=WorkerId}}
    end.

handle_call({call, CmdTag, Payload}, _From, #state{port=Port}=S) ->
    %% Wire format:
    %% <<1-byte version, 16-byte CmdTag, payload-binary>>
    Version = <<1>>,
    Tag16 = pad16(CmdTag),
    true = port_command(Port, <<Version/binary, Tag16/binary, Payload/binary>>),
    receive
        {Port, {data, Reply}} ->
            %% Reply is binary: <<1, Tag16, Data/binary>> (same shape)
            case Reply of
                <<1, _Tag:16/binary, Data/binary>> ->
                    {reply, {ok, Data}, S};
                _ ->
                    {reply, {error, bad_reply}, S}
            end;
        {'EXIT', Port, Reason} ->
            {reply, {error, {port_exit, Reason}}, S}
    after application:get_env(lexbor_erl, op_timeout_ms, 3000) ->
            {reply, {error, timeout}, S}
    end.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info({'EXIT', Port, Status}, #state{port=Port, worker_id=WorkerId}=S) ->
    error_logger:error_msg("lexbor_erl worker ~p port exited: ~p~n", [WorkerId, Status]),
    {stop, port_died, S};
handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, #state{port=Port}) ->
    catch port_close(Port), 
    ok.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

pad16(Bin) when is_binary(Bin) ->
    <<(binary:part(Bin, 0, min(byte_size(Bin),16)))/binary,
      (pad_zeros(16 - min(byte_size(Bin),16)))/binary>>.

pad_zeros(N) when N =< 0 -> <<>>;
pad_zeros(N) ->
    <<0:8, (pad_zeros(N-1))/binary>>.

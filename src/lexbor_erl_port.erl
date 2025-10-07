%% @doc Port manager for lexbor_erl - manages communication with the C port.
%%
%% This gen_server manages a single OS port that runs the `lexbor_port'
%% C executable. It handles:
%% <ul>
%%   <li>Starting and supervising the C port process</li>
%%   <li>Sending commands to the port with proper framing</li>
%%   <li>Receiving and routing responses back to callers</li>
%%   <li>Handling port crashes and timeouts</li>
%% </ul>
%%
%% == Port Protocol ==
%%
%% The protocol uses `{packet, 4}' framing (4-byte length prefix) with
%% the following binary format for requests and responses:
%%
%% Request: `&lt;&lt;Version:8, Tag:128, Payload/binary&gt;&gt;'
%% Response: `&lt;&lt;Version:8, Tag:128, Data/binary&gt;&gt;'
%%
%% Where:
%% <ul>
%%   <li>`Version' is always 1</li>
%%   <li>`Tag' is a 16-byte command identifier (padded)</li>
%%   <li>`Payload' and `Data' are operation-specific binary data</li>
%% </ul>
%%
%% == Thread Safety ==
%%
%% This module ensures thread-safe access to the C library by serializing
%% all requests through a single gen_server. Multiple Erlang processes
%% can call concurrently - requests will be queued and processed sequentially.
%%
%% @end
-module(lexbor_erl_port).
-behaviour(gen_server).

-export([start_link/0, alive/0, call/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {port :: port()}).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Start the port manager as a linked process.
%%
%% This function is typically called by the OTP application supervisor.
%% It starts the gen_server, opens the port to the C executable, and
%% registers itself locally as `lexbor_erl_port'.
%%
%% The port executable path is resolved in the following order:
%% <ol>
%%   <li>Application environment: `{lexbor_erl, port_cmd}'</li>
%%   <li>Application priv directory: `priv/lexbor_port'</li>
%%   <li>Relative to compiled beam: `../priv/lexbor_port'</li>
%% </ol>
%%
%% @returns `{ok, Pid}' on success, `{error, Reason}' on failure
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Check if the port manager is alive and running.
%%
%% @returns `true' if the process is registered and alive, `false' otherwise
-spec alive() -> boolean().
alive() ->
    whereis(?SERVER) =/= undefined.

%% @doc Send a command to the C port and wait for a response.
%%
%% This is the main entry point for all operations. It sends a command
%% tag and payload to the C port, waits for the response, and returns
%% the result.
%%
%% The call will block until:
%% <ul>
%%   <li>A response is received from the port</li>
%%   <li>The port exits (returns `{error, {port_exit, Reason}}')</li>
%%   <li>Timeout is reached (configured via `op_timeout_ms', default 3000ms)</li>
%% </ul>
%%
%% == Command Tags ==
%%
%% Valid command tags include:
%% <ul>
%%   <li>`&lt;&lt;"PARSE_SERIALIZE"&gt;&gt;' - Parse and serialize HTML</li>
%%   <li>`&lt;&lt;"SELECT_HTML"&gt;&gt;' - Parse and select elements</li>
%%   <li>`&lt;&lt;"PARSE_DOC"&gt;&gt;' - Parse and store document</li>
%%   <li>`&lt;&lt;"RELEASE_DOC"&gt;&gt;' - Release stored document</li>
%%   <li>`&lt;&lt;"SELECT_NODES"&gt;&gt;' - Select nodes from document</li>
%%   <li>`&lt;&lt;"OUTER_HTML"&gt;&gt;' - Get outer HTML of node</li>
%% </ul>
%%
%% @param CmdTag Binary command identifier (will be padded to 16 bytes)
%% @param Payload Binary payload for the command
%% @returns `{ok, Binary}' with response data, or `{error, Reason}'
-spec call(binary(), binary()) -> {ok, binary()} | {error, term()}.
call(CmdTag, Payload) ->
    gen_server:call(?SERVER, {call, CmdTag, Payload}, infinity).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

%% @private
%% @doc Initialize the gen_server and open the C port.
%%
%% Resolves the path to the `lexbor_port' executable and opens it as
%% an OS port with `{packet, 4}' framing. If the executable cannot be
%% found, returns `{stop, {port_not_found, Path}}'.
init([]) ->
    % Get the port command path, preferring configured value
    % but defaulting to finding it in the application's priv directory
    PortCmd = case application:get_env(lexbor_erl, port_cmd) of
        {ok, Cmd} ->
            Cmd;
        undefined ->
            % Find the priv directory for this application
            case code:priv_dir(lexbor_erl) of
                {error, bad_name} ->
                    % Fallback for development/test scenarios
                    % Try to find it relative to the compiled beam file
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
            {ok, #state{port=Port}}
    end.

%% @private
%% @doc Handle synchronous calls to send commands to the port.
%%
%% Formats the command as a binary frame (version + tag + payload),
%% sends it to the port, and waits for a response. Returns an error
%% if the port exits or if the timeout is reached.
handle_call({call, CmdTag, Payload}, _From, #state{port=Port}=S) ->
    %% Wire format (very simple):
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

%% @private
%% @doc Handle asynchronous cast messages (currently unused).
handle_cast(_Msg, S) ->
    {noreply, S}.

%% @private
%% @doc Handle port exit messages.
%%
%% If the C port process exits for any reason, this logs an error
%% and stops the gen_server, which will trigger a restart by the
%% supervisor.
handle_info({'EXIT', Port, Status}, #state{port=Port}=S) ->
    error_logger:error_msg("lexbor_erl port exited: ~p~n", [Status]),
    {stop, port_died, S};
handle_info(_Msg, S) ->
    {noreply, S}.

%% @private
%% @doc Clean up resources when the gen_server terminates.
%%
%% Closes the port to ensure the C process is properly terminated.
terminate(_Reason, #state{port=Port}) ->
    catch port_close(Port), ok.

%% @private
%% @doc Handle code upgrades (no state changes needed).
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ===================================================================
%% Internal helpers
%% ===================================================================

%% @private
%% @doc Pad or truncate a binary to exactly 16 bytes.
%%
%% Used to normalize command tags to the expected wire format size.
%% If the input is shorter than 16 bytes, it's padded with zeros.
%% If longer, it's truncated.
pad16(Bin) when is_binary(Bin) ->
    <<(binary:part(Bin, 0, min(byte_size(Bin),16)))/binary,
      (pad_zeros(16 - min(byte_size(Bin),16)))/binary>>.

%% @private
%% @doc Generate a binary of N zero bytes.
pad_zeros(N) when N =< 0 -> <<>>;
pad_zeros(N) ->
    <<0:8, (pad_zeros(N-1))/binary>>.

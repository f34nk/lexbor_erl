%% @doc OTP application behavior for lexbor_erl.
%%
%% Starts the supervisor tree with a configured pool size. The pool size
%% defaults to the number of scheduler threads if not configured.
%%
%% Configuration example:
%% ```
%% {lexbor_erl, [
%%   {pool_size, 8},           % Number of workers (default: schedulers_online)
%%   {op_timeout_ms, 3000}     % Operation timeout (default: 3000ms)
%% ]}
%% '''
%%
%% @end
-module(lexbor_erl_app).

-behaviour(application).

-export([start/2, stop/1]).

%% @doc Start the application and supervisor tree.
%%
%% Reads the `pool_size' configuration or uses the number of scheduler
%% threads as the default.
%%
%% @param Type Application start type
%% @param Args Application start arguments
%% @returns `{ok, Pid}' with supervisor PID
start(_Type, _Args) ->
    % Get pool size from configuration, default to number of schedulers
    PoolSize =
        application:get_env(lexbor_erl, pool_size, erlang:system_info(schedulers_online)),
    lexbor_erl_sup:start_link(PoolSize).

%% @doc Stop the application.
%%
%% @param State Application state
%% @returns `ok'
stop(_State) ->
    ok.

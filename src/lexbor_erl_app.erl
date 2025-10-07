%% @doc OTP application behavior for lexbor_erl.
%%
%% Starts the port process that communicates with the Lexbor C library.
%% The port process is supervised and will be restarted if it crashes.
%%
%% Configuration example:
%% ```
%% {lexbor_erl, [
%%   {op_timeout_ms, 3000}     % Operation timeout (default: 3000ms)
%% ]}
%% '''
%%
%% @end
-module(lexbor_erl_app).
-behaviour(application).

-export([start/2, stop/1]).

%% @doc Start the application and port process.
%%
%% @param Type Application start type
%% @param Args Application start arguments
%% @returns `{ok, Pid}' with port manager PID
start(_Type, _Args) ->
    lexbor_erl_port:start_link().

%% @doc Stop the application.
%%
%% @param State Application state
%% @returns `ok'
stop(_State) ->
    ok.

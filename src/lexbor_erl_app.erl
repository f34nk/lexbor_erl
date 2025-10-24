-module(lexbor_erl_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    lexbor_erl_port:start_link().

stop(_State) ->
    ok.

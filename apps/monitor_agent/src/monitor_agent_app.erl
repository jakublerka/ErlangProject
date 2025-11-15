-module(monitor_agent_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    io:fwrite("Hello, World!\n").
    %monitor_agent_supervisor:start_link().

stop(_State) ->
    ok.
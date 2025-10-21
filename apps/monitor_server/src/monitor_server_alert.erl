-module(monitor_alert).
-export([start_link/0, check/2]).

start_link() ->
    register(?MODULE, spawn_link(fun loop/0)).

loop() ->
    receive
        {check, Node, Metrics} ->
            %% Simple example: check if CPU usage > 90
            case parse_cpu(Metrics) > 90 of
                true -> io:format("⚠️ ALERT: High CPU on ~p!~n", [Node]);
                false -> ok
            end,
            loop()
    end.

check(Node, Metrics) ->
    ?MODULE ! {check, Node, Metrics}.

parse_cpu(#{cpu := CPUString}) ->
    %% Dummy parser example; replace with real logic
    85;
parse_cpu(_) ->
    0.

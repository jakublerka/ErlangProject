-module(monitor_aggregator).
-export([start_link/0, add_metrics/2]).

start_link() ->
    register(?MODULE, spawn_link(fun loop/0)).

loop() ->
    receive
        {add, Node, Metrics} ->
            ets:insert(metrics, {Node, Metrics}),
            monitor_alert:check(Node, Metrics),
            loop()
    end.

add_metrics(Node, Metrics) ->
    ?MODULE ! {add, Node, Metrics}.

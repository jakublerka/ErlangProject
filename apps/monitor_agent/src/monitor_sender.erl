-module(monitor_sender).
-export([start_link/0, send_metrics/1]).

start_link() ->
    register(?MODULE, spawn_link(fun loop/0)).

loop() ->
    receive
        {send, Metrics} ->
            %% Send to the central node
            {monitor_receiver, 'monitor@central'} ! {metrics, node(), Metrics},
            loop()
    end.

send_metrics(Metrics) ->
    ?MODULE ! {send, Metrics}.

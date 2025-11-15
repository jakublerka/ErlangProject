-module(monitor_server_supervisor).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpecs = [
        {receiver, {monitor_receiver, start_link, []}, permanent, 5000, worker, [monitor_receiver]},
        {aggregator, {monitor_aggregator, start_link, []}, permanent, 5000, worker, [monitor_aggregator]},
        {alert, {monitor_alert, start_link, []}, permanent, 5000, worker, [monitor_alert]}
    ],
    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.

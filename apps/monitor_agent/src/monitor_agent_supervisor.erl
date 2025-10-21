-module(monitor_agent_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpecs = [
        {collector, {monitor_collector, start_link, []}, permanent, 5000, worker, [monitor_collector]},
        {sender, {monitor_sender, start_link, []}, permanent, 5000, worker, [monitor_sender]}
    ],
    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.

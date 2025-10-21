-module(monitor_collector).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_info/2]).

-record(state, {interval = 5000}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    timer:send_interval(5000, collect),
    {ok, #state{}}.

handle_info(collect, State) ->
    Metrics = get_metrics(),
    monitor_sender:send_metrics(Metrics),
    {noreply, State}.

get_metrics() ->
    CPU = os:cmd("grep 'cpu ' /proc/stat"),
    Mem = os:cmd("free -m | grep Mem"),
    #{cpu => CPU, mem => Mem, time => erlang:system_time(second)}.

-module(monitor_receiver).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_info/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("Receiver started on ~p~n", [node()]),
    {ok, #{}}.

handle_info({metrics, FromNode, Metrics}, State) ->
    io:format("Received metrics from ~p: ~p~n", [FromNode, Metrics]),
    monitor_aggregator:add_metrics(FromNode, Metrics),
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

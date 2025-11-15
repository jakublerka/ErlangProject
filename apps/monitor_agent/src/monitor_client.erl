%% ============================================================
%% monitor_client.erl
%% Collects metrics locally and sends them to monitor_server
%% ============================================================

-module(monitor_client).
-behaviour(gen_server).

-export([start_link/1]).

-export([
    init/1,
    handle_info/2,
    handle_cast/2,
    handle_call/3,
    terminate/2,
    code_change/3
]).

-record(state, {id}).

start_link(Id) ->
    gen_server:start_link(?MODULE, Id, []).

init(Id) ->
    %% Tick every 3 seconds
    timer:send_interval(3000, collect),
    io:format("monitor_client (~p) started~n", [Id]),
    {ok, #state{id=Id}}.

handle_info(collect, S=#state{id=Id}) ->
    M = collect_metrics(),
    gen_server:cast({global, monitor_server}, {metrics, Id, M}),
    {noreply, S};

handle_info(_, S) ->
    {noreply, S}.

handle_cast(_, S) ->
    {noreply, S}.

handle_call(_, _From, S) ->
    {reply, ok, S}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%% ============================================================
%% Dummy metrics (replace with real OS calls later)
%% ============================================================
collect_metrics() ->
    CPU = rand:uniform(100),     %% 1–100%
    RAM = rand:uniform(16000),   %% up to 16GB
    #{cpu => CPU, ram_mb => RAM}.

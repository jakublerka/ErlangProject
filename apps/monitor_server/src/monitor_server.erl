%% ============================================================
%% monitor_server.erl
%% Receives metrics from clients, checks thresholds, sends alerts
%% ============================================================

-module(monitor_server).
-behaviour(gen_server).

-export([start_link/0]).

-export([
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {table}).

start_link() ->
    gen_server:start_link({global, monitor_server}, ?MODULE, [], []).

init([]) ->
    T = ets:new(metrics, [set, public, named_table]),
    io:format("monitor_server started~n"),
    {ok, #state{table=T}}.


%% ============================================================
%% Receive metrics from clients
%% ============================================================
handle_cast({metrics, ClientId, M}, S=#state{table=T}) ->
    ets:insert(T, {ClientId, M}),
    check_alerts(ClientId, M),
    {noreply, S};

handle_cast(_, S) ->
    {noreply, S}.

handle_call(_Req, _From, S) ->
    {reply, ok, S}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.


%% ============================================================
%% Threshold Checking
%% ============================================================
check_alerts(ClientId, #{cpu := CPU, ram_mb := RAM}) ->
    if CPU > 90 -> alert({ClientId, high_cpu, CPU});
       true -> ok end,

    if RAM > 14000 -> alert({ClientId, high_ram, RAM});
       true -> ok end;

check_alerts(_, _) ->
    ok.


%% ============================================================
%% Safe alert dispatch
%% ============================================================
alert(AlertMsg) ->
    case global:whereis_name(alert_manager) of
        undefined ->
            io:format("*** WARNING: alert_manager not running; dropping alert: ~p~n", [AlertMsg]),
            ok;
        Pid ->
            gen_server:cast(Pid, {alert, AlertMsg}),
            ok
    end.

%% ============================================================
%% monitor_server.erl
%% Receives metrics from clients, checks thresholds, sends alerts
%% ============================================================

-module(monitor_server).
-behaviour(gen_server).

-export([start_link/0, get_metrics/0]).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {table}).

start_link() ->
    gen_server:start_link({via, global, monitor_server}, ?MODULE, [], []).

get_metrics() ->
    gen_server:call({via, global, monitor_server}, get_metrics).

init([]) ->
    Table = ets:new(metrics, [named_table, public, set]),
    io:format("monitor_server started~n"),
    {ok, #state{table = Table}}.

handle_cast({metrics, ClientId, Metrics}, S = #state{table = T}) ->
    ets:insert(T, {ClientId, Metrics}),
    check_alerts(ClientId, Metrics),
    check_criticals(ClientId, Metrics),
    {noreply, S};

handle_cast(_Other, S) ->
    {noreply, S}.

handle_call(get_metrics, _From, S = #state{table = T}) ->
    {reply, ets:tab2list(T), S};
handle_call(_R, _From, S) ->
    {reply, ok, S}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ========= Threshold checking =========
check_alerts(ClientId, #{timestamp := TS, cpu := CPU, ram_percent := RAM}) ->
    if CPU > 85, CPU < 95 -> alert({TS, ClientId, "wysokie zuzycie procesora", CPU}); true -> ok end,
    if RAM > 85, RAM < 95 -> alert({TS, ClientId, "wysokie zuzycie pamieci", RAM}); true -> ok end;
check_alerts(_, _) ->
    ok.

check_criticals(ClientId, #{timestamp := TS, cpu := CPU, ram_percent := RAM}) ->
    if CPU > 95 -> critical({TS, ClientId, "krytyczne zuzycie procesora", CPU}); true -> ok end,
    if RAM > 95-> critical({TS, ClientId, "krytyczne zuzycie pamieci", RAM}); true -> ok end;
check_criticals(_, _) ->
    ok.


%% ========= Safe alert dispatch =========
alert(AlertMsg) ->
    case global:whereis_name(alert_manager) of
        undefined ->
            io:format("*** WARNING: alert_manager not running; dropping alert: ~p~n", [AlertMsg]),
            ok;
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, {alert, AlertMsg}),
            ok
    end.

critical(CriticalMsg) ->
    case global:whereis_name(alert_manager) of
        undefined ->
            io:format("*** WARNING: alert_manager not running; dropping alert: ~p~n", [CriticalMsg]),
            ok;
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, {critical, CriticalMsg}),
            ok
    end.


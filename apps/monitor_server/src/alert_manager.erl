%% ============================================================
%% alert_manager.erl
%% Receives alerts from monitor_server, prints them on the console and logs them to a file
%% ============================================================

-module(alert_manager).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({via, global, alert_manager}, ?MODULE, [], []).

init([]) ->
    io:format("alert_manager started~n"),
    {ok, #state{}}.

handle_cast({alert, {TS, ClientId, Type, Value}}, State) ->
    Msg = io_lib:format("ALERT: ~p ~p ~p ~p~n", [TS, ClientId, Type, Value]),
    io:format("~s", [Msg]),
    _ = file:write_file("alerts.log", list_to_binary(Msg), [append]),
    {noreply, State};

handle_cast({critical, {TS, ClientId, Type, Value}}, State) ->
    Msg = io_lib:format("CRITICAL: ~p ~p ~p ~p~n", [TS, ClientId, Type, Value]),
    io:format("~s", [Msg]),
    _ = file:write_file("alerts.log", list_to_binary(Msg), [append]),
    {noreply, State};

handle_cast(_Other, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

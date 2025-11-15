%% ============================================================
%% alert_manager.erl
%% Receives alerts from monitor_server, prints them on the console and logs them to a file
%% ============================================================

-module(alert_manager).
-behaviour(gen_server).

-export([start_link/0]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {}).

start_link() ->
    gen_server:start_link({global, alert_manager}, ?MODULE, [], []).

init([]) ->
    io:format("alert_manager started~n"),
    {ok, #state{}}.

handle_cast({alert, {ClientId, Type, Value}}, State) ->
    Msg = io_lib:format("ALERT: ~p ~p ~p~n", [ClientId, Type, Value]),
    io:format("~s", [Msg]),
    file:write_file("alerts.log", list_to_binary(Msg), [append]),
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

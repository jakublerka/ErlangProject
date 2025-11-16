%% ============================================================
%% monitor_client.erl
%% Collects metrics locally and sends them to monitor_server
%% ============================================================

-module(monitor_client).
-behaviour(gen_server).

-export([start_link/1]).

-export([
    init/1, handle_info/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3
]).

-record(state, {id}).

start_link(Id) ->
    Servernode = 'projektpc01@192.168.1.152',
    % IdStr =
    %     case Id of
    %         Atom when is_atom(Atom) -> atom_to_list(Atom);
    %         Str when is_list(Str) -> Str;
    %         _ -> io_lib:format("~p", [Id])
    %     end,
    % RegName = ("monitor_client_" ++ IdStr),
    gen_server:start_link({local, Id}, ?MODULE, [Id], []),
    net_kernel:connect_node(Servernode).

% get_state() ->
%     case find_local_client() of
%         undefined -> {error, no_client_running};
%         Name -> gen_server:call(Name, get_state)
%     end.

% find_local_client() ->
%     lists:foldl(
%       fun(Name, Acc) ->
%               case string:prefix(atom_to_list(Name), "monitor_client_") of
%                   true  -> Name;
%                   false -> Acc
%               end
%       end,
%       undefined,
%       registered()
%     ).


init([IdStr]) ->
    io:format("monitor_client (~p) started~n", [IdStr]),
    %ServerNode = 'projektpc01@192.168.1.152',
    % case net_kernel:connect_node(ServerNode) of
    %     true ->
    %         io:format("Node connected to server ~p~n", [ServerNode]),
    %         {ok, connected};
    %     false ->
    %         io:format("Failed to connect to server ~p~n", [ServerNode]),
    %         {error, failed};
    %     ignored ->
    %         io:format("Connection ignored for server ~p (name mismatch)~n", [ServerNode]),
    %         {error, ignored}
    % end, % Ensure connectivity to monitor_server host
    timer:send_interval(timer:seconds(60), collect),
    {ok, #state{id = IdStr}}.

handle_info(collect, State = #state{id = Id}) ->
    Metrics = collect_metrics(),
    gen_server:cast({via, global, monitor_server},
                    {metrics, Id, Metrics}),
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.

%% -------- Metric collection --------
timestamp() ->
    {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
    lists:flatten(
        io_lib:format("~2..0B.~2..0B.~4..0B ~2..0B:~2..0B:~2..0B",
                      [D, M, Y, H, Min, S])
    ).

get_cpu() ->
    Cmd = "powershell -NoProfile -NonInteractive -command \"(Get-CimInstance Win32_Processor | "
          "Measure-Object -Property LoadPercentage -Average).Average\"",
    Output = os:cmd(Cmd),
    to_int(Output).

%% Returns RAM used in MB
get_ram_percent() ->
    %% PowerShell: compute RAM usage percentage
    Cmd = "powershell -NoProfile -NonInteractive -command \""
          "$os = Get-CimInstance Win32_OperatingSystem; "
          "($os.TotalVisibleMemorySize - $os.FreePhysicalMemory) * 100 / "
          "$os.TotalVisibleMemorySize\"",
    Output = os:cmd(Cmd),
    to_int(Output).

to_int(Str) ->
    case string:to_integer(string:trim(Str)) of
        {Int, _} -> Int;
        error -> 0
    end.

collect_metrics() ->
    TS = timestamp(),
    CPU = get_cpu(),
    RAM = get_ram_percent(),
    #{timestamp => TS, cpu => CPU, ram_percent => RAM}.
-module(zane_log).
-behaviour(gen_server).

-export([start_link/0]).
-export([log/2, log/3, trace/2, trace/3]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SRV, ?MODULE).


start_link() -> gen_server:start_link({local, ?SRV}, ?MODULE, [], []).

log(Mod, Msg) -> log(Mod, Msg, []).

log(Mod, Fmt, Args) ->
    gen_server:cast(?SRV, {log, Mod, Fmt, Args}).

trace(Mod, Msg) -> trace(Mod, Msg, []).

trace(Mod, Fmt, Args) ->
    gen_server:cast(?SRV, {trace, Mod, Fmt, Args}).


%% Behaviour: gen_server
%% ----------------------------------------------------------------------------

init([]) -> {ok, nil}.


handle_call(Msg, From, State) ->
    zane_log:log(?MODULE, "Ignoring unknown message from ~p: ~p", [From, Msg]),
    {noreply, State}.

handle_cast({log, Mod, Fmt, Args}, State) ->
    {Date, Time} = erlang:localtime(),
    Datetime = tuple_to_list(Date) ++ tuple_to_list(Time),
    Timestamp = io_lib:format("~p-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", Datetime),
    Msg = io_lib:format(Fmt, Args),
    io:format("~s [~p] ~s~n", [Timestamp, Mod, Msg]),
    {noreply, State};
handle_cast({trace, _Mod, Fmt, Args}, State) ->
    Msg = io_lib:format(Fmt, Args),
    io:format("~s~n", [Msg]),
    {noreply, State};
handle_cast(Msg, State) ->
    zane_log:log(?MODULE, "Ignoring unknown message: ~p", [Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    zane_log:log(?MODULE, "Ignoring unknown message: ~p", [Msg]),
    {noreply, State}.

terminate(Reason, _State) ->
    zane_log:log(?MODULE, "Terminating (~p)", [Reason]),
    ok.

code_change(OldVsn, State, _Extra) ->
    zane_log:log(?MODULE, "Performing code upgrade from ~p", [OldVsn]),
    {ok, State}.

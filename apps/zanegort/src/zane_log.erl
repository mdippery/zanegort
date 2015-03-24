-module(zane_log).
-export([log/2, log/3, trace/2, trace/3]).


log(Mod, Msg) -> log(Mod, Msg, []).

log(Mod, Fmt, Args) ->
    {Date, Time} = erlang:localtime(),
    Datetime = tuple_to_list(Date) ++ tuple_to_list(Time),
    Timestamp = io_lib:format("~p-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", Datetime),
    Msg = io_lib:format(Fmt, Args),
    io:format("~s [~p] ~s~n", [Timestamp, Mod, Msg]).

trace(Mod, Msg) -> trace(Mod, Msg, []).

trace(_Mod, Fmt, Args) ->
    Msg = io_lib:format(Fmt, Args),
    io:format("~s~n", [Msg]).

-module(zane_log).
-export([log/3, trace/3]).


log(Mod, Msg, Args) ->
    {{Year,Month,Day}, {Hour,Minute,Second}} = erlang:localtime(),
    io:format("~p-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B ",
              [Year, Month, Day, Hour, Minute, Second]),
    io:format("~p: ", [Mod]),
    io:format(Msg, Args),
    io:format("~n").



trace(_Mod, Msg, Args) ->
    io:format(Msg, Args),
    io:format("~n").

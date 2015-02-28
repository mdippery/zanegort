-module(zane_log).
-export([debug/3, info/3, warning/3, critical/3, trace/3]).


debug(Mod, Msg, Args) -> log("DEBUG", Mod, Msg, Args).


info(Mod, Msg, Args) -> log("INFO", Mod, Msg, Args).


warning(Mod, Msg, Args) -> log("WARNING", Mod, Msg, Args).


critical(Mod, Msg, Args) -> log("CRITICAL", Mod, Msg, Args).


trace(_Mod, Msg, Args) ->
    io:format(Msg, Args),
    io:format("~n").


log(Label, Mod, Msg, Args) ->
    {{Year,Month,Day}, {Hour,Minute,Second}} = erlang:localtime(),
    io:format("~p-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B ",
              [Year, Month, Day, Hour, Minute, Second]),
    io:format("[~s] ~p: ", [Label, Mod]),
    io:format(Msg, Args),
    io:format("~n").

-module(zane_db).
-export([insert/3, find/2]).


db_base_dir() -> zane_utils:getenv("ZANE_DB_DIR", "/tmp").


db_path(Type) -> filename:join(db_base_dir(), Type).


insert(Type, Nickname, Value) ->
    case find(Type, Nickname) of
        {ok, _} ->
            % TODO: Replace duplicates
            {error, duplicate};
        _ ->
            Path = db_path(Type),
            Line = string:join([Nickname, Value], ",") ++ "\n",
            file:write_file(Path, Line, [append])
    end.


find(Type, Nickname) ->
    Path = db_path(Type),
    case file:open(Path, [read]) of
        {ok, Device} -> find_in_device(Device, Nickname);
        {error, Reason} -> {error, Reason}
    end.


find_in_device(Device, Nickname) -> find_in_device(Device, Nickname, []).


find_in_device(Device, Nickname, Acc) ->
    case io:get_line(Device, "") of
        eof ->
            file:close(Device),
            find_in_lines(Nickname, Acc);
        Line ->
            find_in_device(Device, Nickname, Acc ++ [Line])
    end.


find_in_lines(_Nickname, []) -> nil;

find_in_lines(Nickname, [Head|Rest]) ->
    [Owner,Value|_] = string:tokens(Head, ","),
    case Owner of
        Nickname -> {ok, Value};
        _ -> find_in_lines(Nickname, Rest)
    end.

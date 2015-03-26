-module(zane_db).
-export([insert/3, find/2]).


db_base_dir() -> os_utils:getenv("ZANE_DB_DIR", "/tmp").

db_path(Type) -> filename:join(db_base_dir(), Type).

insert(Type, Nickname, Value) ->
    case find(Type, Nickname) of
        {ok, _} ->
            Lines = gather_lines(Type),
            case replace_line(Nickname, Value, Lines) of
                {found, NewLines} ->
                    case save_lines(Type, NewLines) of
                        ok ->
                            {ok, updated};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {not_found, _} ->
                    {error, not_found}
            end;
        _ ->
            Path = db_path(Type),
            Line = string:join([Nickname, Value], ",") ++ "\n",
            case file:write_file(Path, Line, [append]) of
                ok ->
                    {ok, inserted};
                {error, Reason} ->
                    {error, Reason}
            end
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

gather_lines(Type) ->
    Path = db_path(Type),
    case file:open(Path, [read]) of
        {ok, Device} -> gather_lines(Device, Type);
        {error, Reason} -> {error, Reason}
    end.

gather_lines(Device, Type) -> gather_lines(Device, Type, []).

gather_lines(Device, Type, Acc) ->
    case io:get_line(Device, "") of
        eof ->
            file:close(Device),
            Acc;
        Line ->
            Clean = string:strip(Line, right, $\n),
            gather_lines(Device, Type, Acc ++ [Clean])
    end.

replace_line(Nickname, NewValue, Lines) -> replace_line(Nickname, NewValue, [], Lines).

replace_line(_Nickname, _NewValue, Left, []) -> {not_found, Left};
replace_line(Nickname, NewValue, Left, [Head|Rest]) ->
    [Nick,_Val|_] = string:tokens(Head, ","),
    case Nick of
        Nickname ->
            New = string:join([Nick, NewValue], ","),
            NewData = Left ++ [New] ++ Rest,
            {found, NewData};
        _ ->
            replace_line(Nickname, NewValue, Left ++ [Head], Rest)
    end.

save_lines(Type, Lines) ->
    AllLines = string:join(Lines, "\n") ++ "\n",
    Path = db_path(Type),
    file:write_file(Path, AllLines).

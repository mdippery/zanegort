-module(zane_bot).
-export([connect/4]).


-record(irc_client, {host, port=6667, nickname, channel}).


connect(Host, Port, Nickname, Channel) ->
    Client = #irc_client{host=Host, port=Port, nickname=Nickname, channel=Channel},
    {ok, Sock} = gen_tcp:connect(Host, Port, [{packet, line}]),
    irc_proto:nick(Sock, Nickname),
    irc_proto:user(Sock, Nickname),
    loop(Sock, Client).


loop(Sock, Client) ->
    receive
        {tcp, Sock, Data} ->
            io:format("[~w] Received line:~n~s~n", [Sock, Data]),
            process_line(Sock, Client, lists:map(fun zane_string:strip/1, string:tokens(Data, " :"))),
            loop(Sock, Client);
        quit ->
            irc_proto:quit(Sock, "User terminated connection"),
            gen_tcp:close(Sock),
            exit(stopped)
    end.


process_line(Sock, Client, [_,"376"|_]) ->
    irc_proto:join(Sock, Client#irc_client.channel);

process_line(Sock, _Client, ["PING"|Rest]) ->
    irc_proto:pong(Sock, Rest);

process_line(_Sock, _Client, [_,"PRIVMSG",_Channel|Args]) ->
    [MaybeCmd|Rest] = Args,
    case string:substr(MaybeCmd, 1, 1) of
        "!" -> zane_cmd:handle(MaybeCmd, Rest);
        _ -> ok
    end;

process_line(_Sock, _Client, _Line) -> ok.

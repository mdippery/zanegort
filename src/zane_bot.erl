-module(zane_bot).
-export([connect/4]).


-record(irc_client, {host, port=6667, nickname, channel}).


connect(Host, Port, Nickname, Channel) ->
    Client = #irc_client{host=Host, port=Port, nickname=Nickname, channel=Channel},
    {ok, Sock} = gen_tcp:connect(Host, Port, [{packet, line}]),
    send_line(Sock, "NICK " ++ Nickname),
    send_line(Sock, "USER " ++ Nickname ++ " 0 * :" ++ Nickname),
    loop(Sock, Client).


loop(Sock, Client) ->
    receive
        {tcp, Sock, Data} ->
            io:format("[~w] Received line:~n~s~n", [Sock, Data]),
            process_line(Sock, Client, lists:map(fun zane_string:strip/1, string:tokens(Data, " :"))),
            loop(Sock, Client);
        quit ->
            send_line(Sock, "QUIT :User terminated connection"),
            gen_tcp:close(Sock),
            exit(stopped)
    end.


process_line(Sock, Client, [_,"376"|_]) ->
    send_line(Sock, "JOIN :" ++ Client#irc_client.channel);

process_line(Sock, _Client, ["PING"|Rest]) ->
    send_line(Sock, "PONG " ++ Rest);

process_line(_Sock, _Client, [_,"PRIVMSG",_Channel|Args]) ->
    [MaybeCmd|Rest] = Args,
    case string:substr(MaybeCmd, 1, 1) of
        "!" -> process_command(MaybeCmd, Rest);
        _ -> ok
    end;

process_line(_Sock, _Client, _Line) -> ok.


process_command(Cmd, Args) ->
    io:format("Processing command: [~s], [~s]", [Cmd, Args]),
    ok.


send_line(Sock, Line) ->
    io:format("[~w] Writing line:~n~s~n~n", [Sock, Line]),
    gen_tcp:send(Sock, Line ++ "\r\n").

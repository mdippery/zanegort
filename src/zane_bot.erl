-module(zane_bot).
-export([connect/4]).
-include("zane.hrl").


connect(Host, Port, Nickname, Channel) ->
    Client = #irc_client{host=Host, port=Port, nickname=Nickname, channel=Channel},
    {ok, Sock} = gen_tcp:connect(Host, Port, [{packet, line}]),
    irc_proto:nick(Sock, Nickname),
    irc_proto:user(Sock, Nickname),
    loop(Sock, Client).


loop(Sock, Client) ->
    receive
        {tcp, Sock, Data} ->
            io:format("Received line: <~p>~n", [Data]),
            Line = zane_string:strip(Data),
            process_line(Sock, Client, string:tokens(Line, " :")),
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

process_line(Sock, Client, [From,"PRIVMSG",_Channel|Args]) ->
    Nick = extract_nickname(From),
    [MaybeCmd|Rest] = Args,
    case string:substr(MaybeCmd, 1, 1) of
        "!" -> zane_cmd:handle(Sock, Client, Nick, MaybeCmd, Rest);
        _ -> ok
    end;

process_line(_Sock, _Client, _Line) -> ok.


extract_nickname(Username) ->
    [User|_] = string:tokens(Username, "@"),
    [From|_] = string:tokens(User, "!"),
    From.

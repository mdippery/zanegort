-module(irc_proto).
-export([pong/2, nick/2, user/2, join/2, quit/2]).


pong(Sock, Resp) -> send(Sock, "PONG " ++ Resp).


nick(Sock, Nickname) -> send(Sock, "NICK " ++ Nickname).


user(Sock, Username) -> send(Sock, "USER " ++ Username ++ " 0 * :" ++ Username).


join(Sock, Channel) -> send(Sock, "JOIN :" ++ Channel).


quit(Sock, Msg) -> send(Sock, "QUIT :" ++ Msg).


send(Sock, Line) ->
    io:format("Sending line: <~s>~n", [Line]),
    gen_tcp:send(Sock, Line ++ "\r\n").

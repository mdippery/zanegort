-module(irc_proto).
-export([
    pong/2,
    nick/2,
    user/2,
    join/2,
    quit/2,
    say/3,
    ctcp/4
]).


pong(Sock, Resp) -> send(Sock, "PONG " ++ Resp).


nick(Sock, Nickname) -> send(Sock, "NICK " ++ Nickname).


user(Sock, Username) -> send(Sock, "USER " ++ Username ++ " 0 * :" ++ Username).


join(Sock, Channel) -> send(Sock, "JOIN :" ++ Channel).


quit(Sock, Msg) -> send(Sock, "QUIT :" ++ Msg).


say(Sock, To, Msg) -> send(Sock, "PRIVMSG " ++ To ++ " :" ++ Msg).


ctcp(Sock, To, Action, Msg) when is_atom(Action) ->
    ctcp(Sock, To, string:to_upper(atom_to_list(Action)), Msg);
ctcp(Sock, To, Action, Msg) ->
    UAction = string:to_upper(Action),
    FullMsg = UAction ++ " " ++ Msg,
    notice(Sock, To, <<1,FullMsg,1>>).


notice(Sock, To, Msg) -> send(Sock, "NOTICE " ++ To ++ " :" ++ Msg).


send(Sock, Line) -> gen_tcp:send(Sock, Line ++ "\r\n").

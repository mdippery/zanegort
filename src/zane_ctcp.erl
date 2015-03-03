-module(zane_ctcp).
-include("zane.hrl").

-export([handle/3, extract_ctcp/3]).


-define(VERSION, "zanegort v1.0").
-define(SOURCE, "https://github.com/mdippery/zanegort").
-define(CTCP_CMDS, ["FINGER", "VERSION", "SOURCE", "USERINFO", "CLIENTINFO",
                    "ERRMSG", "PING", "TIME"]).


handle(Sock, From, "VERSION") ->
    irc_proto:ctcp(Sock, From, ?VERSION);

handle(Sock, From, "SOURCE") ->
    irc_proto:ctcp(Sock, From, ?SOURCE);

handle(_Sock, From, Msg) ->
    zane_log:log(?MODULE, "Unhandled CTCP command from ~p: ~p. Ignoring.", [From, Msg]),
    ok.


extract_ctcp(Client, To, MaybeCtcp) ->
    Clean = zane_string:remove_001(MaybeCtcp),
    case is_ctcp(Client, To, Clean) of
        true -> Clean;
        false -> nil
    end.


is_ctcp(#irc_client{nickname=Nickname}, Nickname, Cmd) -> lists:member(Cmd, ?CTCP_CMDS);
is_ctcp(_Client, _Nickname, _Cmd) -> false.

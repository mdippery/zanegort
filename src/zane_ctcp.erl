-module(zane_ctcp).
-include("zane.hrl").

-export([handle/5]).


-define(VERSION, "zanegort v1.0").
-define(SOURCE, "https://github.com/mdippery/zanegort").
-define(CTCP_CMDS, ["FINGER", "VERSION", "SOURCE", "USERINFO", "CLIENTINFO",
                    "ERRMSG", "PING", "TIME"]).


handle(Sock, Client, From, To, Args) ->
    zane_log:log(?MODULE, "handle From: ~p To: ~p Args: ~p", [From, To, Args]),
    ok.

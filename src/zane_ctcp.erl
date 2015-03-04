-module(zane_ctcp).
-include("zane.hrl").

-export([handle/5]).


-define(VERSION, "zanegort v1.0").
-define(SOURCE, "https://github.com/mdippery/zanegort").


handle(Sock, #irc_client{nickname=Nickname}, From, Nickname, DirtyArgs) ->
    Args = lists:map(fun zane_string:remove_001/1, DirtyArgs),
    dispatch(Sock, From, Args);

handle(_Sock, _Client, _From, _To, _Args) ->
    nil.


dispatch(Sock, From, ["VERSION"]) ->
    irc_proto:ctcp(Sock, From, ?VERSION);

dispatch(Sock, From, ["SOURCE"]) ->
    irc_proto:ctcp(Sock, From, ?SOURCE);

dispatch(_Sock, From, Args) ->
    zane_log:log(?MODULE, "Unrecognized CTCP from ~p: ~p", [From, Args]),
    nil.

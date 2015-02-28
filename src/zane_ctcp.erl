-module(zane_ctcp).
-export([handle/3]).


-define(VERSION, "zanegort v1.0").
-define(SOURCE, "https://github.com/mdippery/zanegort").


handle(Sock, From, "VERSION") ->
    irc_proto:ctcp(Sock, From, ?VERSION);

handle(Sock, From, "SOURCE") ->
    irc_proto:ctcp(Sock, From, ?SOURCE);

handle(_Sock, From, Msg) ->
    zane_log:log(?MODULE, "Unhandled CTCP command from ~p: ~p. Ignoring.", [From, Msg]),
    ok.

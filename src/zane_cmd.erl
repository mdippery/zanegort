-module(zane_cmd).
-export([handle/5]).
-include("zane.hrl").

-define(SOURCE_URL, "https://github.com/mdippery/zanegort").
-define(HELP_URL, ?SOURCE_URL).


handle(_Sock, _Client, Nick, "!set", [Key,Value|_Rest]) ->
    put_property(Key, Nick, Value);

handle(Sock, #irc_client{channel=Channel}, _Nick, "!source", _Args) ->
    Msg = "Source is available at " ++ ?SOURCE_URL,
    irc_proto:say(Sock, Channel, Msg);

handle(_Sock, _Client, _Nick, "!web", [Nickname|_Rest]) ->
    get_property(Nickname, "web");

handle(_Sock, _Client, _Nick, "!github", [Nickname|_Rest]) ->
    get_property(Nickname, "github");

handle(_Sock, _Client, _Nick, "!stack", [Nickname|_Rest]) ->
    get_property(Nickname, "stack");

handle(Sock, #irc_client{channel=Channel}, _Nick, "!help", _Args) ->
    Msg = "Command help is available at " ++ ?HELP_URL,
    irc_proto:say(Sock, Channel, Msg);

handle(_Sock, _Client, _Nick, Other, Args) ->
    io:format("Invalid cmd: ~p ~p. Ignoring.~n", [Other, Args]).


get_property(Nickname, Key) ->
    io:format("Retrieving ~p key for ~p~n", [Key, Nickname]).


put_property("web", Nickname, Url) ->
    io:format("Storing website for ~p: ~p~n", [Nickname, Url]);

put_property("github", Nickname, Username) ->
    io:format("Storing GitHub for ~p: ~p~n", [Nickname, Username]);

put_property("stack", Nickname, Uid) ->
    io:format("Storing Stack Overflow ID for ~p: ~p~n", [Nickname, Uid]);

put_property(Key, Nickname, _Val) ->
    io:format("Invalid key for ~p: ~p~n", [Key, Nickname]).

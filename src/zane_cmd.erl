-module(zane_cmd).
-export([handle/4]).
-include("zane.hrl").

-define(SOURCE_URL, "https://github.com/mdippery/zanegort").
-define(HELP_URL, ?SOURCE_URL).


handle(_Sock, _Client, "!set", Args) ->
    io:format("Handling 'set ~p'~n", [Args]);

handle(Sock, #irc_client{channel=Channel}, "!source", _Args) ->
    Msg = "Source is available at " ++ ?SOURCE_URL,
    irc_proto:say(Sock, Channel, Msg);

handle(_Sock, _Client, "!web", [Url|_Rest]) ->
    io:format("Returning website for ~p~n", [Url]);

handle(_Sock, _Client, "!github", [Username|_Rest]) ->
    io:format("GitHub: https://github.com/~p~n", [Username]);

handle(_Sock, _Client, "!stack", [Username|_Rest]) ->
    io:format("Stack Overflow: http://stackoverflow.com/users/~p~n", [Username]);

handle(Sock, #irc_client{channel=Channel}, "!help", _Args) ->
    Msg = "Command help is available at " ++ ?HELP_URL,
    irc_proto:say(Sock, Channel, Msg);

handle(_Sock, _Client, Other, Args) ->
    io:format("Invalid cmd: ~p ~p. Ignoring.~n", [Other, Args]).

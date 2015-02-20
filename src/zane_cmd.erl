-module(zane_cmd).
-export([handle/5]).
-include("zane.hrl").

-define(SOURCE_URL, "https://github.com/mdippery/zanegort").
-define(HELP_URL, ?SOURCE_URL).


handle(Sock, #irc_client{channel=Channel}, Nick, "set", [Key,Value|Rest]) ->
    % Due to the way lines are parsed, URLs beginning with "http://"
    % will be chopped in two, so reassemble before storing.
    Val = string:join([Value|Rest], ":"),
    put_property(Sock, Channel, Key, Nick, Val);

handle(Sock, #irc_client{channel=Channel}, _Nick, "source", _Args) ->
    Msg = "Source is available at " ++ ?SOURCE_URL,
    irc_proto:say(Sock, Channel, Msg);

handle(Sock, #irc_client{channel=Channel}, _Nick, "web", [Nickname|_Rest]) ->
    Prefix = "",
    Noun = "website",
    get_property_or_error(Sock, Channel, Nickname, "web", Prefix, Noun);

handle(Sock, #irc_client{channel=Channel}, _Nick, "github", [Nickname|_Rest]) ->
    Prefix = "https://github.com/",
    Noun = "GitHub profile",
    get_property_or_error(Sock, Channel, Nickname, "github", Prefix, Noun);

handle(Sock, #irc_client{channel=Channel}, _Nick, "stack", [Nickname|_Rest]) ->
    Prefix = "http://stackoverflow.com/users/",
    Noun = "Stack Overflow profile",
    get_property_or_error(Sock, Channel, Nickname, "stack", Prefix, Noun);

handle(Sock, #irc_client{channel=Channel}, _Nick, "!help", _Args) ->
    Msg = "Command help is available at " ++ ?HELP_URL,
    irc_proto:say(Sock, Channel, Msg);

handle(_Sock, _Client, _Nick, Other, Args) ->
    io:format("Invalid cmd: ~p ~p. Ignoring.~n", [Other, Args]).


get_property_or_error(Sock, Channel, Nickname, Key, Prefix, Noun) ->
    case zane_db:find(Key, Nickname) of
        {ok, Value} ->
            Url = Prefix ++ Value,
            irc_proto:say(Sock, Channel, Nickname ++ "'s " ++ Noun ++ " is " ++ Url),
            ok;
        nil ->
            irc_proto:say(Sock, Channel, Nickname ++ " has not set their " ++ Noun ++ " yet"),
            ok;
        {error, Reason} ->
            io:format("Error getting ~p for ~p: ~p~n", [Key, Nickname, Reason]),
            error
    end.


put_property(Sock, Channel, Type, Nickname, Value) ->
    case lists:member(Type, ["web", "github", "stack"]) of
        true ->
            case zane_db:insert(Type, Nickname, Value) of
                ok ->
                    irc_proto:say(Sock, Channel, "Set " ++ Type ++ " for " ++ Nickname),
                    ok;
                {error, Reason} ->
                    io:format("Error saving ~p for ~p: ~p~n", [Type, Nickname, Reason]),
                    error
            end;
        false ->
            io:format("Invalid key for ~p: ~p. Ignoring.~n", [Nickname, Type]),
            error
    end.

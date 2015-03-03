-module(zane_cmd).
-include("zane.hrl").

-export([handle/5, extract_command/1]).

-define(CMD_PREFIX, os_utils:getenv("ZANE_CMD_PREFIX", ?DEFAULT_CMD_PREFIX)).
-define(HELP_URL, "https://github.com/mdippery/zanegort").


handle(Sock, #irc_client{channel=Channel}, Nick, "set", [Key,Value|Rest]) ->
    % Due to the way lines are parsed, URLs beginning with "http://"
    % will be chopped in two, so reassemble before storing.
    Val = string:join([Value|Rest], ":"),
    put_property(Sock, Channel, Key, Nick, Val);

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

handle(Sock, #irc_client{channel=Channel}, _Nick, "reddit", [Nickname|_Rest]) ->
    Prefix = "http://reddit.com/user/",
    Noun = "Reddit profile",
    get_property_or_error(Sock, Channel, Nickname, "reddit", Prefix, Noun);

handle(Sock, #irc_client{channel=Channel}, _Nick, "help", _Args) ->
    Msg = "Command help is available at " ++ ?HELP_URL,
    irc_proto:say(Sock, Channel, Msg);

handle(_Sock, _Client, _Nick, Other, Args) ->
    zane_log:log(?MODULE, "Invalid cmd: ~p ~p. Ignoring.", [Other, Args]).


extract_command(RawCmd) -> extract_command(RawCmd, ?CMD_PREFIX).


extract_command(RawCmd, CmdPrefix) ->
    case is_command(RawCmd, CmdPrefix) of
        true -> string:substr(RawCmd, string:len(CmdPrefix)+1);
        false -> nil
    end.


is_command(MaybeCmd, CmdPrefix) ->
    case string:substr(MaybeCmd, 1, string:len(CmdPrefix)) of
        CmdPrefix -> true;
        _ -> false
    end.


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
            zane_log:log(?MODULE, "Error getting ~p for ~p: ~p", [Key, Nickname, Reason]),
            error
    end.


put_property(Sock, Channel, Type, Nickname, Value) ->
    case lists:member(Type, ["web", "github", "stack", "reddit"]) of
        true ->
            case zane_db:insert(Type, Nickname, Value) of
                ok ->
                    irc_proto:say(Sock, Channel, "Set " ++ Type ++ " for " ++ Nickname),
                    ok;
                {error, Reason} ->
                    zane_log:log(?MODULE, "Error saving ~p for ~p: ~p", [Type, Nickname, Reason]),
                    error
            end;
        false ->
            zane_log:log(?MODULE, "Invalid key for ~p: ~p. Ignoring.", [Nickname, Type]),
            error
    end.

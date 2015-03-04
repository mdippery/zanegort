-module(zane_cmd).
-include("zane.hrl").

-export([handle/5]).
-export([normalize_listener/1]).

-define(HELP_URL, "https://github.com/mdippery/zanegort/wiki/Help").


handle(Sock, #irc_client{channel=Channel, nickname=Nickname}, From, Channel, [Listener|Args]) ->
    case normalize_listener(Listener) of
        Nickname ->
            dispatch(Sock, Channel, From, Args);
        _ ->
            nil
    end;

handle(_Sock, _Client, _From, _To, _Args) ->
    nil.


dispatch(Sock, To, _From, ["web","for",Nickname]) ->
    get_property_or_error(Sock, To, Nickname, "web", "", "website");

dispatch(Sock, To, _From, ["github","for",Nickname]) ->
    Prefix = "https://github.com/",
    Noun = "GitHub profile",
    get_property_or_error(Sock, To, Nickname, "github", Prefix, Noun);

dispatch(Sock, To, _From, ["stack","for",Nickname]) ->
    Prefix = "http://stackoverflow.com/users/",
    Noun = "Stack Overflow profile",
    get_property_or_error(Sock, To, Nickname, "stack", Prefix, Noun);

dispatch(Sock, To, _From, ["reddit","for",Nickname]) ->
    Prefix = "http://reddit.com/user/",
    Noun = "Reddit profile",
    get_property_or_error(Sock, To, Nickname, "reddit", Prefix, Noun);

dispatch(Sock, To, From, ["set","web","to",Url]) ->
    put_property(Sock, To, "web", From, Url);

dispatch(Sock, To, From, ["set","github","to",Username]) ->
    put_property(Sock, To, "github", From, Username);

dispatch(Sock, To, From, ["set","stack","to",UserId]) ->
    put_property(Sock, To, "stack", From, UserId);

dispatch(Sock, To, From, ["set","reddit","to",Username]) ->
    put_property(Sock, To, "reddit", From, Username);

dispatch(Sock, To, _From, ["help"]) ->
    Msg = "Command help is available at " ++ ?HELP_URL,
    irc_proto:say(Sock, To, Msg);

dispatch(Sock, To, _From, _Args) ->
    irc_proto:say(Sock, To, "no thanks!").


normalize_listener("zane") ->
    "zanegort";
normalize_listener("zane,") ->
    "zanegort";
normalize_listener(Nickname) ->
    L = string:len(Nickname),
    case string:rchr(Nickname, $,) of
        L -> string:substr(Nickname, 1, L-1);
        _ -> Nickname
    end.


get_property_or_error(Sock, Channel, Nickname, Key, Prefix, Noun) ->
    case zane_db:find(Key, Nickname) of
        {ok, Value} ->
            Url = Prefix ++ Value,
            irc_proto:say(Sock, Channel, Nickname ++ "'s " ++ Noun ++ " is " ++ Url),
            ok;
        nil ->
            irc_proto:say(Sock, Channel, "i don't know!"),
            ok;
        {error, Reason} ->
            zane_log:log(?MODULE, "Error getting ~p for ~p: ~p", [Key, Nickname, Reason]),
            irc_proto:say(Sock, Channel, "i don't know!"),
            error
    end.


put_property(Sock, Channel, Type, Nickname, Value) ->
    case zane_db:insert(Type, Nickname, Value) of
        ok ->
            irc_proto:say(Sock, Channel, "Set " ++ Type ++ " for " ++ Nickname ++ " to " ++ Value),
            ok;
        {error, Reason} ->
            zane_log:log(?MODULE, "Error saving ~p for ~p: ~p", [Type, Nickname, Reason]),
            irc_proto:say("damn it"),
            error
    end.

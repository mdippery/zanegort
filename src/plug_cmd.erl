-module(plug_cmd).
-behaviour(gen_event).
-include("zanegort.hrl").

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).
-export([extract_url/1]).

-define(HELP_URL, "https://github.com/mdippery/zanegort/wiki/Help").


%% Behaviour: gen_event
%% ----------------------------------------------------------------------------

init(Client) ->
    zane_log:log(?MODULE, "Loading plugin"),
    {ok, Client}.

handle_event({privmsg, From, Channel, [Listener|Args]}, State=#irc_client{channel=Channel, nickname=Nickname}) ->
    case normalize_listener(Listener) of
        Nickname ->
            dispatch(Channel, From, Args),
            {ok, State};
        _ ->
            {ok, State}
    end;
handle_event({privmsg, _From, _Channel, _Args}, State) ->
    {ok, State};
handle_event(Msg, State) ->
    zane_log:log(?MODULE, "Ignoring unknown event: ~p", [Msg]),
    {ok, State}.

handle_call(Msg, State) ->
    zane_log:log(?MODULE, "Ignoring unknown message: ~p", [Msg]),
    {ok, ok, State}.

handle_info(Msg, State) ->
    zane_log:log(?MODULE, "Ignoring unknown message: ~p", [Msg]),
    {ok, State}.

terminate(Reason, _State) ->
    zane_log:log(?MODULE, "Terminating (~p)", [Reason]),
    ok.

code_change(OldVsn, State, _Extra) ->
    zane_log:log(?MODULE, "Performing code upgrade from ~p", [OldVsn]),
    {ok, State}.


%% Private implementation
%% ----------------------------------------------------------------------------

dispatch(To, _From, ["web","for",Nickname]) ->
    get_property_or_error(To, Nickname, "web", "", "website");
dispatch(To, _From, ["github","for",Nickname]) ->
    Prefix = "https://github.com/",
    Noun = "GitHub profile",
    get_property_or_error(To, Nickname, "github", Prefix, Noun);
dispatch(To, _From, ["stack","for",Nickname]) ->
    Prefix = "http://stackoverflow.com/users/",
    Noun = "Stack Overflow profile",
    get_property_or_error(To, Nickname, "stack", Prefix, Noun);
dispatch(To, _From, ["reddit","for",Nickname]) ->
    Prefix = "http://reddit.com/user/",
    Noun = "Reddit profile",
    get_property_or_error(To, Nickname, "reddit", Prefix, Noun);
dispatch(To, _From, ["twitter","for",Nickname]) ->
    Prefix = "https://twitter.com/",
    Noun = "Twitter profile",
    get_property_or_error(To, Nickname, "twitter", Prefix, Noun);
dispatch(To, From, ["set","web","to"|Args]) ->
    Url = extract_url(Args),
    put_property(To, "web", From, Url);
dispatch(To, From, ["set","github","to",Username]) ->
    put_property(To, "github", From, Username);
dispatch(To, From, ["set","stack","to",UserId]) ->
    put_property(To, "stack", From, UserId);
dispatch(To, From, ["set","reddit","to",Username]) ->
    put_property(To, "reddit", From, Username);
dispatch(To, From, ["set","twitter","to",Username]) ->
    put_property(To, "twitter", From, Username);
dispatch(To, _From, ["help"]) ->
    Msg = "Command help is available at " ++ ?HELP_URL,
    irc_proto:say(To, Msg);
dispatch(To, _From, _Args) ->
    irc_proto:me(To, "shrugs").

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

extract_url([Head|Rest]) ->
    case string:substr(Head, 1, 4) of
        "http" ->
            Path = string:join(Rest, ""),
            Head ++ ":" ++ Path;
        _ ->
            Head
    end.

get_property_or_error(Channel, Nickname, Key, Prefix, Noun) ->
    zane_log:log(?MODULE, "Getting ~p for ~p", [Key, Nickname]),
    case zane_db:find(Key, Nickname) of
        {ok, Value} ->
            Url = Prefix ++ Value,
            irc_proto:say(Channel, Nickname ++ "'s " ++ Noun ++ " is " ++ Url),
            ok;
        nil ->
            irc_proto:say(Channel, "i don't know!"),
            ok;
        {error, Reason} ->
            zane_log:log(?MODULE, "Error getting ~p for ~p: ~p", [Key, Nickname, Reason]),
            irc_proto:say(Channel, "i don't know!"),
            error
    end.

put_property(Channel, Type, Nickname, Value) ->
    zane_log:log(?MODULE, "Setting ~p to ~p for ~p", [Type, Value, Nickname]),
    case zane_db:insert(Type, Nickname, Value) of
        {ok, inserted} ->
            irc_proto:say(Channel, "Set " ++ Type ++ " for " ++ Nickname ++ " to " ++ Value),
            ok;
        {ok, updated} ->
            irc_proto:say(Channel, "Updated " ++ Type ++ " for " ++ Nickname ++ " to " ++ Value),
            ok;
        {error, Reason} ->
            zane_log:log(?MODULE, "Error saving ~p for ~p: ~p", [Type, Nickname, Reason]),
            irc_proto:say(Channel, "damn it"),
            error
    end.

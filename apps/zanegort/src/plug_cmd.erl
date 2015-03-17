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

-record(state, {client, sock}).

-define(HELP_URL, "https://github.com/mdippery/zanegort/wiki/Help").


%% Behaviour: gen_event
%% ----------------------------------------------------------------------------

init({Client, Sock}) ->
    {ok, #state{client=Client, sock=Sock}}.

handle_event({privmsg, From, Channel, [Listener|Args]}, State=#state{sock=Sock, client=#irc_client{channel=Channel, nickname=Nickname}}) ->
    case normalize_listener(Listener) of
        Nickname ->
            dispatch(Sock, Channel, From, Args),
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
dispatch(Sock, To, _From, ["twitter","for",Nickname]) ->
    Prefix = "https://twitter.com/",
    Noun = "Twitter profile",
    get_property_or_error(Sock, To, Nickname, "twitter", Prefix, Noun);
dispatch(Sock, To, From, ["set","web","to"|Args]) ->
    Url = extract_url(Args),
    put_property(Sock, To, "web", From, Url);
dispatch(Sock, To, From, ["set","github","to",Username]) ->
    put_property(Sock, To, "github", From, Username);
dispatch(Sock, To, From, ["set","stack","to",UserId]) ->
    put_property(Sock, To, "stack", From, UserId);
dispatch(Sock, To, From, ["set","reddit","to",Username]) ->
    put_property(Sock, To, "reddit", From, Username);
dispatch(Sock, To, From, ["set","twitter","to",Username]) ->
    put_property(Sock, To, "twitter", From, Username);
dispatch(Sock, To, _From, ["help"]) ->
    Msg = "Command help is available at " ++ ?HELP_URL,
    irc_proto:say(Sock, To, Msg);
dispatch(Sock, To, _From, _Args) ->
    irc_proto:me(Sock, To, "shrugs").

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
            string:strip(string:join(Rest, ""), left, $/);
        _ ->
            Head
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

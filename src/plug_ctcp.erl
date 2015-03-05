-module(plug_ctcp).
-include("zane.hrl").

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-record(state, {client, sock}).


%% Behaviour: gen_event
%% ----------------------------------------------------------------------------


init({Client, Sock}) ->
    {ok, #state{client=Client, sock=Sock}}.


handle_event({privmsg, From, Nickname, [Head|_]}, State=#state{sock=Sock, client=#irc_client{nickname=Nickname}}) ->
    Cmd = string:strip(Head, both, 1),
    case Cmd of
        "SOURCE" when length(Cmd) < length(Head) ->
            irc_proto:ctcp(Sock, From, source, ?SOURCE);
        "VERSION" when length(Cmd) < length(Head) ->
            irc_proto:ctcp(Sock, From, version, ?VERSION);
        _ ->
            zane_log:log(?MODULE, "Unrecognized CTCP command: ~p", [Cmd])
    end,
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

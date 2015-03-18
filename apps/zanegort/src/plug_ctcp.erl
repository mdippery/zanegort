-module(plug_ctcp).
-include("zanegort.hrl").

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


handle_event({privmsg, From, Nickname, [Cmd|_]}, State=#state{sock=Sock, client=#irc_client{nickname=Nickname}}) ->
    handle_ctcp(Sock, From, list_to_binary(Cmd)),
    {ok, State};
handle_event({privmsg, _From, Channel, _Args}, State=#state{client=#irc_client{channel=Channel}}) ->
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

handle_ctcp(Sock, From, <<1,"SOURCE",1>>) ->
    zane_log:log(?MODULE, "Responding to /ctcp SOURCE from ~p", [From]),
    irc_proto:ctcp(Sock, From, source, ?SOURCE);
handle_ctcp(Sock, From, <<1,"VERSION",1>>) ->
    zane_log:log(?MODULE, "Responding to /ctcp VERSION from ~p", [From]),
    Version = io_lib:format("zanegort v~s", [zanegort_app:vsn()]),
    irc_proto:ctcp(Sock, From, version, Version);
handle_ctcp(_Sock, _From, Arg) ->
    Cmd = string:strip(Arg, both, 1),
    zane_log:log(?MODULE, "Unrecognized CTCP command: ~p", [Cmd]).

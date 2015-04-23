-module(plug_webdev).
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


%% Behaviour: gen_event
%% ----------------------------------------------------------------------------

init(Client) ->
    zane_log:log(?MODULE, "Loading plugin"),
    {ok, Client}.


handle_event({privmsg, _From, Channel, ["!eval"|_]}, State=#irc_client{channel=Channel}) ->
    zane_log:log(?MODULE, "Responding to !eval"),
    timer:sleep(1500),
    irc_proto:say(Channel, "wrong! the answer is 42"),
    {ok, State};
handle_event({privmsg, _From, Channel, ["!boobs"|_]}, State=#irc_client{channel=Channel}) ->
    zane_log:log(?MODULE, "Responding to !boobs"),
    irc_proto:say(Channel, "(.)(.)"),
    {ok, State};
handle_event({privmsg, _From, Channel, ["!rekt"|_]}, State=#irc_client{channel=Channel}) ->
    zane_log:log(?MODULE, "Responding to !rekt"),
    timer:sleep(3000),
    irc_proto:say(Channel, "rectum? damn near killed 'im!"),
    {ok, State};
handle_event({privmsg, _From, Channel, ["cmon!"|_]}, State=#irc_client{channel=Channel}) ->
    zane_log:log(?MODULE, "Responding to cmon!"),
    timer:sleep(1000),
    irc_proto:say(Channel, "yeah, like i'm going to take a whiz through this $5000 suit. c'mon!"),
    {ok, State};
handle_event({privmsg, _From, Channel, ["!same"|_]}, State=#irc_client{channel=Channel}) ->
    zane_log:log(?MODULE, "Responding to !same"),
    timer:sleep(3000),
    irc_proto:say(Channel, "eh, not really the same"),
    {ok, State};
handle_event({privmsg, "timeshifter", Channel, ["SHUT", "UP"|_]}, State=#irc_client{channel=Channel}) ->
    zane_log:log(?MODULE, "Responding to timeshifter: SHUT UP"),
    timer:sleep(750),
    irc_proto:say(Channel, "YOU SHUT UP"),
    timer:sleep(1000),
    irc_proto:say(Channel, "god damn loudmouth bastard"),
    {ok, State};
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

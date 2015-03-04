-module(plug_webdev).
-behaviour(gen_event).
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


handle_event({privmsg, _From, Channel, ["!eval"|_Args]}, State=#state{sock=Sock, client=#irc_client{channel=Channel}}) ->
    timer:sleep(1500),
    irc_proto:say(Sock, Channel, "wrong! the answer is 42"),
    {ok, State};

handle_event({privmsg, _From, Channel, ["!trivia"|_Args]}, State=#state{sock=Sock, client=#irc_client{channel=Channel}}) ->
    timer:sleep(1500),
    irc_proto:say(Sock, Channel, "catfact?"),
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

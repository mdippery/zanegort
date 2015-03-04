-module(zane_ctcp).
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


handle_event({privmsg, From, Nickname, DirtyArgs}, State=#state{sock=Sock, client=#irc_client{nickname=Nickname}}) ->
    Args = lists:map(fun zane_string:remove_001/1, DirtyArgs),
    dispatch(Sock, From, Args),
    {ok, State};

handle_event({privmsg, _From, _Nickname, _Args}, State) ->
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


dispatch(Sock, From, ["VERSION"]) ->
    irc_proto:ctcp(Sock, From, ?VERSION);

dispatch(Sock, From, ["SOURCE"]) ->
    irc_proto:ctcp(Sock, From, ?SOURCE);

dispatch(_Sock, From, Args) ->
    zane_log:log(?MODULE, "Unrecognized CTCP from ~p: ~p", [From, Args]),
    nil.

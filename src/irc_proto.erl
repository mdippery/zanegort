-module(irc_proto).
-behaviour(gen_server).

-export([start_link/2]).
-export([
    pong/1,
    nick/1,
    user/1,
    join/1,
    quit/1,
    say/2,
    me/2,
    ctcp/3
]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {host, port, sock}).

-define(SRV, ?MODULE).


start_link(Host, Port) ->
    gen_server:start_link({local, ?SRV}, ?MODULE, {Host, Port}, []).

pong(Resp) -> send("PONG " ++ Resp).

nick(Nickname) -> send_sync("NICK " ++ Nickname).

user(Username) -> send_sync("USER " ++ Username ++ " 0 * :" ++ Username).

join(Channel) -> send_sync("JOIN :" ++ Channel).

quit(Msg) ->
    Resp = send_sync("QUIT :" ++ Msg),
    gen_server:call(?SRV, close_connection),
    Resp.

say(To, Msg) -> send("PRIVMSG " ++ To ++ " :" ++ Msg).

me(To, Action) ->
    Msg = [1|"ACTION "] ++ Action ++ [1],
    send("PRIVMSG " ++ To ++ " :" ++ Msg).

ctcp(To, Action, Msg) when is_atom(Action) ->
    ctcp(To, string:to_upper(atom_to_list(Action)), Msg);
ctcp(To, Action, Msg) ->
    UAction = string:to_upper(Action),
    FullMsg = UAction ++ " " ++ Msg,
    CtcpMsg = [1|FullMsg] ++ [1],
    notice(To, CtcpMsg).

notice(To, Msg) -> send("NOTICE " ++ To ++ " :" ++ Msg).


%% Behaviour: gen_server
%% ----------------------------------------------------------------------------

init({Host, Port}) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [{packet, line}]),
    State = #state{host=Host, port=Port, sock=Sock},
    {ok, State}.

handle_call({send, Line}, _From, State=#state{sock=Sock}) ->
    gen_tcp:send(Sock, Line),
    {reply, ok, State};
handle_call(close_connection, _From, State=#state{sock=Sock}) ->
    gen_tcp:close(Sock),
    {reply, ok, State};
handle_call(Msg, From, State) ->
    zane_log:log(?MODULE, "Ignoring unknown message from ~p: ~p", [From, Msg]),
    {noreply, State}.

handle_cast({send, Line}, State=#state{sock=Sock}) ->
    gen_tcp:send(Sock, Line),
    {noreply, State};
handle_cast(Msg, State) ->
    zane_log:log(?MODULE, "Ignoring unknown message: ~p", [Msg]),
    {noreply, State}.

handle_info({tcp, Sock, Data}, State=#state{sock=Sock}) ->
    zane_bot:handle_line(Data),
    {noreply, State};
handle_info({tcp_closed, Sock}, State=#state{sock=Sock}) ->
    zane_log:log(?MODULE, "Socket closed"),
    zane_bot:handle_disconnect(),
    {stop, tcp_closed, State};
handle_info(Msg, State) ->
    zane_log:log(?MODULE, "Ignoring unknown message: ~p", [Msg]),
    {noreply, State}.

terminate(Reason, _State) ->
    zane_log:log(?MODULE, "Terminating: ~p", [Reason]),
    ok.

code_change(OldVsn, State, _Extra) ->
    zane_log:log(?MODULE, "Performing code upgrade from ~p", [OldVsn]),
    {ok, State}.


%% Private implementation
%% ----------------------------------------------------------------------------

send(Line) -> gen_server:cast(?SRV, {send, Line ++ "\r\n"}).

send_sync(Line) -> gen_server:call(?SRV, {send, Line ++ "\r\n"}).

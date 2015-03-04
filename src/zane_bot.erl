-module(zane_bot).
-behaviour(gen_server).
-include("zane.hrl").

-export([start_link/4, stop/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SRV, ?MODULE).
-define(QUIT, "User terminated connection").


-record(state, {client, sock}).


start_link(Host, Port, Nickname, Channel) ->
    gen_server:start_link({local, ?SRV}, ?MODULE, {Host, Port, Nickname, Channel}, []).


stop() ->
    gen_server:cast(?SRV, disconnect).



%% Behaviour: gen_server
%% ----------------------------------------------------------------------------


init({Host, Port, Nickname, Channel}) ->
    Client = #irc_client{host=Host, port=Port, nickname=Nickname, channel=Channel},
    {ok, Sock} = gen_tcp:connect(Host, Port, [{packet, line}]),
    irc_proto:nick(Sock, Nickname),
    irc_proto:user(Sock, Nickname),
    State = #state{client=Client, sock=Sock},
    {ok, State}.


handle_call(Msg, From, State) ->
    zane_log:log(?MODULE, "Ignoring unknown message from ~p: ~p", [From, Msg]),
    {noreply, State}.


handle_cast(disconnect, State=#state{sock=Sock}) ->
    irc_proto:quit(Sock, ?QUIT),
    gen_tcp:close(Sock),
    {stop, normal, State};

handle_cast({disconnect, Reason, Quit}, State=#state{sock=Sock}) ->
    irc_proto:quit(Sock, Quit),
    gen_tcp:close(Sock),
    {stop, {error, Reason}, State};

handle_cast(Msg, State) ->
    zane_log:log(?MODULE, "Ignoring unknown message: ~p", [Msg]),
    {noreply, State}.


handle_info({tcp, Sock, Data}, State=#state{sock=Sock}) ->
    Line = zane_string:strip(Data),
    process_line(State, string:tokens(Line, " :")),
    {noreply, State};

handle_info({tcp_closed, _Sock}, State) ->
    zane_log:log(?MODULE, "Socket closed"),
    {stop, tcp_closed, State};

handle_info(Msg, State) ->
    zane_log:log(?MODULE, "Ignoring unknown message: ~p", [Msg]),
    {noreply, State}.


terminate(Reason, _State) ->
    zane_log:log(?MODULE, "Terminating (~p)", [Reason]),
    ok.


code_change(OldVsn, State, _Extra) ->
    zane_log:log(?MODULE, "Performing code upgrade from ~p", [OldVsn]),
    {ok, State}.



%% Private implementation
%% ----------------------------------------------------------------------------


process_line(#state{sock=Sock, client=#irc_client{channel=Channel}}, [_,"376"|_]) ->
    irc_proto:join(Sock, Channel);

process_line(#state{client=#irc_client{nickname=Nickname, channel=Channel}}, [_,"474"|_]) ->
    zane_log:log(?MODULE, "~p is banned from ~p", [Nickname, Channel]),
    gen_server:cast(?SRV, {disconnect, banned, "aw :("});

process_line(#state{sock=Sock}, ["PING"|Rest]) ->
    irc_proto:pong(Sock, Rest);

process_line(#state{sock=Sock, client=Client}, [Sender,"PRIVMSG",To|Args]) ->
    From = extract_nickname(Sender),
    zane_cmd:handle(Sock, Client, From, To, Args),
    zane_ctcp:handle(Sock, Client, From, To, Args),
    ok;

process_line(#state{sock=Sock}, [_,"KICK",Channel|Args]) ->
    zane_log:log(?MODULE, "Kicked from ~p: ~p. Rejoining.", [Channel, Args]),
    timer:sleep(5000),
    irc_proto:join(Sock, Channel);

process_line(_State, _Line) -> ok.


extract_nickname(Username) ->
    [User|_] = string:tokens(Username, "@"),
    [From|_] = string:tokens(User, "!"),
    From.

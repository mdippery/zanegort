-module(zane_bot).
-behaviour(gen_server).
-include("zanegort.hrl").

-export([start_link/0, stop/0]).
-export([handle_line/1, handle_connect/0, handle_disconnect/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SRV, ?MODULE).
-define(EVENT_SRV, event_mgr).
-define(PLUGINS, [plug_cmd, plug_ctcp, plug_webdev]).
-define(QUIT, "User terminated connection").

-record(state, {client}).


start_link() ->
    {ok, Host} = application:get_env(zanegort, irc_host),
    {ok, Port} = application:get_env(zanegort, irc_port),
    {ok, Nickname} = application:get_env(zanegort, irc_nickname),
    {ok, Channel} = application:get_env(zanegort, irc_channel),
    gen_server:start_link({local, ?SRV}, ?MODULE, {Host, Port, Nickname, Channel}, []).

stop() ->
    gen_server:cast(?SRV, disconnect).

handle_line(Line) ->
    gen_server:cast(?SRV, {received, Line}).

handle_connect() ->
    gen_server:cast(?SRV, connected).

handle_disconnect() ->
    gen_server:cast(?SRV, tcp_closed).


%% Behaviour: gen_server
%% ----------------------------------------------------------------------------

init({Host, Port, Nickname, Channel}) ->
    Client = #irc_client{host=Host, port=Port, nickname=Nickname, channel=Channel},
    {ok, _} = zane_log:start_link(),
    {ok, _} = irc_proto:start_link(Host, Port),
    {ok, _} = gen_event:start_link({local, ?EVENT_SRV}),
    lists:foreach(
        fun(Plugin) -> gen_event:add_handler(?EVENT_SRV, Plugin, Client) end,
        ?PLUGINS),
    State = #state{client=Client},
    zane_log:log(?MODULE, "Connected: ~s:~p/~s as ~s", [Host, Port, Channel, Nickname]),
    {ok, State}.

handle_call(Msg, From, State) ->
    zane_log:log(?MODULE, "Ignoring unknown message from ~p: ~p", [From, Msg]),
    {noreply, State}.

handle_cast({received, Data}, State) ->
    Line = zane_string:strip(Data),
    process_line(State, string:tokens(Line, " :")),
    {noreply, State};
handle_cast(connected, State=#state{client=#irc_client{nickname=Nickname}}) ->
    irc_proto:nick(Nickname),
    irc_proto:user(Nickname),
    {noreply, State};
handle_cast(disconnect, State) ->
    irc_proto:quit(?QUIT),
    {stop, normal, State};
handle_cast({disconnect, Reason, Quit}, State) ->
    irc_proto:quit(Quit),
    {stop, {error, Reason}, State};
handle_cast(tcp_closed, State) ->
    zane_log:log(?MODULE, "Socket closed"),
    {stop, tcp_closed, State};
handle_cast(Msg, State) ->
    zane_log:log(?MODULE, "Ignoring unknown message: ~p", [Msg]),
    {noreply, State}.

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

process_line(#state{client=#irc_client{channel=Channel}}, [_,"376"|_]) ->
    irc_proto:join(Channel);
process_line(#state{client=#irc_client{channel=Channel}}, [_,"473"|_]) ->
    zane_log:log(?MODULE, "~p is invite-only", [Channel]),
    gen_server:cast(?SRV, {disconnect, inviteonly, "aw :("});
process_line(#state{client=#irc_client{nickname=Nickname, channel=Channel}}, [_,"474"|_]) ->
    zane_log:log(?MODULE, "~p is banned from ~p", [Nickname, Channel]),
    gen_server:cast(?SRV, {disconnect, banned, "aw :("});
process_line(_State, ["PING"|Rest]) ->
    irc_proto:pong(Rest);
process_line(_State, [Sender,"PRIVMSG",To|Args]) ->
    From = extract_nickname(Sender),
    gen_event:notify(?EVENT_SRV, {privmsg, From, To, Args});
process_line(#state{client=#irc_client{nickname=Nickname}}, [_,"KICK",Channel,Nickname|Args]) ->
    Msg = string:join(Args, " "),
    zane_log:log(?MODULE, "Kicked from ~p: ~p. Rejoining.", [Channel, Msg]),
    timer:sleep(5000),
    irc_proto:join(Channel);
process_line(_State, _Line) ->
    ok.

extract_nickname(Username) ->
    [User|_] = string:tokens(Username, "@"),
    [From|_] = string:tokens(User, "!"),
    From.

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


start_link(Host, Port, Nickname, Channel) ->
    gen_server:start_link({local, ?SRV}, ?MODULE, [Host, Port, Nickname, Channel], []).


stop() ->
    gen_server:call(?SRV, disconnect).



%% Behaviour: gen_server
%% ----------------------------------------------------------------------------


init([Host, Port, Nickname, Channel]) ->
    Client = #irc_client{host=Host, port=Port, nickname=Nickname, channel=Channel},
    {ok, Sock} = gen_tcp:connect(Host, Port, [{packet, line}]),
    irc_proto:nick(Sock, Nickname),
    irc_proto:user(Sock, Nickname),
    {ok, {Sock, Client}}.


handle_call(disconnect, _From, {Sock, Client}) ->
    irc_proto:quit(Sock, ?QUIT),
    gen_tcp:close(Sock),
    {stop, normal, ok, {Sock, Client}};

handle_call(Msg, From, State) ->
    zane_log:log(?MODULE, "Ignoring unknown message from ~p: ~p", [From, Msg]),
    {noreply, State}.


handle_cast(Msg, State) ->
    zane_log:log(?MODULE, "Ignoring unknown message: ~p", [Msg]),
    {noreply, State}.


handle_info({tcp, Sock, Data}, {Sock, Client}) ->
    Line = zane_string:strip(Data),
    process_line(Sock, Client, string:tokens(Line, " :")),
    {noreply, {Sock, Client}};

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


process_line(Sock, #irc_client{channel=Channel}, [_,"376"|_]) ->
    irc_proto:join(Sock, Channel);

process_line(Sock, _Client, ["PING"|Rest]) ->
    irc_proto:pong(Sock, Rest);

process_line(Sock, Client, [From,"PRIVMSG",To|Args]) ->
    [MaybeCmd|Rest] = Args,
    Nick = zane_utils:extract_nickname(From),
    case zane_utils:extract_command(MaybeCmd) of
        nil ->
            case zane_utils:extract_ctcp(Client, To, MaybeCmd) of
                nil -> ok;
                Ctcp -> zane_ctcp:handle(Sock, Nick, Ctcp)
            end;
        Cmd ->
            zane_cmd:handle(Sock, Client, Nick, Cmd, Rest)
    end;

process_line(Sock, _Client, [_,"KICK",Channel|Args]) ->
    zane_log:log(?MODULE, "Kicked from ~p: ~p. Rejoining.", [Channel, Args]),
    timer:sleep(5000),
    irc_proto:join(Sock, Channel);

process_line(_Sock, _Client, _Line) -> ok.

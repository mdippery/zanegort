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
    {ok, [Sock, Client]}.


handle_call(disconnect, _From, [Sock|Rest]) ->
    irc_proto:quit(Sock, "User terminated connection"),
    gen_tcp:close(Sock),
    {stop, stopped, ok, [Sock|Rest]};

handle_call(Msg, From, State) ->
    io:format("zane_bot: Ignoring unknown message ~p from ~p~n", [Msg, From]),
    {noreply, State}.


handle_cast(Msg, State) ->
    io:format("zane_bot: Ignoring unknown message ~p~n", [Msg]),
    {noreply, State}.


handle_info({tcp, Sock, Data}, [Sock,Client|_Rest]) ->
    Line = zane_string:strip(Data),
    process_line(Sock, Client, string:tokens(Line, " :")),
    {noreply, [Sock, Client]};

handle_info({tcp_closed, _Sock}, State) ->
    io:format("zane_bot: Socket closed"),
    {stop, tcp_closed, State};

handle_info(Msg, State) ->
    io:format("state = ~p~n", [State]),
    io:format("zane_bot: Ignoring unknown message ~p~n", [Msg]),
    {noreply, State}.


terminate(Reason, _State) ->
    io:format("zane_bot: Terminating due to ~p~n", [Reason]),
    ok.


code_change(OldVsn, State, _Extra) ->
    io:format("zane_bot: Performing code upgrade from ~p~n", [OldVsn]),
    {ok, State}.



%% Private implementation
%% ----------------------------------------------------------------------------


process_line(Sock, #irc_client{channel=Channel}, [_,"376"|_]) ->
    irc_proto:join(Sock, Channel);

process_line(Sock, _Client, ["PING"|Rest]) ->
    irc_proto:pong(Sock, Rest);

process_line(Sock, Client, [From,"PRIVMSG",_Channel|Args]) ->
    [MaybeCmd|Rest] = Args,
    case zane_utils:is_command(MaybeCmd) of
        true ->
            Nick = zane_irc:extract_nickname(From),
            Cmd = zane_utils:extract_command(MaybeCmd),
            zane_cmd:handle(Sock, Client, Nick, Cmd, Rest);
        false ->
            ok
    end;

process_line(Sock, _Client, [_,"KICK",Channel|Args]) ->
    io:format("Kicked from ~p: ~p. Rejoining.~n", [Channel, Args]),
    timer:sleep(5000),
    irc_proto:join(Sock, Channel);

process_line(_Sock, _Client, _Line) -> ok.

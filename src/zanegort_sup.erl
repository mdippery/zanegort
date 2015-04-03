-module(zanegort_sup).
-behaviour(supervisor).
-include("zanegort.hrl").

-export([start_link/0]).
-export([init/1]).

-define(SRV, ?MODULE).
-define(SHUTDOWN, 5000).


start_link() ->
    {ok, Host} = application:get_env(zanegort, irc_host),
    {ok, Port} = application:get_env(zanegort, irc_port),
    {ok, Nickname} = application:get_env(zanegort, irc_nickname),
    {ok, Channel} = application:get_env(zanegort, irc_channel),
    supervisor:start_link({local, ?SRV}, ?MODULE, {Host, Port, Nickname, Channel}).


%% Behaviour: supervisor
%% ----------------------------------------------------------------------------

init({Host, Port, Nickname, Channel}) ->
    Client = #irc_client{host=Host, port=Port, nickname=Nickname, channel=Channel},
    Opts = {one_for_one, 5, 60},
    Specs = [
        worker(zane_log, []),
        worker(zane_bot, [Client]),
        supervisor(zane_plug_sup, [Client]),
        worker(irc_proto, [Host, Port])
    ],
    {ok, {Opts, Specs}}.


%% Private implementation
%% ----------------------------------------------------------------------------

worker(Module, Args) ->
    {Module,
     {Module, start_link, Args},
     permanent, ?SHUTDOWN, worker, [Module]}.

supervisor(Module, Args) ->
    {Module,
     {Module, start_link, Args},
     permanent, ?SHUTDOWN, supervisor, [Module]}.

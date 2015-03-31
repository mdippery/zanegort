-module(zanegort_sup).
-behaviour(supervisor).
-include("zanegort.hrl").

-export([start_link/4]).
-export([init/1]).

-define(SRV, ?MODULE).
-define(SHUTDOWN, 5000).


start_link(Host, Port, Nickname, Channel) ->
    supervisor:start_link({local, ?SRV}, ?MODULE, {Host, Port, Nickname, Channel}).


%% Behaviour: supervisor
%% ----------------------------------------------------------------------------

init({Host, Port, Nickname, Channel}) ->
    Client = #irc_client{host=Host, port=Port, nickname=Nickname, channel=Channel},
    Opts = {one_for_one, 5, 60},
    Specs = [
        worker(zane_log, []),
        worker(irc_proto, [Host, Port]),
        worker(zane_bot, [Client]),
        supervisor(zane_plug_sup, [Client])
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

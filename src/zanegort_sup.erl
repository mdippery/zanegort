-module(zanegort_sup).
-behaviour(supervisor).

-export([start_link/4]).
-export([init/1]).

-define(SRV, ?MODULE).
-define(EVENT_SRV, event_mgr).
-define(SHUTDOWN, 5000).


start_link(Host, Port, Nickname, Channel) ->
    supervisor:start_link({local, ?SRV}, ?MODULE, {Host, Port, Nickname, Channel}).


%% Behaviour: supervisor
%% ----------------------------------------------------------------------------

init({Host, Port, Nickname, Channel}) ->
    Opts = {one_for_one, 5, 60},
    Specs = [
        worker(zane_log, []),
        worker(gen_event, [{local, ?EVENT_SRV}]),
        worker(irc_proto, [Host, Port]),
        worker(zane_bot, [Host, Port, Nickname, Channel])
    ],
    {ok, {Opts, Specs}}.


%% Private implementation
%% ----------------------------------------------------------------------------

worker(Module, Args) ->
    {Module,
     {Module, start_link, Args},
     permanent, ?SHUTDOWN, worker, [Module]}.

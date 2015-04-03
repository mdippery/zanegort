-module(zanegort_sup).
-behaviour(supervisor).
-include("zanegort.hrl").

-export([start_link/0]).
-export([init/1]).

-define(SRV, ?MODULE).
-define(SHUTDOWN, 5000).

-define(CHILD(Type, Module, Args), {Module,
                                    {Module, start_link, Args},
                                    permanent, ?SHUTDOWN, Type, [Module]}).
-define(WORKER(Module, Args), ?CHILD(worker, Module, Args)).
-define(SUPERVISOR(Module, Args), ?CHILD(supervisor, Module, Args)).


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
        ?WORKER(zane_log, []),
        ?WORKER(zane_bot, [Client]),
        ?SUPERVISOR(zane_plug_sup, [Client]),
        ?WORKER(irc_proto, [Host, Port])
    ],
    {ok, {Opts, Specs}}.

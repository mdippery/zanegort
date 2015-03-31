-module(zane_plug_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SRV, ?MODULE).
-define(EVENT_SRV, event_mgr).
-define(PLUGINS, [plug_cmd, plug_ctcp, plug_webdev]).


start_link(Client) ->
    {ok, Pid} = supervisor:start_link({local, ?SRV}, ?MODULE, []),
    lists:foreach(
        fun(Plugin) -> gen_event:add_handler(?EVENT_SRV, Plugin, Client) end,
        ?PLUGINS),
    {ok, Pid}.


%% Behaviour: supervisor
%% ----------------------------------------------------------------------------

init([]) ->
    Opts = {one_for_one, 6, 60},
    Specs = [
        {?EVENT_SRV,
         {gen_event, start_link, [{local, ?EVENT_SRV}]},
         permanent, 5000, worker, [dynamic]}
    ],
    {ok, {Opts, Specs}}.

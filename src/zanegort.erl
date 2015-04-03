-module(zanegort).
-behaviour(application).
-export([vsn/0]).
-export([start/2, stop/1]).


vsn() -> vsn(zanegort).

vsn(Application) -> vsn(Application, application:which_applications()).

vsn(_Application, []) -> undefined;
vsn(Application, [{Application,_,Vsn}|_]) -> Vsn;
vsn(Application, [_|Rest]) -> vsn(Application, Rest).


%% Behaviour: application
%% ----------------------------------------------------------------------------

start(_StartType, _StartArgs) -> zanegort_sup:start_link().

stop(_State) -> ok.

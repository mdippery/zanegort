-module(zanegort_app).
-export([vsn/0]).


vsn() -> vsn(zanegort).

vsn(Application) -> vsn(Application, application:which_applications()).

vsn(_Application, []) -> undefined;
vsn(Application, [{Application,_,Vsn}|_]) -> Vsn;
vsn(Application, [_|Rest]) -> vsn(Application, Rest).

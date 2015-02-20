-module(zane_string).
-export([strip/1]).


strip(S) -> string:strip(S, both, $\n).

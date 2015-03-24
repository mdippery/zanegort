-module(zane_string).
-export([strip/1]).


strip(S) -> string:strip(string:strip(S, right, $\n), right, $\r).

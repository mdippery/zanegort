-module(zane_string).
-export([strip/1, remove_001/1]).


strip(S) -> string:strip(string:strip(S, right, $\n), right, $\r).

remove_001(S) -> string:strip(S, both, 1).

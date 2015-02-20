-module(zane_irc).
-export([extract_nickname/1]).



extract_nickname(Username) ->
    [User|_] = string:tokens(Username, "@"),
    [From|_] = string:tokens(User, "!"),
    From.

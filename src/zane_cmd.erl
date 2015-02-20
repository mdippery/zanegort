-module(zane_cmd).
-export([handle/2]).


handle("!set", Args) ->
    io:format("Handling 'set ~s'~n", [Args]);

handle("!source", _Args) ->
    io:format("Source is https://github.com/mdippery/zanegort~n");

handle("!web", [Url|_Rest]) ->
    io:format("Returning website for ~s~n", [Url]);

handle("!github", [Username|_Rest]) ->
    io:format("GitHub: https://github.com/~s~n", [Username]);

handle("!stack", [UserId|_Rest]) ->
    io:format("Stack Overflow: http://stackoverflow.com/users/~s~n", [UserId]);

handle("!help", _Args) ->
    io:format("Responding to help request~n");

handle(Other, Args) ->
    io:format("Invalid cmd: ~s ~s. Ignoring.~n", [Other, Args]).

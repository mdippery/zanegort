-module(os_utils).

-export([getenv/2]).


getenv(Key, Default) ->
    case os:getenv(Key)  of
        false -> Default;
        Value -> Value
    end.

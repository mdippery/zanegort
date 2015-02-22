-module(zane_utils).
-include("zane.hrl").

-export([
    getenv/2,
    is_command/1, is_command/2,
    extract_command/1, extract_command/2
]).


getenv(Key, Default) ->
    case os:getenv(Key)  of
        false -> Default;
        Value -> Value
    end.


cmd_prefix() -> getenv("ZANE_CMD_PREFIX", ?DEFAULT_CMD_PREFIX).


is_command(MaybeCmd) -> is_command(MaybeCmd, cmd_prefix()).


is_command(MaybeCmd, CmdPrefix) ->
    case string:substr(MaybeCmd, 1, string:len(CmdPrefix)) of
        CmdPrefix -> true;
        _ -> false
    end.


extract_command(RawCmd) -> extract_command(RawCmd, cmd_prefix()).


extract_command(RawCmd, CmdPrefix) ->
    case is_command(RawCmd, CmdPrefix) of
        true -> string:substr(RawCmd, string:len(CmdPrefix)+1);
        false -> throw(not_a_command)
    end.

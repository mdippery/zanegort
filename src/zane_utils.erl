-module(zane_utils).
-include("zane.hrl").

-export([
    is_command/1, is_command/2,
    extract_command/1, extract_command/2
]).

-define(CMD_PREFIX, os_utils:getenv("ZANE_CMD_PREFIX", ?DEFAULT_CMD_PREFIX)).


is_command(MaybeCmd) -> is_command(MaybeCmd, ?CMD_PREFIX).


is_command(MaybeCmd, CmdPrefix) ->
    case string:substr(MaybeCmd, 1, string:len(CmdPrefix)) of
        CmdPrefix -> true;
        _ -> false
    end.


extract_command(RawCmd) -> extract_command(RawCmd, ?CMD_PREFIX).


extract_command(RawCmd, CmdPrefix) ->
    case is_command(RawCmd, CmdPrefix) of
        true -> string:substr(RawCmd, string:len(CmdPrefix)+1);
        false -> throw(not_a_command)
    end.

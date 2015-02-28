-module(zane_utils).
-include("zane.hrl").

-export([extract_command/1]).

-define(CMD_PREFIX, os_utils:getenv("ZANE_CMD_PREFIX", ?DEFAULT_CMD_PREFIX)).


extract_command(RawCmd) -> extract_command(RawCmd, ?CMD_PREFIX).


extract_command(RawCmd, CmdPrefix) ->
    case is_command(RawCmd, CmdPrefix) of
        true -> string:substr(RawCmd, string:len(CmdPrefix)+1);
        false -> nil
    end.


is_command(MaybeCmd, CmdPrefix) ->
    case string:substr(MaybeCmd, 1, string:len(CmdPrefix)) of
        CmdPrefix -> true;
        _ -> false
    end.

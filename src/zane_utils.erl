-module(zane_utils).
-include("zane.hrl").

-export([extract_command/1, extract_ctcp/3]).

-define(CMD_PREFIX, os_utils:getenv("ZANE_CMD_PREFIX", ?DEFAULT_CMD_PREFIX)).


extract_command(RawCmd) -> extract_command(RawCmd, ?CMD_PREFIX).


extract_command(RawCmd, CmdPrefix) ->
    case is_command(RawCmd, CmdPrefix) of
        true -> string:substr(RawCmd, string:len(CmdPrefix)+1);
        false -> nil
    end.


extract_ctcp(Client, To, MaybeCtcp=[_|Rest]) ->
    case is_ctcp(Client, To, MaybeCtcp) of
        true -> Rest;
        false -> nil
    end.


is_command(MaybeCmd, CmdPrefix) ->
    case string:substr(MaybeCmd, 1, string:len(CmdPrefix)) of
        CmdPrefix -> true;
        _ -> false
    end.


is_ctcp(#irc_client{nickname=Nickname}, Nickname, [P|_Rest]) when P =:= $: -> true;
is_ctcp(_Client, _Nickname, _Command) -> false.

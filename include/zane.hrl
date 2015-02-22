-define(DEFAULT_CMD_PREFIX, "!").

-record(irc_client, {host, port=6667, nickname, channel}).

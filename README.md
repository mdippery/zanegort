**zanegort** is a simple IRC bot for performing some basic tasks in most IRC
channels, particularly programming-oriented ones.

## Building

You must have [rebar](https://github.com/rebar/rebar/#downloading) installed to
compile zanegort.

    $ rebar compile

## Usage

    $ erl -pa ebin
    > zane_bot:start_link(Hostname, Port, Nickname, Channel).
    > zane_bot:stop().

## Help

See the [manual](https://github.com/mdippery/zanegort/wiki/Help) for more
information on how to use zanegort.

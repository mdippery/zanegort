**zanegort** is a simple IRC bot for performing some basic tasks in most IRC
channels, particularly programming-oriented ones.

## Building

    $ erl -make

## Usage

    $ erl -pa ebin
    > zane_bot:start_link(Hostname, Port, Nickname, Channel).
    > zane_bot:stop().

## Help

See the [manual](https://github.com/mdippery/zanegort/wiki/Help) for more
information on how to use zanegort.

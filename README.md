**zanegort** is a simple IRC bot for performing some basic tasks in most IRC
channels, particularly programming-oriented ones.

## Configuring

1. Copy `config/sys.config.sample` to `config/sys.config`.
2. Change values in `config/sys.config` to suit your preferences. (The options
   should be fairly self-explanatory.)

## Building

You must have [rebar](https://github.com/rebar/rebar/#downloading) installed to
compile zanegort.

    $ make release

## Usage

    $ _rel/zanegort/bin/zanegort

## Help

See the [manual](https://github.com/mdippery/zanegort/wiki/Help) for more
information on how to use zanegort.

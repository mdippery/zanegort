-module(time_utils).
-export([
    minutes/1, minutes/2,
    hours/1, hours/2,
    days/1, days/2,
    total_seconds/2
]).


minutes(Seconds) -> Seconds div 60.

minutes(Megaseconds, Seconds) -> minutes(total_seconds(Megaseconds, Seconds)).

hours(Seconds) -> minutes(Seconds) div 60.

hours(Megaseconds, Seconds) -> hours(total_seconds(Megaseconds, Seconds)).

days(Seconds) -> hours(Seconds) div 24.

days(Megaseconds, Seconds) -> days(total_seconds(Megaseconds, Seconds)).

total_seconds(Megaseconds, Seconds) ->  Megaseconds * 1000000 + Seconds.

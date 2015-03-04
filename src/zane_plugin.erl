-module(zane_plugin).
-export([behaviour_info/1]).


behaviour_info(callbacks) -> [{handle, 5}];
behaviour_info(_) -> undefined.

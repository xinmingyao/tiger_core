-module(tiger_core_util).
-export([get_env/2]).


-spec get_env(atom(), term()) -> term().
get_env(Key, Default) ->
    case application:get_env(tiger_core, Key) of
	{ok, Value} -> {ok, Value};
	undefined   -> {ok, Default}
    end.

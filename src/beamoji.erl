%%% @doc 🚪⇢ 🪄
-module(beamoji).

-export([init/1]).

%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    beamoji_prv:init(State).

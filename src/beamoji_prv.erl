%%% @private
-module(beamoji_prv).

-export([init/1, do/1, format_error/1]).

%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([{name, '🪄'},
                          {module, beamoji_prv},
                          {bare, true},
                          {deps, []},
                          {example, "rebar3 🪄"},
                          {opts, []},
                          {short_desc, "🪄 ⇢ 🤯"},
                          {desc, "🪄📜 ⇢ 📃✨ ⇢ 🤯"}]),
    {ok, rebar_state:add_provider(State, Provider)}.

%% @private
%% @todo Implement this function
-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    rebar_api:warn("🪄 called ⚠️", []),
    {ok, State}.

%% @private
-spec format_error(any()) -> binary().
format_error(Reason) ->
    unicode:characters_to_binary(
        io_lib:format("~tp", [Reason])).

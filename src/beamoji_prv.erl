%%% @private
-module(beamoji_prv).

-export([init/1, do/1, format_error/1]).

%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([{name, emojify},
                          {module, beamoji_prv},
                          {bare, true},
                          {deps, []},
                          {example, "rebar3 🪄"},
                          {opts, []},
                          {short_desc, "blow your mind with emojis"},
                          {desc, "🪄📜 ⇢ 📃✨ ⇢ 🤯"}]),
    {ok, rebar_state:add_provider(State, Provider)}.

%% @private
-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    rebar_api:info("emojifying your code...", []),
    FormatOptions = rebar_state:get(State, format, []),
    NewState =
        rebar_state:set(State, format, [{formatter, beamoji_formatter} | FormatOptions]),
    rebar3_format_prv:do(NewState).

%% @private
-spec format_error(any()) -> binary().
format_error(Reason) ->
    unicode:characters_to_binary(
        io_lib:format("~tp", [Reason])).

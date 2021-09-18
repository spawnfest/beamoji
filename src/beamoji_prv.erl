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
                          {opts, opts()},
                          {short_desc, "blow your mind with emojis"},
                          {desc, "🪄📜 ⇢ 📃✨ ⇢ 🤯"}]),
    {ok, rebar_state:add_provider(State, Provider)}.

opts() ->
    [{translator,
      $🗣,
      "translator",
      {atom, beamoji_id_translator},
      "Translator module to use when emojifying the code"}].

%% @private
-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    Translator = get_translator(State),
    rebar_api:info("emojifying your code with ~p...", [Translator]),
    FormatOptions = rebar_state:get(State, format, []),
    FormatterOptions = proplists:get_value(options, FormatOptions, #{}),
    NewState =
        rebar_state:set(State,
                        format,
                        [{formatter, beamoji_formatter},
                         {options, FormatterOptions#{translator => Translator}}
                         | FormatOptions]),
    rebar3_format_prv:do(NewState).

get_translator(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    proplists:get_value(translator, Args, beamoji_id_translator).

%% @private
-spec format_error(any()) -> binary().
format_error(Reason) ->
    unicode:characters_to_binary(
        io_lib:format("~tp", [Reason])).

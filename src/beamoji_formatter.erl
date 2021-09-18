-module(beamoji_formatter).

-behaviour(rebar3_formatter).

-export([init/2, format_file/3]).

-spec init(rebar3_formatter:opts(), undefined | rebar_state:t()) -> beamoji_translator:'🗺'().
init(#{translator := Translator}, _) ->
    beamoji_translator:'🐣'(Translator).

%% @todo Replace this by the actual desired emojification of atoms.
-spec format_file(file:filename_all(), beamoji_translator:'🗺'(), rebar3_formatter:opts()) ->
                     rebar3_formatter:result().
format_file(File, State, #{translator := Translator} = OptionsMap) ->
    rebar_api:info("emojifying your code with ~p / ~p...", [Translator, State]),
    default_formatter:format_file(File, nostate, OptionsMap).

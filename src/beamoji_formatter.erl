-module(beamoji_formatter).

-behaviour(rebar3_formatter).

-export([init/2, format_file/3]).

-spec init(rebar3_formatter:opts(), undefined | rebar_state:t()) -> nostate.
init(_, _) ->
    nostate.

%% @todo Replace this by the actual desired emojification of atoms.
-spec format_file(file:filename_all(), nostate, rebar3_formatter:opts()) ->
                     rebar3_formatter:result().
format_file(File, nostate, OptionsMap) ->
    default_formatter:format_file(File, nostate, OptionsMap).

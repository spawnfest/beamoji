-module(booleans).

-beamoji_translator(beamoji_baseemoji_translator).

-include_lib("beamoji/include/beamoji.hrl").

-type t() :: true | false | undefined.
-export_type([t/0]).

-export([true/0, false/0, undefined/0]).

-spec true() -> t().
true() ->
    true.

-spec false() -> t().
false() ->
    false.

-spec undefined() -> t().
undefined() ->
    undefined.

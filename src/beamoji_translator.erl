-module(beamoji_translator).

-export(['🐣'/1, '⏩'/3, '⏪'/3]).

-type '⚛'() :: atom().
-type '⚛️'() :: atom().
-type '🗺'() :: map().

-export_type(['⚛'/0, '⚛️'/0, '🗺'/0]).

-callback '🐣'() -> '🗺'().
-callback '⏩'('⚛'(), '🗺'()) -> '⚛️'().
-callback '⏪'('⚛️'(), '🗺'()) -> '⚛'().

'🐣'(Translator) ->
    Translator:'🐣'().

'⏩'(UnquotedAtom, Translator, State) ->
    Translator:'⏩'(UnquotedAtom, State).

'⏪'(EmojifiedAtom, Translator, State) ->
    Translator:'⏪'(EmojifiedAtom, State).

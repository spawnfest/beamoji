-module(beamoji_translator).

-export(['🐣'/1, '⏩'/2, '⏪'/2]).
-export(['🗣'/1]).

-type '🗣'() :: module().
-type '⚛'() :: atom().
-type '⚛️'() :: atom().
-type '🗺'() :: map().

-opaque '🫖'() :: #{'🗣' := '🗣'(), '🗺' := '🗺'()}.

-export_type(['⚛'/0, '⚛️'/0, '🗺'/0, '🫖'/0]).

-callback '🐣'() -> '🗺'().
-callback '⏩'('⚛'(), '🗺'()) -> '⚛️'().
-callback '⏪'('⚛️'(), '🗺'()) -> '⚛'().

-spec '🐣'('🗣'()) -> '🫖'().
'🐣'(Translator) ->
    #{'🗣' => Translator, '🗺' => Translator:'🐣'()}.

-spec '⏩'('⚛'(), '🫖'()) -> '⚛️'().
'⏩'(UnquotedAtom, #{'🗣' := Translator, '🗺' := State}) ->
    Translator:'⏩'(UnquotedAtom, State).

-spec '⏪'('⚛️'(), '🫖'()) -> '⚛'().
'⏪'(EmojifiedAtom, #{'🗣' := Translator, '🗺' := State}) ->
    Translator:'⏪'(EmojifiedAtom, State).

-spec '🗣'('🫖'()) -> '🗣'().
'🗣'(#{'🗣' := Translator}) ->
    Translator.

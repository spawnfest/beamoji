-module(beamoji_emojilist_translator).

-behaviour(beamoji_translator).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

-export(['🐣'/0, '⏩'/2, '⏪'/2]).

-spec '🐣'() -> beamoji_translator:'🗺'().
'🐣'() ->
    Path = filename:join([code:priv_dir(beamoji), "emoji.eterm"]),
    {ok, [#{} = ShortToEmoji]} = file:consult(Path),
    Emojis = maps:values(ShortToEmoji),
    Shorts = maps:keys(ShortToEmoji),
    EmojiToShort =
        maps:from_list(
            lists:zip(Emojis, Shorts)),
    #{'⏩' => ShortToEmoji, '⏪' => EmojiToShort}.

-spec '⏩'(beamoji_translator:'⚛'(), beamoji_translator:'🗺'()) ->
             beamoji_translator:'⚛️'().
'⏩'(UnquotedAtom, #{'⏩' := ShortToEmoji}) ->
    case maps:get(atom_to_binary(UnquotedAtom), ShortToEmoji, undefined) of
        undefined ->
            UnquotedAtom;
        Value ->
            binary_to_atom(Value)
    end.

-spec '⏪'(beamoji_translator:'⚛️'(), beamoji_translator:'🗺'()) ->
             beamoji_translator:'⚛'().
'⏪'(EmojifiedAtom, #{'⏪' := EmojiToShort}) ->
    case maps:get(atom_to_binary(EmojifiedAtom), EmojiToShort, 0) of
        0 ->
            EmojifiedAtom;
        Value ->
            binary_to_atom(Value)
    end.

-ifdef(TEST).

'⏪_test'() ->
    State = '🐣'(),
    ?assertEqual(smile, '⏪'('😄', State)),
    ?assertEqual(true, '⏪'('✔️', State)),
    ?assertEqual(false, '⏪'('❌', State)),
    ?assertEqual(undefined, '⏪'('👻', State)),
    ok.

'⏩_test'() ->
    State = '🐣'(),
    ?assertEqual('😄', '⏩'(smile, State)),
    ?assertEqual('✔️', '⏩'(true, State)),
    ?assertEqual('❌', '⏩'(false, State)),
    ?assertEqual('👻', '⏩'(undefined, State)),
    ok.

-endif.

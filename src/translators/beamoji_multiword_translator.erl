%% @todo Remove edge cases (check CT suite)
%% @todo Remove compound words from words.eterm
-module(beamoji_multiword_translator).

-behaviour(beamoji_translator).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

-export(['🐣'/0, '⏩'/2, '⏪'/2]).

-spec '🐣'() -> beamoji_translator:'🗺'().
'🐣'() ->
    Path = filename:join([code:priv_dir(beamoji), "words.eterm"]),
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
    Words = binary:split(atom_to_binary(UnquotedAtom), <<"_">>, [global]),
    '⏩'(Words, ShortToEmoji, <<>>).

'⏩'([], _, _) ->
    '';
'⏩'([Word], ShortToEmoji, <<>>) ->
    case maps:get(Word, ShortToEmoji, {not_found}) of
        {not_found} ->
            binary_to_atom(Word);
        Value ->
            binary_to_atom(Value)
    end;
'⏩'([Word], ShortToEmoji, Acc) ->
    case maps:get(Word, ShortToEmoji, {not_found}) of
        {not_found} ->
            binary_to_atom(<<Acc/binary, $_, Word/binary>>);
        Value ->
            binary_to_atom(<<Acc/binary, Value/binary>>)
    end;
'⏩'([Word1, Word2 | Words], ShortToEmoji, <<>>) ->
    case maps:get(Word1, ShortToEmoji, {not_found}) of
        {not_found} ->
            case maps:get(Word2, ShortToEmoji, {not_found}) of
                {not_found} ->
                    '⏩'([Word2 | Words], ShortToEmoji, Word1);
                _ ->
                    '⏩'([Word2 | Words], ShortToEmoji, <<Word1/binary, $_>>)
            end;
        Value1 ->
            '⏩'([Word2 | Words], ShortToEmoji, Value1)
    end;
'⏩'([Word1, Word2 | Words], ShortToEmoji, Acc) ->
    case maps:get(Word1, ShortToEmoji, {not_found}) of
        {not_found} ->
            case maps:get(Word2, ShortToEmoji, {not_found}) of
                {not_found} ->
                    '⏩'([Word2 | Words], ShortToEmoji, <<Acc/binary, $_, Word1/binary>>);
                _ ->
                    '⏩'([Word2 | Words], ShortToEmoji, <<Acc/binary, $_, Word1/binary, $_>>)
            end;
        Value1 ->
            '⏩'([Word2 | Words], ShortToEmoji, <<Acc/binary, Value1/binary>>)
    end.

-spec '⏪'(beamoji_translator:'⚛️'(), beamoji_translator:'🗺'()) ->
             beamoji_translator:'⚛'().
'⏪'(EmojifiedAtom, #{'⏪' := EmojiToShort}) ->
    '⏪'(string:next_codepoint(atom_to_binary(EmojifiedAtom)), EmojiToShort, <<>>).

'⏪'([], _, Acc) ->
    binary_to_atom(Acc);
'⏪'([$_ | Rest], EmojiToShort, Acc) ->
    case binary:split(Rest, <<"_">>) of
        [Rest] ->
            '⏪'([], EmojiToShort, <<Acc/binary, $_, Rest/binary>>);
        [Word, <<>>] ->
            '⏪'([], EmojiToShort, <<Acc/binary, $_, Word/binary, $_>>);
        [Word, Next] ->
            '⏪'(string:next_codepoint(Next), EmojiToShort, <<Acc/binary, $_, Word/binary>>)
    end;
'⏪'([EmojiChar | Rest], EmojiToShort, Acc) ->
    Emoji = unicode:characters_to_binary([EmojiChar]),
    '⏪↩️'(Emoji, Rest, EmojiToShort, Acc).

'⏪'([$_ | Rest], EmojiToShort, WordAcc, <<>>) ->
    '⏪'(string:next_codepoint(Rest), EmojiToShort, WordAcc);
'⏪'([], EmojiToShort, EmojiAcc, Acc) ->
    '⏪↩️'(EmojiAcc, <<>>, EmojiToShort, Acc);
'⏪'([EmojiChar | Rest], EmojiToShort, EmojiAcc, Acc) ->
    EmojiCharBin = unicode:characters_to_binary([EmojiChar]),
    Emoji = <<EmojiAcc/binary, EmojiCharBin/binary>>,
    '⏪↩️'(Emoji, Rest, EmojiToShort, Acc).

'⏪↩️'(Emoji, Rest, EmojiToShort, Acc) ->
    case maps:get(Emoji, EmojiToShort, {not_found}) of
        {not_found} ->
            case Rest of
                <<>> ->
                    '⏪'([], EmojiToShort, <<Acc/binary, $_, Emoji/binary>>);
                Rest ->
                    '⏪'(string:next_codepoint(Rest), EmojiToShort, Emoji, Acc)
            end;
        Value ->
            case Acc of
                <<>> ->
                    '⏪'(string:next_codepoint(Rest), EmojiToShort, Value);
                Acc ->
                    '⏪'(string:next_codepoint(Rest), EmojiToShort, <<Acc/binary, $_, Value/binary>>)
            end
    end.

-ifdef(TEST).

'⏪_test'() ->
    State = '🐣'(),
    ?assertEqual(smile, '⏪'('😄', State)),
    ?assertEqual(true, '⏪'('✔️', State)),
    ?assertEqual(false, '⏪'('❌', State)),
    ?assertEqual(undefined, '⏪'('👻', State)),
    ?assertEqual(check_true, '⏪'('✅✔️', State)),
    ?assertEqual(check_true_or_false, '⏪'('✅✔️_or_❌', State)),
    ?assertEqual(or_check_true, '⏪'('or_✅✔️', State)),
    ?assertEqual(check_true_, '⏪'('✅✔️_', State)),
    ?assertEqual(check_true_or, '⏪'('✅✔️_or', State)),
    ?assertEqual(check_true_or_, '⏪'('✅✔️_or_', State)),
    ?assertEqual(impossible_to_find, '⏪'(impossible_to_find, State)),
    ?assertEqual('', '⏪'('', State)),
    ok.

'⏩_test'() ->
    State = '🐣'(),
    ?assertEqual('😄', '⏩'(smile, State)),
    ?assertEqual('✔️', '⏩'(true, State)),
    ?assertEqual('❌', '⏩'(false, State)),
    ?assertEqual('👻', '⏩'(undefined, State)),
    ?assertEqual('✅✔️', '⏩'(check_true, State)),
    ?assertEqual('✅✔️_or_❌', '⏩'(check_true_or_false, State)),
    ?assertEqual('or_✅✔️', '⏩'(or_check_true, State)),
    ?assertEqual('✅✔️_', '⏩'(check_true_, State)),
    ?assertEqual('✅✔️_or', '⏩'(check_true_or, State)),
    ?assertEqual('✅✔️_or_', '⏩'(check_true_or_, State)),
    ?assertEqual('', '⏩'('', State)),
    ok.

-endif.

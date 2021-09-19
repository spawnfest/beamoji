-module(beamoji_baseemoji_translator).

-behaviour(beamoji_translator).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

-export(['🐣'/0, '⏩'/2, '⏪'/2]).

-spec '🐣'() -> beamoji_translator:'🗺'().
'🐣'() ->
    Path = filename:join([code:priv_dir(beamoji), "base_emoji.eterm"]),
    {ok, [BaseEmoji]} = file:consult(Path),
    {_I, ToBaseEmoji, FromBaseEmoji} =
        lists:foldl(fun(Emoji, {I, To, From}) ->
                       {I + 1, maps:put(I, Emoji, To), maps:put(Emoji, I, From)}
                    end,
                    {0, #{}, #{}},
                    BaseEmoji),
    #{'⏩' => ToBaseEmoji, '⏪' => FromBaseEmoji}.

-spec '⏩'(beamoji_translator:'⚛'(), beamoji_translator:'🗺'()) ->
             beamoji_translator:'⚛️'().
'⏩'(UnquotedAtom, #{'⏩' := ToBaseEmoji}) ->
    Letters = atom_to_list(UnquotedAtom),
    Emojis = [maps:get(X, ToBaseEmoji, X) || X <- Letters],
    binary_to_atom(iolist_to_binary(Emojis)).

-spec '⏪'(beamoji_translator:'⚛️'(), beamoji_translator:'🗺'()) ->
             beamoji_translator:'⚛'().
'⏪'(EmojifiedAtom, #{'⏪' := FromBaseEmoji}) ->
    CodePoints = string:to_graphemes(atom_to_list(EmojifiedAtom)),
    Emojis = lists:map(fun characters_to_binary/1, CodePoints),
    Letters = [maps:get(X, FromBaseEmoji, X) || X <- Emojis],
    binary_to_atom(iolist_to_binary(Letters)).

characters_to_binary(A) when is_integer(A) ->
    unicode:characters_to_binary([A]);
characters_to_binary(L) when is_list(L) ->
    unicode:characters_to_binary(L).

-ifdef(TEST).

'⏪_test'() ->
    State = '🐣'(),
    ?assertEqual(smile, '⏪'('🆘🧉📱🦙👀', State)),
    ?assertEqual(true, '⏪'('🦖🌈🦄👀', State)),
    ?assertEqual(false, '⏪'('🔥🍎🦙🆘👀', State)),
    ?assertEqual(undefined, '⏪'('🦄🆕🐶👀🔥📱🆕👀🐶', State)),
    ?assertEqual(should, '⏪'('🆘❤️🆗🦄🦙🐶', State)),
    ok.

'⏩_test'() ->
    State = '🐣'(),
    ?assertEqual('🆘🧉📱🦙👀', '⏩'(smile, State)),
    ?assertEqual('🦖🌈🦄👀', '⏩'(true, State)),
    ?assertEqual('🔥🍎🦙🆘👀', '⏩'(false, State)),
    ?assertEqual('🦄🆕🐶👀🔥📱🆕👀🐶', '⏩'(undefined, State)),
    ok.

-endif.

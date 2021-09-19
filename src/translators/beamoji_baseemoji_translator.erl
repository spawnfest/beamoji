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
                    {1, #{}, #{}},
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
    Letters = atom_to_list(EmojifiedAtom),
    Emojis = [maps:get(<<X/utf8>>, FromBaseEmoji, X) || X <- Letters],
    binary_to_atom(iolist_to_binary(Emojis)).

-ifdef(TEST).

'⏪_test'() ->
    State = '🐣'(),
    ?assertEqual(smile, '⏪'('🆘🧉📱🦙👀', State)),
    ?assertEqual(true, '⏪'('🦖🌈🦄👀', State)),
    ?assertEqual(false, '⏪'('🔥🍎🦙🆘👀', State)),
    ?assertEqual(undefined, '⏪'('🦄🆕🐕️👀🔥📱🆕👀🐕️', State)),
    ok.

'⏩_test'() ->
    State = '🐣'(),
    ?assertEqual('🆘🧉📱🦙👀', '⏩'(smile, State)),
    ?assertEqual('🦖🌈🦄👀', '⏩'(true, State)),
    ?assertEqual('🔥🍎🦙🆘👀', '⏩'(false, State)),
    ?assertEqual('🦄🆕🐕️👀🔥📱🆕👀🐕️', '⏩'(undefined, State)),
    ok.

-endif.

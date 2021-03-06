-module(beamoji_baseemoji_translator).

-behaviour(beamoji_translator).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

-export(['π£'/0, 'β©'/2, 'βͺ'/2]).

-spec 'π£'() -> beamoji_translator:'πΊ'().
'π£'() ->
    Path = filename:join([code:priv_dir(beamoji), "base_emoji.eterm"]),
    {ok, [BaseEmoji]} = file:consult(Path),
    {_I, ToBaseEmoji, FromBaseEmoji} =
        lists:foldl(fun(Emoji, {I, To, From}) ->
                       {I + 1, maps:put(I, Emoji, To), maps:put(Emoji, I, From)}
                    end,
                    {0, #{}, #{}},
                    BaseEmoji),
    #{'β©' => ToBaseEmoji, 'βͺ' => FromBaseEmoji}.

-spec 'β©'(beamoji_translator:'β'(), beamoji_translator:'πΊ'()) ->
             beamoji_translator:'βοΈ'().
'β©'(UnquotedAtom, #{'β©' := ToBaseEmoji}) ->
    Letters = atom_to_list(UnquotedAtom),
    Emojis = [maps:get(X, ToBaseEmoji, X) || X <- Letters],
    binary_to_atom(iolist_to_binary(Emojis)).

-spec 'βͺ'(beamoji_translator:'βοΈ'(), beamoji_translator:'πΊ'()) ->
             beamoji_translator:'β'().
'βͺ'(EmojifiedAtom, #{'βͺ' := FromBaseEmoji}) ->
    CodePoints = string:to_graphemes(atom_to_list(EmojifiedAtom)),
    Emojis = lists:map(fun characters_to_binary/1, CodePoints),
    Letters = [maps:get(X, FromBaseEmoji, X) || X <- Emojis],
    binary_to_atom(iolist_to_binary(Letters)).

characters_to_binary(A) when is_integer(A) ->
    unicode:characters_to_binary([A]);
characters_to_binary(L) when is_list(L) ->
    unicode:characters_to_binary(L).

-ifdef(TEST).

'βͺ_test'() ->
    State = 'π£'(),
    ?assertEqual(smile, 'βͺ'('ππ§π±π¦π', State)),
    ?assertEqual(true, 'βͺ'('π¦ππ¦π', State)),
    ?assertEqual(false, 'βͺ'('π₯ππ¦ππ', State)),
    ?assertEqual(undefined, 'βͺ'('π¦ππΆππ₯π±πππΆ', State)),
    ?assertEqual(should, 'βͺ'('πβ€οΈππ¦π¦πΆ', State)),
    ok.

'β©_test'() ->
    State = 'π£'(),
    ?assertEqual('ππ§π±π¦π', 'β©'(smile, State)),
    ?assertEqual('π¦ππ¦π', 'β©'(true, State)),
    ?assertEqual('π₯ππ¦ππ', 'β©'(false, State)),
    ?assertEqual('π¦ππΆππ₯π±πππΆ', 'β©'(undefined, State)),
    ok.

-endif.

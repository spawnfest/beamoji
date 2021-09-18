-module(beamoji_pt_2).

-beamoji_translator(beamoji_emojilist_translator).

-export(['🤔'/1, '🤔'/0, '🤔'/2]).

'🤔'('✔️') ->
    '❌';
'🤔'('❌') ->
    '✔️';
'🤔'('👻') ->
    '🤔'('❌').

'🤔'() ->
    L = ['🤔', {'🤔'}, #{'🤔' => '🤔'}],
    '🤔':'🤔'(L).

'🤔'(A, B)
   when A == '🤔', B == '🤔' orelse A /= '🤔' ->
     begin '🤔' end.

-module(beamoji_pt_2_translated).

-beamoji_translator(beamoji_emojilist_translator).

-export([thinking_face/1, thinking_face/0, thinking_face/2]).

thinking_face(true) ->
    false;
thinking_face(false) ->
    true;
thinking_face(undefined) ->
    thinking_face(false).

thinking_face() ->
    L = [thinking_face, {thinking_face}, #{thinking_face => thinking_face}],
    thinking_face:thinking_face(L).

thinking_face(A, B)
  when A == thinking_face, B == thinking_face orelse A /= thinking_face ->
    begin thinking_face end.

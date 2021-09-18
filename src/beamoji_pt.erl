-module(beamoji_pt).

-export([parse_transform/2, parse_transform_info/0, walker/2]).

-type '👀'() :: term().

% Brujo would be proud
-callback walker('👀'(), '👀'()) -> {'👀'(), '👀'()}.

parse_transform(Forms0, Options) ->
    #{translator_mod := TranslatorMod} = Options,
    TranslatorState = beamoji_translator:'🐣'(TranslatorMod),
    TranslatorFn = fun beamoji_translator:'⏩'/2,
    State = #{translator_fn => TranslatorFn, translator_state => TranslatorState},
    {Forms, _NewState} = ast_walk:forms(Forms0, fun walker/2, State),
    Forms.

parse_transform_info() ->
    #{error_location => column}.

walker(State, {atom, Line, Atom}) ->
    #{translator_fn := TranslatorFn, translator_state := TranslatorState} = State,
    NewAtom = TranslatorFn(Atom, TranslatorState),
    NewAst = {atom, Line, NewAtom},
    {NewAst, State};
walker(State, Ast) ->
    {Ast, State}.

-module(beamoji_pt).

-export([parse_transform/2, parse_transform_info/0, walker/2]).

parse_transform(Forms0, _Options) ->
    State = update_translator(#{}, beamoji_id_translator),
    {Forms, _NewState} = ast_walk:forms(Forms0, fun walker/2, State),
    Forms.

parse_transform_info() ->
    #{error_location => column}.

update_translator(State0, TranslatorMod) ->
    TranslatorState = beamoji_translator:'🐣'(TranslatorMod),
    TranslatorFn = fun beamoji_translator:'⏪'/2,
    maps:merge(State0, #{translator_fn => TranslatorFn, translator_state => TranslatorState}).

walker(State, Ast = {attribute, _Anno, beamoji_translator, TranslatorMod}) ->
    NewState = update_translator(State, TranslatorMod),
    {Ast, NewState};
walker(State, {atom, Anno, Atom}) ->
    NewAtom = translate(State, Atom),
    NewAst = {atom, Anno, NewAtom},
    {NewAst, State};
walker(State, {function, Anno, Name, Arity, Clauses}) when is_atom(Name) ->
    NewName = translate(State, Name),
    NewAst = {function, Anno, NewName, Arity, Clauses},
    {NewAst, State};
walker(State, {attribute, Anno, export, Es}) ->
    NewEs = [{translate(State, Name), Arity} || {Name, Arity} <- Es],
    NewAst = {attribute, Anno, export, NewEs},
    {NewAst, State};
walker(State, Ast) ->
    {Ast, State}.

translate(State, Atom) ->
    #{translator_fn := TranslatorFn, translator_state := TranslatorState} = State,
    TranslatorFn(Atom, TranslatorState).

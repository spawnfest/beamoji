-module(beamoji_utils).

-export(['🪄'/2]).

-type translator() :: fun((erl_parse:abstract_form()) -> erl_parse:abstract_form()).

-spec '🪄'(translator(), erl_parse:abstract_form()) -> erl_parse:abstract_form().
'🪄'(Translator, {atom, Anno, Atom}) ->
    {atom, Anno, Translator(Atom)};
'🪄'(Translator, {function, Anno, Name, Arity, Clauses}) when is_atom(Name) ->
    {function, Anno, Translator(Name), Arity, Clauses};
'🪄'(Translator, {attribute, Anno, record, {Name, Fields}}) when is_atom(Name) ->
    {attribute, Anno, record, {Translator(Name), Fields}};
'🪄'(Translator, {attribute, Anno, AttrName, Es})
    when AttrName == export; AttrName == export_type ->
    {attribute, Anno, AttrName, '🪄🪄'(Translator, Es)};
'🪄'(Translator, {attribute, Anno, import, {Mod, Es}}) ->
    {attribute, Anno, import, {Mod, '🪄🪄'(Translator, Es)}};
'🪄'(Translator, {attribute, Anno, AttrName, {Name, Type, Variables}})
    when AttrName == type; AttrName == opaque ->
    {attribute, Anno, AttrName, {Translator(Name), Type, Variables}};
'🪄'(Translator, {attribute, Anno, AttrName, {{Name, Arity}, Clauses}})
    when AttrName == spec; AttrName == callback ->
    {attribute, Anno, AttrName, {{Translator(Name), Arity}, Clauses}};
'🪄'(Translator, {attribute, Anno, spec, {{Module, Name, Arity}, Clauses}}) ->
    {attribute, Anno, spec, {{Module, Translator(Name), Arity}, Clauses}};
'🪄'(_Translator, Ast) ->
    Ast.

'🪄🪄'(Translator, Es) ->
    [{Translator(Name), Arity} || {Name, Arity} <- Es].

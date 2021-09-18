-module(beamoji_pt_SUITE).

-compile(export_all).

all() ->
    [identity_pt,
     walker_works,
     walker_attr_updates_translator,
     beamoji_attr_updates_translator].

data_dir(Config) ->
    proplists:get_value(data_dir, Config).

identity_pt(Config) ->
    Forms = parse_test_module(Config, "pt_test_1.erl"),
    Opts = [],
    TransformedForms = beamoji_pt:parse_transform(Forms, Opts),
    Forms = TransformedForms.

walker_works(_Config) ->
    TranslatorState = i_am_the_translator_state,
    ReverseAtomFn =
        fun(UnquotedAtom, i_am_the_translator_state) ->
           Str = atom_to_list(UnquotedAtom),
           list_to_atom(lists:reverse(Str))
        end,
    State1 = #{translator_fn => ReverseAtomFn, translator_state => TranslatorState},
    {{atom, 42, cba}, State1} = beamoji_pt:walker(State1, {atom, 42, abc}),
    {{integer, 10, 42}, State1} = beamoji_pt:walker(State1, {integer, 10, 42}).

walker_attr_updates_translator(Config) ->
    Attr = {attribute, 1, beamoji_translator, beamoji_emojilist_translator},
    {Attr, #{translator_fn := Fn, translator_state := _}} = beamoji_pt:walker(#{}, Attr),
    true = is_function(Fn).

parse_test_module(Config, FileName) ->
    DataDir = data_dir(Config),
    ModPath = filename:join(DataDir, FileName),
    {ok, Forms} = epp:parse_file(ModPath, []),
    Forms.

beamoji_attr_updates_translator(Config) ->
    Forms = parse_test_module(Config, "pt_test_2.erl"),
    ExpectedForms = parse_test_module(Config, "pt_test_2_translated.erl"),
    Opts = [],
    TransformedForms = beamoji_pt:parse_transform(Forms, Opts),
    compare_mod_asts(TransformedForms, ExpectedForms).

compare_mod_asts([_FileAttr1, _ModAttr1 | Ast1], [_FileAttr2, _ModAttr2 | Ast1]) ->
    true.

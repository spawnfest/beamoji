-module(beamoji_pt_SUITE).

-export([all/0, identity_pt/1, walker_works/1]).

all() ->
    [identity_pt, walker_works].

data_dir(Config) ->
    proplists:get_value(data_dir, Config).

identity_pt(Config) ->
    DataDir = data_dir(Config),
    ModPath = filename:join(DataDir, "pt_test_1.erl"),
    {ok, Forms} = epp:parse_file(ModPath, []),
    Opts = #{translator_mod => beamoji_id_translator},
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

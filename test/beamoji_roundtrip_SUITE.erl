-module(beamoji_roundtrip_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
         end_per_testcase/2]).
-export([beamoji_baseemoji_translator/1, beamoji_emojilist_translator/1,
         beamoji_id_translator/1, beamoji_multiword_translator/1]).

all() ->
    [beamoji_baseemoji_translator, beamoji_emojilist_translator, beamoji_id_translator].

init_per_suite(Config) ->
    {ok, OldPath} = file:get_cwd(),
    [{old_path, OldPath} | Config].

end_per_suite(Config) ->
    ok =
        file:set_cwd(
            proplists:get_value(old_path, Config)).

init_per_testcase(TestCase, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Files = files(DataDir),
    TestDir = filename:join(DataDir, atom_to_list(TestCase)),
    file:make_dir(TestDir),
    lists:foreach(fun(File) ->
                     {ok, _} = file:copy(File, filename:join(TestDir, filename:basename(File)))
                  end,
                  Files),
    ok = file:set_cwd(TestDir),
    Config.

end_per_testcase(_TestCase, Config) ->
    {ok, Cwd} = file:get_cwd(),
    DataDir = proplists:get_value(data_dir, Config),
    ok = file:set_cwd(DataDir),
    ok = file:del_dir_r(Cwd),
    Config.

beamoji_baseemoji_translator(_Config) ->
    roundtrip(beamoji_baseemoji_translator).

beamoji_emojilist_translator(_Config) ->
    roundtrip(beamoji_emojilist_translator).

beamoji_id_translator(_Config) ->
    roundtrip(beamoji_id_translator).

%% @todo Make this one work. It still has some edge cases.
beamoji_multiword_translator(_Config) ->
    roundtrip(beamoji_multiword_translator).

roundtrip(Translator) ->
    {ok, Cwd} = file:get_cwd(),
    OriginalASTs = get_asts(Cwd),
    State = initialize_rebar(Translator),
    {ok, _} = beamoji_prv:do(State),
    [] = diff(OriginalASTs, get_asts(Cwd)),
    ok.

get_asts(Cwd) ->
    lists:map(fun get_ast/1, files(Cwd)).

get_ast(File) ->
    IncludePath =
        filename:join(
            code:lib_dir(beamoji), "include"),
    {ok, Forms} = epp:parse_file(File, [{includes, [IncludePath]}]),
    {File, beamoji_pt:parse_transform(Forms, [])}.

files(Dir) ->
    filelib:wildcard(
        filename:join(Dir, "**/*.erl")).

initialize_rebar(Translator) ->
    {ok, State1} =
        beamoji:init(
            rebar_state:new()),
    {ok, State2} = rebar_prv_app_discovery:do(State1),
    {ok, Cwd} = file:get_cwd(),
    Files = {files, files(Cwd)},
    State3 = rebar_state:set(State2, format, [Files]),
    rebar_state:command_parsed_args(State3, {[{translator, Translator}], something}).

diff(Original, New) ->
    Changes =
        lists:zipwith(fun({File, OldAST}, {File, NewAST}) ->
                         CleanOldAST = remove_known_attrs(remove_line_numbers(OldAST)),
                         CleanNewAST = remove_known_attrs(remove_line_numbers(NewAST)),
                         #{file => File,
                           new => CleanNewAST -- CleanOldAST,
                           old => CleanOldAST -- CleanNewAST}
                      end,
                      Original,
                      New),
    [Change
     || Change <- Changes, maps:get(new, Change) /= [] orelse maps:get(old, Change) /= []].

%% @doc Removes line numbers from ASTs to allow for "semantic" comparison
remove_line_numbers(AST) when is_list(AST) ->
    lists:map(fun remove_line_numbers/1, AST);
remove_line_numbers(AST) when is_tuple(AST) ->
    [Type, _Line | Rest] = tuple_to_list(AST),
    list_to_tuple([Type, '#' | remove_line_numbers(Rest)]);
remove_line_numbers(AST) ->
    AST.

remove_known_attrs(AST) ->
    lists:filter(fun is_unknown_attr/1, AST).

is_unknown_attr({attribute, '#', beamoji_translator, _}) ->
    false;
is_unknown_attr({attribute, '#', file, {_, '#'}}) ->
    false;
is_unknown_attr({attribute, '#', compile, {parse_transform, '#'}}) ->
    false;
is_unknown_attr(_) ->
    true.

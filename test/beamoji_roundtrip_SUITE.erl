-module(beamoji_roundtrip_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([roundtrip/1]).

all() ->
    [roundtrip].

init_per_suite(Config) ->
    {ok, OldPath} = file:get_cwd(),
    [{old_path, OldPath} | Config].

end_per_suite(Config) ->
    ok =
        file:set_cwd(
            proplists:get_value(old_path, Config)).

roundtrip(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OriginalASTs = get_asts(DataDir),
    State = initialize_rebar(DataDir),
    {ok, _} = beamoji_prv:do(State),
    [] = diff(OriginalASTs, get_asts(DataDir)),
    ok.

get_asts(DataDir) ->
    lists:map(fun get_ast/1, files(DataDir)).

get_ast(File) ->
    IncludePath =
        filename:join(
            code:lib_dir(beamoji), "include"),
    {ok, Forms} = epp:parse_file(File, [{includes, [IncludePath]}]),
    {File, beamoji_pt:parse_transform(Forms, [])}.

files(DataDir) ->
    filelib:wildcard(
        filename:join(DataDir, "*.erl")).

initialize_rebar(DataDir) ->
    ok = file:set_cwd(DataDir),
    {ok, State1} =
        beamoji:init(
            rebar_state:new()),
    {ok, State2} = rebar_prv_app_discovery:do(State1),
    Files = {files, files(DataDir)},
    State3 = rebar_state:set(State2, format, [Files]),
    rebar_state:command_parsed_args(State3,
                                    {[{translator, beamoji_baseemoji_translator}], something}).

diff(Original, New) ->
    Changes =
        lists:zipwith(fun({File, OldAST}, {File, NewAST}) ->
                         CleanOldAST = remove_line_numbers(OldAST),
                         CleanNewAST = remove_line_numbers(NewAST),
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

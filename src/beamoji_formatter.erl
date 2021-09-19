-module(beamoji_formatter).

-include_lib("kernel/include/file.hrl").

-define(HRL_PATH, "beamoji/include/beamoji.hrl").

-behaviour(rebar3_formatter).

-export([init/2, format_file/3]).

-spec init(rebar3_formatter:opts(), undefined | rebar_state:t()) ->
              beamoji_translator:'🫖'().
init(#{translator := Translator}, _) ->
    beamoji_translator:'🐣'(Translator).

%% @doc Format/Emojify a file.
-spec format_file(file:filename_all(),
                  beamoji_translator:'🫖'(),
                  rebar3_formatter:opts()) ->
                     rebar3_formatter:result().
format_file(File, Translator, Opts) ->
    {ok, AST} = get_ast(File, Translator, Opts),
    Comments = erl_comment_scan:file(File),
    WithComments = erl_recomment:recomment_forms(AST, Comments),
    {Result, Formatted} =
        case ensure_translator_attributes(WithComments, Translator) of
            {ok, NewAST} ->
                rebar_api:info("emojifying ~ts your code with ~p...",
                               [File, beamoji_translator:'🗣'(Translator)]),
                {changed, format(File, NewAST, Opts)};
            {error, not_a_module} ->
                {ok, Original} = file:read_file(File),
                {unchanged, Original};
            {error, translator_mismatch} ->
                rebar_api:warn("~ts was emojified with a different translator."
                               " We can't emojify over it.",
                               [File]),
                {ok, Original} = file:read_file(File),
                {unchanged, Original}
        end,
    _ = maybe_save_file(maps:get(output_dir, Opts), File, Formatted),
    Result.

get_ast(File, Translator, Opts) ->
    DodgerOpts =
        [{scan_opts, [text]},
         no_fail,
         compact_strings,
         {post_fixer, fun(Tokens) -> emojify(Tokens, Translator) end}]
        ++ [parse_macro_definitions || maps:get(parse_macro_definitions, Opts, true)],
    ktn_dodger:parse_file(File, DodgerOpts).

format(File, AST, Opts) ->
    Formatted = default_formatter:format(AST, empty_lines(File), Opts),
    insert_last_line(iolist_to_binary(Formatted)).

empty_lines(File) ->
    {ok, Data} = file:read_file(File),
    List = binary:split(Data, [<<"\n">>], [global, trim]),
    {ok, NonEmptyLineRe} = re:compile("\\S"),
    {Res, _} =
        lists:foldl(fun(Line, {EmptyLines, N}) ->
                       case re:run(Line, NonEmptyLineRe) of
                           {match, _} ->
                               {EmptyLines, N + 1};
                           nomatch ->
                               {[N | EmptyLines], N + 1}
                       end
                    end,
                    {[], 1},
                    List),
    lists:reverse(Res).

maybe_save_file(none, _File, _Formatted) ->
    none;
maybe_save_file(current, File, Formatted) ->
    ok = file:write_file(File, Formatted),
    File;
maybe_save_file(OutputDir, File, Formatted) ->
    OutFile =
        filename:join(
            filename:absname(OutputDir), File),
    ok = filelib:ensure_dir(OutFile),
    {ok, FileInfo} = file:read_file_info(File),
    ok = file:write_file(OutFile, Formatted),
    ok = file:change_mode(OutFile, FileInfo#file_info.mode),
    OutFile.

insert_last_line(Formatted) ->
    {ok, Re} = re:compile("[\n]+$"),
    case re:run(Formatted, Re) of
        {match, _} ->
            re:replace(Formatted, Re, "\n", [{return, binary}]);
        nomatch ->
            <<Formatted/binary, "\n">>
    end.

ensure_translator_attributes(FormList, State) ->
    Nodes = erl_syntax:form_list_elements(FormList),
    Includes =
        [concrete(IncludePath)
         || Node <- Nodes,
            erl_syntax:type(Node) == attribute,
            attr_name(Node) == include_lib,
            IncludePath <- erl_syntax:attribute_arguments(Node)],
    case lists:member(?HRL_PATH, Includes) of
        true ->
            ensure_same_translator(Nodes, beamoji_translator:'🗣'(State));
        false ->
            add_translator_attributes(Nodes, beamoji_translator:'🗣'(State))
    end.

ensure_same_translator(Nodes, Translator) ->
    case [concrete(FoundTranslator)
          || Node <- Nodes,
             erl_syntax:type(Node) == attribute,
             attr_name(Node) == beamoji_translator,
             FoundTranslator <- erl_syntax:attribute_arguments(Node)]
    of
        [Translator | _] ->
            {ok, erl_syntax:form_list(Nodes)};
        _ ->
            {error, translator_mismatch}
    end.

add_translator_attributes(Nodes, Translator) ->
    case lists:splitwith(fun(Node) ->
                            erl_syntax:type(Node) == attribute andalso attr_name(Node) == module
                         end,
                         Nodes)
    of
        {[], _All} ->
            {error, not_a_module};
        {BeforeModule, AfterModule} ->
            Module = lists:last(BeforeModule),
            Pos = erl_syntax:get_pos(Module),
            TranslatorAttr =
                erl_syntax:set_pos(
                    erl_syntax:attribute(
                        erl_syntax:atom(beamoji_translator), [erl_syntax:atom(Translator)]),
                    Pos),
            IncludeLibAttr =
                erl_syntax:set_pos(
                    erl_syntax:attribute(
                        erl_syntax:atom(include_lib), [erl_syntax:string(?HRL_PATH)]),
                    Pos),
            {ok,
             erl_syntax:form_list(BeforeModule ++ [TranslatorAttr, IncludeLibAttr | AfterModule])}
    end.

attr_name(Node) ->
    concrete(erl_syntax:attribute_name(Node)).

%% @doc Macro dodging version of erl_syntax:concrete/1
concrete(Node) ->
    try
        erl_syntax:concrete(Node)
    catch
        _:_ ->
            Node
    end.

emojify(AST, Translator) ->
    case ast_walk:form(AST, fun walker/2, Translator) of
        {AST, _} ->
            no_fix;
        {NewAST, _} ->
            {form, NewAST}
    end.

walker(_Translator, AST = {function, F, '?preprocessor declaration?', _, _}) ->
    %% Don't translate preprocessor macros at all.
    rebar_api:warn("preprocessor declaration: ~p", [F]),
    IdTranslator = beamoji_translator:'🐣'(beamoji_id_translator),
    {AST, IdTranslator};
walker(Translator, AST) ->
    Translate = fun(Atom) -> translate(Translator, Atom) end,
    NewAST = beamoji_utils:'🪄'(Translate, AST),
    {NewAST, Translator}.

translate(Translator, Atom) ->
    case iolist_to_binary(io_lib:format("~p", [Atom])) of
        <<$', _/binary>> -> % a quoted atom
            Atom;
        _ ->
            beamoji_translator:'⏩'(Atom, Translator)
    end.

-module(source_analyzer).

-export([analyze/1]).

-include_lib("kernel/include/logger.hrl").
-include_lib("compiler/src/core_parse.hrl").

analyze(#{file := File, include_paths := IncludePaths}) ->
    ?LOG_INFO("Analyzing: ~s", [File]),

    {ok, SourceAnalysis} = erl_analyzer:analyze(#{file => File, include_paths => IncludePaths}),
    CompAnalysis = fun() ->
        {ok, #{File := Res}} = cerl_analyzer:analyze([File], IncludePaths),
        Res
    end,

    Result =
        erlang_libraries(File, IncludePaths, SourceAnalysis, CompAnalysis) ++
            erlang_ct_suites(File, IncludePaths, SourceAnalysis, CompAnalysis) ++
            erlang_prop_tests(File, IncludePaths, SourceAnalysis, CompAnalysis) ++
            erlang_script(File, IncludePaths, SourceAnalysis, CompAnalysis) ++
            [],

    {ok, Sources} = file:read_file(File),
    SourceHash = str:new(
        io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, Sources))])
    ),
    AstHash = begin
        Ast = erl_analyzer:ast(SourceAnalysis),
        AstBin = str:new(io_lib:format("~p", [Ast])),
        str:new(io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, AstBin))]))
    end,

    #{
        file => File,
        source => #{
            source => Sources,
            source_hash => SourceHash,
            ast_hash => AstHash,
            symbol => <<"All">>
        },
        source_analysis => SourceAnalysis,
        signatures => uniq(Result)
    }.

%%--------------------------------------------------------------------------------------------------
%% Analyzer for Erlang scripts
%%--------------------------------------------------------------------------------------------------

erlang_script(File, IncludePaths, SourceAnalysis, CompAnalysis) ->
    case path:extension(File) of
        {ok, <<".erl">>} -> get_erlang_script(File, IncludePaths, SourceAnalysis, CompAnalysis);
        _ -> []
    end.

get_erlang_script(File, _IncludePaths, SourceAnalysis, _CompAnalysis) ->
    HasMain = lists:any(
        fun
            ({_Mod, main, 1}) -> true;
            (_Fn) -> false
        end,
        erl_analyzer:functions(SourceAnalysis)
    ),

    case HasMain of
        false ->
            [];
        true ->
            [
                #{
                    apps => [kernel],
                    main => path:filename(File),
                    name => File,
                    rule => <<"erlang_script">>
                }
            ]
    end.

%%--------------------------------------------------------------------------------------------------
%% Analyzer for Erlang libraries
%%--------------------------------------------------------------------------------------------------

erlang_libraries(File, _IncludePaths, SourceAnalysis, CompAnalysis) ->
    ModDeps =
        case path:extension(File) of
            {ok, <<".hrl">>} -> [];
            _ -> mods_from_analyses(File, CompAnalysis, SourceAnalysis)
        end,

    TypeModDeps =
        case path:extension(File) of
            % NOTE(@ostera): header files don't actually have type dependencies, but they
            % create type-dependencies in the modules that include them.
            %
            {ok, <<".hrl">>} ->
                [];
            _Ext ->
                TypeModDeps0 = erl_analyzer:dependency_type_modules(SourceAnalysis),
                TypeModDeps1 =
                    case erl_stdlib:file_to_module(File) of
                        {ok, CurrMod} -> [Mod || Mod <- TypeModDeps0, Mod =/= CurrMod];
                        _ -> TypeModDeps0
                    end,
                skip_std(uniq(TypeModDeps1))
        end,

    IncludeDeps = includes_from_analyses(File, CompAnalysis, SourceAnalysis),

    [
        #{
            name => File,
            srcs => [path:filename(File)],
            includes => IncludeDeps,
            type_modules => TypeModDeps,
            modules => ModDeps,
            parse_transforms => erl_analyzer:parse_transforms(SourceAnalysis),
            rule => <<"erlang_library">>
        }
    ].

%%--------------------------------------------------------------------------------------------------
%% Analyzer for Erlang Common Test Suites
%%--------------------------------------------------------------------------------------------------

erlang_ct_suites(File, IncludePaths, SourceAnalysis, CompAnalysis) ->
    case str:ends_with(path:filename(File), "_SUITE.erl") of
        true -> get_ct_cases(File, IncludePaths, SourceAnalysis, CompAnalysis);
        false -> []
    end.

get_ct_cases(File, _IncludePaths, SourceAnalysis, CompAnalysisFn) ->
    CompAnalysis = CompAnalysisFn(),

    Cases =
        case CompAnalysis of
            #{error := Err} ->
                ?LOG_ERROR("Error analyzing CommonTest cases for ~p:\n~p\n", [File, Err]),
                [];
            _ ->
                AllFn = cerl_analyzer:function(CompAnalysis, {all, 0}),
                cerl_trees:fold(fun extract_cases/2, [], AllFn)
        end,

    ?LOG_INFO("Found ~p common test cases\n", [length(Cases)]),

    ModDeps = mods_from_analyses(File, CompAnalysis, SourceAnalysis),
    IncludeDeps = includes_from_analyses(File, CompAnalysis, SourceAnalysis),

    [
        #{
            name => <<File/binary, ":", (erlang:atom_to_binary(Case, utf8))/binary, "/1">>,
            test => path:filename(File),
            includes => IncludeDeps,
            modules => ModDeps,
            cases => [Case],
            rule => <<"erlang_test">>
        }
     || Case <- Cases, erlang:is_atom(Case)
    ].

extract_cases(Tree, Acc) -> extract_cases(cerl:type(Tree), Tree, Acc).
extract_cases(literal, #c_literal{} = Tree, Acc) ->
    case cerl:is_c_list(Tree) of
        true ->
            Val = Tree#c_literal.val,
            uniq(Val ++ Acc);
        false ->
            Acc
    end;
extract_cases(_Type, _Tree, Acc) ->
    Acc.

%%--------------------------------------------------------------------------------------------------
%% Analyzer for Erlang Property Tests
%%--------------------------------------------------------------------------------------------------

erlang_prop_tests(File, IncludePaths, SourceAnalysis, CompAnalysis) ->
    case str:begins_with(path:filename(File), "prop_") of
        true -> get_prop_tests(File, IncludePaths, SourceAnalysis, CompAnalysis);
        false -> []
    end.

get_prop_tests(File, _IncludePaths, SourceAnalysis, CompAnalysisFn) ->
    CompAnalysis = CompAnalysisFn(),

    Properties0 =
        case CompAnalysis of
            #{error := _} ->
                [Fn || {_Mod, Fn, _Arity} <- erl_analyzer:functions(SourceAnalysis)];
            _ ->
                maps:keys(cerl_analyzer:functions(SourceAnalysis))
        end,

    Properties = lists:filter(
        fun(Fn) ->
            FnName = erlang:atom_to_binary(Fn, utf8),
            case string:prefix(FnName, <<"prop_">>) of
                nomatch -> false;
                _ -> true
            end
        end,
        Properties0
    ),

    ModDeps = mods_from_analyses(File, CompAnalysis, SourceAnalysis),
    IncludeDeps = includes_from_analyses(File, CompAnalysis, SourceAnalysis),

    [
        #{
            name => <<File/binary, ":", (erlang:atom_to_binary(Prop, utf8))/binary>>,
            test => path:filename(File),
            includes => IncludeDeps,
            modules => ModDeps,
            props => [Prop],
            rule => <<"erlang_proper_test">>
        }
     || Prop <- Properties
    ].

%%--------------------------------------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------------------------------------

mods_from_analyses(File, CompAnalysis, SourceAnalysis) ->
    ModDeps0 =
        [
            Mod
         || Mod <- cerl_analyzer:dependency_modules(CompAnalysis),
            erl_stdlib:is_user_module(Mod)
        ] ++ erl_analyzer:dependency_modules(SourceAnalysis),
    skip_std(uniq([Mod || Mod <- ModDeps0, Mod =/= File])).

includes_from_analyses(File, CompAnalysis, SourceAnalysis) ->
    IncludeDeps0 = uniq(
        cerl_analyzer:dependency_includes(CompAnalysis) ++
            erl_analyzer:dependency_includes(SourceAnalysis)
    ),
    [Hrl || Hrl <- IncludeDeps0, erl_stdlib:is_user_include(Hrl), Hrl =/= File].

skip_std(Mods) ->
    lists:filtermap(
        fun
            (Mod) when is_atom(Mod) ->
                case erl_stdlib:is_user_module(Mod) of
                    true -> {true, Mod};
                    false -> false
                end;
            (Mod) ->
                {true, Mod}
        end,
        Mods
    ).

uniq([]) -> [];
uniq([X]) -> [X];
uniq(Xs) -> sets:to_list(sets:from_list(Xs, [{version, 2}])).

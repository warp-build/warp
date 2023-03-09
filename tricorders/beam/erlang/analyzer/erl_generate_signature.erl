-module(erl_generate_signature).

-include_lib("kernel/include/logger.hrl").
-include_lib("compiler/src/core_parse.hrl").

-export([generate/1]).

generate(GenReq = #{file := File, include_paths := IncludePaths, code_paths := CodePaths}) ->
    {ok, Ast} = erl_ast:parse_file(File, IncludePaths),
    case erl_ast:has_epp_include_errors(Ast) of
        % NOTE(@ostera): if we can quickly determine that we are missing
        % includes, request those missing dependencies first. At this time we can
        % also get the parse transforms that we can statically identify.
        true ->
            {ok,
                {missing_dependencies, #{
                    includes => erl_ast:get_missing_includes(Ast),
                    parse_transforms => erl_ast:get_parse_transforms(Ast)
                }}};
        false ->
            case do_gen(Ast, GenReq) of
                {ok, Result} ->
                    {ok, {completed, Result}};
                {error, Error} ->
                    {ok,
                        {missing_dependencies, #{
                            includes => erl_ast:get_missing_includes(Ast),
                            parse_transforms => erl_ast:get_parse_transforms(Ast)
                        }}}
            end
    end.

do_gen(Ast, #{file := File, include_paths := IncludePaths, code_paths := CodePaths}) ->
    {ok, SourceAnalysis} = erl_analyzer:analyze(#{
        ast => Ast,
        file => File,
        include_paths => IncludePaths
    }),

    {ok, Ext} = path:extension(File),
    do_gen(Ext, File, IncludePaths, CodePaths, SourceAnalysis).

% Analyzers for erlang header code
do_gen(<<".hrl">>, File, _IncludePaths, _CodePaths, SourceAnalysis) ->
    {ok, erlang_header_library(File, SourceAnalysis)};
% Analyzers for erlang module code
do_gen(<<".erl">>, File, IncludePaths, CodePaths, SourceAnalysis) ->
    % NOTE(@ostera): we'll first of all try to compile the module and analyze its Core Erlang repr.
    % as this helps us find missing parse transforms.
    case
        cerl_analyzer:analyze(#{
            file => File,
            include_paths => IncludePaths,
            code_paths => CodePaths
        })
    of
        {ok, CompAnalysis} ->
            {ok, CurrMod} = erl_stdlib:file_to_module(File),
            Signatures =
                erlang_library(CurrMod, File, SourceAnalysis) ++
                    erlang_ct_suites(CurrMod, File, SourceAnalysis, CompAnalysis),
            {ok, Signatures};
        {error, Error} ->
            {error, Error}
    end.

%%--------------------------------------------------------------------------------------------------
%% Signature Generator for Erlang Libraries
%%--------------------------------------------------------------------------------------------------

erlang_library(CurrMod, File, SourceAnalysis) ->
    % NOTE(@ostera): get all the modules that we have calls towards that aren't the current module
    ModDeps = [Mod || Mod <- erl_analyzer:dependency_modules(SourceAnalysis), Mod =/= File],

    % NOTE(@ostera): get all the modules that we use types from that aren't the current module
    TypeModDeps = erl_analyzer:dependency_type_modules(SourceAnalysis),

    % NOTE(@ostera): get all the includes that are user includes (so skip standard Erlang/OTP headers)
    IncludeDeps = [
        Hrl
     || Hrl <- erl_analyzer:dependency_includes(SourceAnalysis),
        erl_stdlib:is_user_include(Hrl)
    ],

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
%% Signature Generator for Erlang Header Libraries
%%--------------------------------------------------------------------------------------------------

erlang_header_library(File, SourceAnalysis) ->
    % NOTE(@ostera): get all the includes that are user includes (so skip standard Erlang/OTP headers)
    % that are also not the current file.
    IncludeDeps = [
        Hrl
     || Hrl <- erl_analyzer:dependency_includes(SourceAnalysis),
        erl_stdlib:is_user_include(Hrl),
        Hrl =/= File
    ],

    [
        #{
            name => File,
            srcs => [path:filename(File)],
            includes => IncludeDeps,
            parse_transforms => erl_analyzer:parse_transforms(SourceAnalysis),
            rule => <<"erlang_library">>
        }
    ].

%%--------------------------------------------------------------------------------------------------
%% Signature Generator for Erlang CommonTest Suites
%%--------------------------------------------------------------------------------------------------

erlang_ct_suites(CurrMod, File, SourceAnalysis, CompAnalysis) ->
    case str:ends_with(path:filename(File), "_SUITE.erl") of
        true -> get_ct_cases(CurrMod, File, SourceAnalysis, CompAnalysis);
        false -> []
    end.

get_ct_cases(CurrMod, File, SourceAnalysis, CompAnalysisFn) ->
    % AllFn = cerl_analyzer:function(CompAnalysis, {all, 0}),
    % Cases = cerl_trees:fold(fun extract_cases/2, [], AllFn),

    % NOTE(@ostera): we can call `CurrMod:all()` and get the evaluated list of tests!
    Cases = CurrMod:all(),

    ?LOG_INFO("Found ~p common test cases\n: ~p", [length(Cases), Cases]),

    % NOTE(@ostera): get all the modules that we have calls towards that aren't the current module
    ModDeps = [Mod || Mod <- erl_analyzer:dependency_modules(SourceAnalysis), Mod =/= File],

    % NOTE(@ostera): get all the modules that we use types from that aren't the current module
    TypeModDeps = erl_analyzer:dependency_type_modules(SourceAnalysis),

    % NOTE(@ostera): get all the includes that are user includes (so skip standard Erlang/OTP headers)
    IncludeDeps = [
        Hrl
     || Hrl <- erl_analyzer:dependency_includes(SourceAnalysis),
        erl_stdlib:is_user_include(Hrl)
    ],

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
%% Helpers
%%--------------------------------------------------------------------------------------------------

uniq([]) -> [];
uniq([X]) -> [X];
uniq(Xs) -> sets:to_list(sets:from_list(Xs, [{version, 2}])).

-module(source_analyzer).

-export([analyze/4]).
-export([analyze_one/4]).

-include_lib("kernel/include/logger.hrl").
-include_lib("compiler/src/core_parse.hrl").

analyze(Files, ModMap, IgnoreModMap, IncludePaths) ->
  Result = [ analyze_one(File, ModMap, IgnoreModMap, IncludePaths) || File <- Files ],
  ?LOG_INFO("Analyzed ~p files successfully.", [length(Files)]),
  {ok, Result}.

analyze_one(File, ModMap, IgnoreModMap, IncludePaths) ->
  ?LOG_INFO("Analyzing: ~s", [File]),

	{ok, #{ File := SourceAnalysis}} = erl_analyzer:analyze([File], ModMap, IncludePaths),
  CompAnalysis = fun () ->
                     {ok, #{ File := Res }} = cerl_analyzer:analyze([File], IncludePaths),
                     Res
                 end,

  Result =
    erlang_libraries(File, ModMap, IgnoreModMap, IncludePaths, SourceAnalysis, CompAnalysis)
    ++ erlang_ct_suites(File, ModMap, IgnoreModMap, IncludePaths, SourceAnalysis, CompAnalysis)
    ++ erlang_prop_tests(File, ModMap, IgnoreModMap, IncludePaths, SourceAnalysis, CompAnalysis)
    ++ erlang_script(File, ModMap, IgnoreModMap, IncludePaths, SourceAnalysis, CompAnalysis)
    ++ [],

  {ok, Sources} = file:read_file(File),
  SourceHash = str:new(io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, Sources))])),
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
    signatures => uniq(Result)
  }.

%%--------------------------------------------------------------------------------------------------
%% Analyzer for Erlang scripts
%%--------------------------------------------------------------------------------------------------

erlang_script(File, ModMap, IgnoreModMap, IncludePaths, SourceAnalysis, CompAnalysis) ->
  case path:extension(File) of
    <<".erl">> -> get_erlang_script(File, ModMap, IgnoreModMap, IncludePaths, SourceAnalysis, CompAnalysis);
    _ -> []
  end.

get_erlang_script(File, ModMap, IgnoreModMap, _IncludePaths, SourceAnalysis, CompAnalysis) ->
  ModDeps = mods_from_analyses(File, ModMap, IgnoreModMap, CompAnalysis, SourceAnalysis),
  IncludeDeps = includes_from_analyses(File, ModMap, CompAnalysis, SourceAnalysis),

  HasMain = lists:any(fun
                        ({_Mod, main, 1}) -> true;
                        (_Fn) -> false
                      end, erl_analyzer:functions(SourceAnalysis)),

  case HasMain of
    false -> [];
    true -> [#{
               apps => [kernel],
               deps => deps_to_labels(IncludeDeps), 
               main => path:filename(File),
               name => File,
               rule => <<"erlang_script">>,
               runtime_deps => deps_to_labels(ModDeps)
              }]
  end.


%%--------------------------------------------------------------------------------------------------
%% Analyzer for Erlang libraries
%%--------------------------------------------------------------------------------------------------

erlang_libraries(File, ModMap, IgnoreModMap, _IncludePaths, SourceAnalysis, CompAnalysis) ->
  ModDeps = case path:extension(File) of
              <<".hrl">> -> [];
              _ -> mods_from_analyses(File, ModMap, IgnoreModMap, CompAnalysis, SourceAnalysis)
            end,
  IncludeDeps = includes_from_analyses(File, ModMap, CompAnalysis, SourceAnalysis),

  [#{
    name => File,
    srcs => [path:filename(File)],
    runtime_deps => [],
    deps => deps_to_labels(IncludeDeps ++ ModDeps), 
    rule => <<"erlang_library">>
   }].

%%--------------------------------------------------------------------------------------------------
%% Analyzer for Erlang Common Test Suites
%%--------------------------------------------------------------------------------------------------

erlang_ct_suites(File, ModMap, IgnoreModMap, IncludePaths, SourceAnalysis, CompAnalysis) ->
  case str:ends_with(path:filename(File), "_SUITE.erl") of
    true -> get_ct_cases(File, ModMap, IgnoreModMap, IncludePaths, SourceAnalysis, CompAnalysis);
    false -> []
  end.

get_ct_cases(File, ModMap, IgnoreModMap, _IncludePaths, SourceAnalysis, CompAnalysisFn) ->
  CompAnalysis = CompAnalysisFn(),

  Cases = case CompAnalysis of
            #{ error := Err } ->
              ?LOG_ERROR("Error analyzing CommonTest cases for ~p:\n~p\n", [File, Err]),
              [];
            _ ->
              AllFn = cerl_analyzer:function(CompAnalysis, {all,0}),
              cerl_trees:fold(fun extract_cases/2, [], AllFn)
          end,

  ?LOG_INFO("Found ~p common test cases\n", [length(Cases)]),

  ModDeps = mods_from_analyses(File, ModMap, IgnoreModMap, CompAnalysis, SourceAnalysis),
  IncludeDeps = includes_from_analyses(File, ModMap, CompAnalysis, SourceAnalysis),

  [#{
    name => <<File/binary, ":", (erlang:atom_to_binary(Case, utf8))/binary, "/1">>,
    test => path:filename(File),
    runtime_deps => [],
    deps => deps_to_labels(IncludeDeps ++ ModDeps), 
    cases => [Case],
    rule => <<"erlang_test">>
   } || Case <- Cases, erlang:is_atom(Case)].

extract_cases(Tree, Acc) -> extract_cases(cerl:type(Tree), Tree, Acc).
extract_cases(literal, #c_literal{}=Tree, Acc) ->
  case cerl:is_c_list(Tree) of
    true ->
      Val = Tree#c_literal.val,
      uniq(Val ++ Acc);
    false -> Acc
  end;
extract_cases(_Type, _Tree, Acc) -> Acc.

%%--------------------------------------------------------------------------------------------------
%% Analyzer for Erlang Property Tests
%%--------------------------------------------------------------------------------------------------

erlang_prop_tests(File, ModMap, IgnoreModMap, IncludePaths, SourceAnalysis, CompAnalysis) ->
  case str:begins_with(path:filename(File), "prop_") of
    true -> get_prop_tests(File, ModMap, IgnoreModMap, IncludePaths, SourceAnalysis, CompAnalysis);
    false -> []
  end.

get_prop_tests(File, ModMap, IgnoreModMap, _IncludePaths, SourceAnalysis, CompAnalysisFn) ->
  CompAnalysis = CompAnalysisFn(),

  Properties = case CompAnalysis of 
                 #{ error := _ } ->
                   [ Fn || {_Mod, Fn, _Arity} <- erl_analyzer:functions(SourceAnalysis) ];
                 _ ->
                   maps:keys(cerl_analyzer:functions(SourceAnalysis))
               end,

  ModDeps = mods_from_analyses(File, ModMap, IgnoreModMap, CompAnalysis, SourceAnalysis),
  IncludeDeps = includes_from_analyses(File, ModMap, CompAnalysis, SourceAnalysis),

  [#{
    name => <<File/binary, ":", (erlang:atom_to_binary(Prop, utf8))/binary>>,
    test => path:filename(File),
    runtime_deps => [],
    deps => deps_to_labels(IncludeDeps ++ ModDeps), 
    props => [Prop],
    rule => <<"erlang_proper_test">>
   } || Prop <- Properties].


%%--------------------------------------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------------------------------------


mods_from_analyses(File, ModMap, IgnoreModMap, CompAnalysis, SourceAnalysis) ->
  ModDeps0 = [ maps:get(Mod, ModMap, Mod)
               || Mod <- cerl_analyzer:dependency_modules(CompAnalysis), 
                  erl_stdlib:is_user_module(Mod),
                  not sets:is_element(Mod, IgnoreModMap)
             ] ++ erl_analyzer:dependency_modules(SourceAnalysis),
  skip_std(uniq([ Mod || Mod <- ModDeps0, Mod =/= File ])).

includes_from_analyses(File, ModMap, CompAnalysis, SourceAnalysis) ->
  IncludeDeps0 = uniq(cerl_analyzer:dependency_includes(CompAnalysis) ++ erl_analyzer:dependency_includes(SourceAnalysis)),
  IncludeDeps1 = [ begin
                     case path:contains(Hrl, <<"include">>) of
                       true ->
                         SubPath = path:nth_tail(Hrl, 3),
                         maps:get(SubPath, ModMap, Hrl);
                       _ ->
                         maps:get(Hrl, ModMap, Hrl)
                     end
                   end
                   || Hrl <- IncludeDeps0,
                      erl_stdlib:is_user_include(Hrl),
                      path:extension(Hrl) == <<".hrl">>,
                      Hrl =/= File
                 ],
  skip_std(uniq([ Path || Path <- IncludeDeps1, Path =/= File ])).

deps_to_labels(Deps) -> uniq(lists:map(fun dep_to_label/1, Deps)).

dep_to_label(Dep) when is_atom(Dep) -> erlang:atom_to_binary(Dep);
dep_to_label(Dep = <<"https://", _Url/binary>>) -> Dep;
dep_to_label(Dep = <<"http://", _Url/binary>>) -> Dep;
dep_to_label(Dep) -> <<"//", Dep/binary>>.

skip_std(Mods) ->
  lists:filtermap(fun
                    (Mod) when is_atom(Mod) ->
                      case erl_stdlib:is_user_module(Mod) of
                        true -> {true, Mod};
                        false -> false
                      end;
                    (Mod) -> {true, Mod}
                  end, Mods).

uniq([]) -> [];
uniq([X]) -> [X];
uniq(Xs) -> sets:to_list(sets:from_list(Xs, [{version, 2}])).

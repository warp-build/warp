-module(source_analyzer).

-export([analyze/4]).

-include_lib("kernel/include/logger.hrl").
-include_lib("compiler/src/core_parse.hrl").

analyze(Files, ModMap, IgnoreModMap, IncludePaths) ->
  Result = lists:foldl(fun (File, R) ->
                           {K, V} = do_analyze(File, ModMap, IgnoreModMap, IncludePaths),
                           maps:put(K, V, R)
                       end, #{}, Files),
  ?LOG_INFO("Analyzed ~p files successfully.", [length(Files)]),
  {ok, Result}.

do_analyze(File, ModMap, IgnoreModMap, IncludePaths) ->
  ?LOG_INFO("Analyzing: ~s", [File]),

	{ok, #{ File := SourceAnalysis}} = erl_analyzer:analyze([File], ModMap, IncludePaths),
	{ok, #{ File := CompAnalysis}} = cerl_analyzer:analyze([File], IncludePaths),

  Result =
    erlang_libraries(File, ModMap, IgnoreModMap, IncludePaths, SourceAnalysis, CompAnalysis)
    ++ erlang_ct_suites(File, ModMap, IgnoreModMap, IncludePaths, SourceAnalysis, CompAnalysis)
    ++ erlang_prop_tests(File, ModMap, IgnoreModMap, IncludePaths, SourceAnalysis, CompAnalysis)
    ++ [],

  {path:add_extension(File, "wsig"), uniq(Result)}.

%%--------------------------------------------------------------------------------------------------
%% Analyzer for Erlang libraries
%%--------------------------------------------------------------------------------------------------

erlang_libraries(File, ModMap, IgnoreModMap, _IncludePaths, SourceAnalysis, CompAnalysis) ->

  ModDeps0 = [ maps:get(Mod, ModMap)
               || Mod <- cerl_analyzer:dependency_modules(CompAnalysis), 
                  erl_stdlib:is_user_module(Mod),
                  not sets:is_element(Mod, IgnoreModMap)
             ]
              ++ erl_analyzer:dependency_modules(SourceAnalysis),
  ModDeps = skip_std(uniq(ModDeps0)),

  IncludeDeps0 = [ maps:get(Hrl, ModMap, Hrl) || Hrl <- cerl_analyzer:dependency_includes(CompAnalysis)
                                            ++ erl_analyzer:dependency_includes(SourceAnalysis),
                                      erl_stdlib:is_user_include(Hrl),
                                      path:extension(Hrl) == <<".hrl">> ],
  IncludeDeps = skip_std(uniq(IncludeDeps0)),

  [#{
    name => path:filename(File),
    srcs => [path:filename(File)],
    deps => lists:map(fun dep_to_label/1, ModDeps ++ IncludeDeps),
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

get_ct_cases(File, ModMap, IgnoreModMap, IncludePaths, SourceAnalysis, CompAnalysis) ->
  Cases = case CompAnalysis of
            #{ error := Err } ->
              io:format("Include Paths: ~p\n", [IncludePaths]),
              io:format("Error analyzing CommonTest cases for ~p:\n~p\n", [File, Err]),
              timer:sleep(100),
              erlang:halt(),
              [];
            _ ->
              AllFn = cerl_analyzer:function(CompAnalysis, {all,0}),
              cerl_trees:fold(fun extract_cases/2, [], AllFn)
          end,

  ModDeps0 = [ maps:get(Mod, ModMap)
               || Mod <- cerl_analyzer:dependency_modules(CompAnalysis), 
                  erl_stdlib:is_user_module(Mod),
                  not sets:is_element(Mod, IgnoreModMap)
             ]
              ++ erl_analyzer:dependency_modules(SourceAnalysis),
  ModDeps = skip_std(uniq(ModDeps0)),

  IncludeDeps0 = [ maps:get(Hrl, ModMap, Hrl) || Hrl <- cerl_analyzer:dependency_includes(CompAnalysis)
                                            ++ erl_analyzer:dependency_includes(SourceAnalysis),
                                      erl_stdlib:is_user_include(Hrl),
                                      path:extension(Hrl) == <<".hrl">> ],
  IncludeDeps = skip_std(uniq(IncludeDeps0)),

  [#{
    name => Case,
    test => path:filename(File),
    deps => lists:map(fun dep_to_label/1, ModDeps ++ IncludeDeps),
    cases => [Case],
    rule => <<"erlang_test">>
   } || Case <- Cases, erlang:is_binary(Case)].

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

get_prop_tests(File, ModMap, IgnoreModMap, IncludePaths, SourceAnalysis, CompAnalysis) ->
  Properties = case CompAnalysis of 
                 #{ error := _ } ->
                   [ Fn || {_Mod, Fn, _Arity} <- erl_analyzer:functions(SourceAnalysis) ];
                 _ ->
                   maps:keys(cerl_analyzer:functions(SourceAnalysis))
               end,

  % TODO(@ostera): use the cases above to extract the external calls each test case makes, and use
  % _those_ as the module listings for each case.

  ModDeps0 = [ maps:get(Mod, ModMap)
               || Mod <- cerl_analyzer:dependency_modules(CompAnalysis), 
                  erl_stdlib:is_user_module(Mod),
                  not sets:is_element(Mod, IgnoreModMap)
             ]
              ++ erl_analyzer:dependency_modules(SourceAnalysis),
  ModDeps = skip_std(uniq(ModDeps0)),

  IncludeDeps0 = [ maps:get(Hrl, ModMap, Hrl) || Hrl <- cerl_analyzer:dependency_includes(CompAnalysis)
                                                         ++ erl_analyzer:dependency_includes(SourceAnalysis),
                                                 erl_stdlib:is_user_include(Hrl),
                                                 path:extension(Hrl) == <<".hrl">>,
                                                 Hrl =/= <<"proper/include/proper.hrl">> ],
  IncludeDeps = skip_std(uniq(IncludeDeps0)),

  [#{
    name => Prop,
    test => path:filename(File),
    deps => lists:map(fun dep_to_label/1, [File] ++ ModDeps ++ IncludeDeps),
    props => [Prop],
    rule => <<"erlang_proper_test">>
   } || Prop <- Properties].


%%--------------------------------------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------------------------------------

dep_to_label(Dep) when is_atom(Dep) -> erlang:atom_to_binary(Dep);
dep_to_label(Dep = <<"https://", _Url/binary>>) -> Dep;
dep_to_label(Dep = <<"http://", _Url/binary>>) -> Dep;
dep_to_label(Dep) -> <<"//", (path:dirname(Dep))/binary, ":", (path:filename(Dep))/binary>>.

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

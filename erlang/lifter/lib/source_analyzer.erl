-module(source_analyzer).

-export([analyze/4]).

-include_lib("kernel/include/logger.hrl").

analyze(Files, ModMap, IgnoreModMap, IncludePaths) ->
  Result = lists:foldl(fun (File, R) ->
                           {K, V} = do_analyze(File, ModMap, IgnoreModMap, IncludePaths),
                           maps:put(K, V, R)
                       end, #{}, Files),
  ?LOG_INFO("Analyzed ~s files successfully.", [length(Files)]),
  {ok, Result}.

do_analyze(File, ModMap, IgnoreModMap, IncludePaths) ->
  ?LOG_INFO("Analyzing: ~s", [File]),

	{ok, #{ File := SourceAnalysis}} = erl_analyzer:analyze([File], ModMap, IncludePaths),

	{ok, #{ File := CompAnalysis}} = cerl_analyzer:analyze([File], IncludePaths),

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

  Result = #{ 
      rule => <<"erlang_library">>,
      srcs => [File],
      deps => ModDeps ++ IncludeDeps
   },

  {path:add_extension(File, "wsig"), Result}.

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

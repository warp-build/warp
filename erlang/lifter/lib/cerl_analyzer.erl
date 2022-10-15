%% @doc The `cerl_analyzer` analyzes a file by compiling it to Core Erlang and
%% traversing the resulting Core Trees.
%%
-module(cerl_analyzer).

-include_lib("kernel/include/logger.hrl").
-include_lib("compiler/src/core_parse.hrl").

-export([analyze/1]).
-export([analyze/2]).

-export([function/2]).
-export([dependency_modules/1]).
-export([dependency_includes/1]).

-export_type([mod_desc/0]).
-export_type([err/0]).

-opaque mod_desc() :: #{
                        name => none | atom(),
                        path => binary(),
                        local_calls => [],
                        external_calls => [],
                        exports => [],
                        type_exports => [],
                        includes => [],
                        attrs => [],
                        ast => []
                       }.

-type err() :: {compilation_error, term()}.

-type opts() :: #{ compiler_opts => [atom()] }.

%%--------------------------------------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------------------------------------
%%
-spec function(mod_desc(), {atom(), integer()}) -> term().
function(#{ functions := Fn }, FA) -> maps:get(FA, Fn()).

-spec dependency_modules(mod_desc()) -> [atom() | binary()].
dependency_modules(#{ external_calls := ExtCalls }) ->
  uniq([ M || #{ calls := Calls } <- ExtCalls, {M,_F,_A} <- Calls ]);
dependency_modules(_) -> [].

-spec dependency_includes(mod_desc()) -> [atom() | binary()].
dependency_includes(#{ includes := Hdrs }) -> uniq(Hdrs);
dependency_includes(_) -> [].

-spec analyze([path:t()]) -> [mod_desc()].
analyze(Paths) ->
  #{ sources := Sources, headers := Headers } = source_tagger:tag(Paths),

  HeaderDirs = [ {i, binary:bin_to_list(filename:dirname(H))} || H <- Headers ],
  IncludePaths = uniq(HeaderDirs),
	analyze(Sources, IncludePaths).

analyze(Sources, IncludePaths) ->
  % NOTE(@ostera): before we analyze the sources, we will create a temporary output directory
  % where we will save all the .beam files as we compile them, so they become available for other
  % modules that need them.
  %
  % This is important to make parse_transforms work correctly.
  %
  {ok, OutDir} = tempdir:new(),
  true = code:add_path(path:to_string(OutDir)),

  Opts = #{
           compiler_opts => [{outdir, path:to_string(OutDir)} | IncludePaths],
           include_paths => IncludePaths
          },
  Results0 = lists:map(fun (Source) -> {Source, do_analyze(Source, Opts)} end, Sources),
  Results = maps:from_list(Results0), 

  _ = file:del_dir(OutDir),

  {ok, Results}.


-spec do_analyze(path:t(), opts()) -> result:t(mod_desc(), err()).
do_analyze(Path, #{ compiler_opts := CompileOpts, include_paths := IncludePaths }) when is_binary(Path) ->
  ?LOG_INFO("(CoreErl) Analyzing: ~s", [Path]),
  ParseTrans = find_required_transforms(Path, IncludePaths),

  % Compile Sources into AST
  case do_compile(Path, CompileOpts ++ ParseTrans) of
    {ok, Mod, Core} ->
			Tree = cerl:from_records(Core),
			Attrs = cerl:module_attrs(Core),

			#{
				name => Mod,
				path => Path,
				local_calls => local_calls(Mod, Tree),
				external_calls => external_calls(Mod, Tree),
				exports => exports(Mod, Tree),
				type_exports => type_exports(Mod, Attrs),
				includes => includes(Mod, Attrs),
        functions => fun () -> functions(Mod, Tree) end
			 };

    {error, Reasons, Other} ->
      #{ path => Path,
         error => clean_compile_error(Reasons, Other) }
  end.

find_required_transforms(Path, IncludePaths) -> 
  {ok, Forms} = erl_ast:parse_file(Path, IncludePaths),
  lists:filtermap(
    fun
      ({attribute, _, compile, {parse_transform, Mod}}) -> {true, {parse_transform, Mod}};
      (_) -> false
    end,
    Forms
   ).
  
do_compile(Path, CompileOpts) ->
  % FIXME(@ostera): split this so we compile only once, and then manually write the file.
  File = path:to_string(Path),
  Opts = CompileOpts ++ default_compile_opts(),
  compile:noenv_file(File, Opts),
  compile:noenv_file(File, [binary, to_core] ++ Opts).

default_compile_opts() ->
  [no_copt, return_errors, no_inline, strict_record_tests,
   strict_record_updates, no_spawn_compiler_process ].

clean_compile_error([{Path, [{none, compile, {undef_parse_transform, Mod}}]}], _Other) ->
  #{ kind => missing_parse_transform,
     path => binary:list_to_bin(Path),
     transform_name => Mod
   };

clean_compile_error(Reasons, Other) ->
  #{ kind => compilation_error,
     reasons => Reasons,
     other => Other }.

functions(_Mod, Tree) ->
  Defs = cerl:module_defs(Tree),
  maps:from_list(lists:foldl(fun (AST, Acc) -> fold_function(AST, Acc) end, [], Defs)).

fold_function({Var, Function}, Acc) ->
  FunName = cerl:fname_id(Var),
  Arity = cerl:fname_arity(Var),
  case FunName of
    module_info -> Acc;
    _ -> [ {{FunName, Arity}, Function} | Acc]
  end.

includes(_Mod, Attrs) ->
  Includes = [cerl:concrete(L2) || {L1, L2} <- Attrs,
                                   cerl:is_literal(L1), cerl:is_literal(L2),
                                   cerl:concrete(L1) =:= file ],
  uniq([ binary:list_to_bin(Path) || {Path, _LOC} <- lists:flatten(Includes) ]).

type_exports(Mod, Attrs) ->
  ExpTypes1 = [cerl:concrete(L2) || {L1, L2} <- Attrs,
                                    cerl:is_literal(L1),
                                    cerl:is_literal(L2),
                                    cerl:concrete(L1) =:= 'export_type'],
  ExpTypes2 = lists:flatten(ExpTypes1),
  uniq([mfa(Mod, F, A) || {F, A} <- ExpTypes2]).

exports(Mod, Tree) ->
  Exports1 = cerl:module_exports(Tree),
  Exports2 = [cerl:var_name(V) || V <- Exports1],
  uniq([ mfa(Mod, F, A) || {F, A} <- Exports2, F =/= module_info ]).

local_calls(_Mod, _Tree) ->
  % Local-module calls
  % {LabeledTree, _} = cerl_trees:label(Tree),
  % {_Deps, _Esc, _Calls, _Letrecs} = dialyzer_dep:analyze(LabeledTree),
  [].

external_calls(Mod, Tree) ->
  Defs = cerl:module_defs(Tree),
  ExtCalls = lists:foldl(fun (AST, Acc) -> fold_external(Mod, AST, Acc) end, [], Defs),
  lists:flatten(ExtCalls).

fold_external(Mod, {Var, Function}, Acc) ->
  FunName = cerl:fname_id(Var),
  Arity = cerl:fname_arity(Var),
  case FunName of
    module_info -> Acc;
    _ ->
      Edges = uniq(cerl_trees:fold(fun extract_call/2, [], Function)),
      MFA = mfa(Mod, FunName, Arity),
      [ #{ mfa => MFA, calls => Edges } |Acc]
  end.

extract_call(Tree, Acc) -> extract_call(cerl:type(Tree), Tree, Acc).
extract_call(call, Tree, Acc) ->
  M = cerl:call_module(Tree),
  F = cerl:call_name(Tree),
  A = length(cerl:call_args(Tree)),
	maybe_extract_call(M, F, A, Acc);
extract_call(_, _, Acc) -> Acc.

maybe_extract_call(#c_literal{} = Mod, #c_literal{} = FnName, A, Acc) ->
  M = cerl:atom_val(Mod),
  F = cerl:atom_val(FnName),
  [mfa(M, F, A)|Acc];
maybe_extract_call(_, _, _, Acc) -> Acc.

mfa(M, F, A) -> {M, F, A}.

uniq([]) -> [];
uniq([X]) -> [X];
uniq(Xs) -> lists:flatten(sets:to_list(sets:from_list(Xs, [{version, 2}]))).

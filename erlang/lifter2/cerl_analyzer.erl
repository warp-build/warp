%% @doc The `cerl_analyzer` analyzes a file by compiling it to Core Erlang and
%% traversing the resulting Core Trees.
%%
-module(cerl_analyzer).

-include_lib("kernel/include/logger.hrl").
-include_lib("compiler/src/core_parse.hrl").

-export([analyze/1]).
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

outdir() ->
  "/tmp/_warp_erlang_lifter_tmp/run"
  ++ erlang:integer_to_list(erlang:system_time())
  ++ erlang:integer_to_list(erlang:monotonic_time()).

%%--------------------------------------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------------------------------------

-spec analyze([path:t()]) -> [mod_desc()].
analyze(Paths) ->
  #{ sources := Sources, headers := Headers } = tag_files(Paths),

  HeaderDirs = [ {i, binary:bin_to_list(filename:dirname(H))} || H <- Headers ],
  IncludeDirs = uniq(HeaderDirs),

  % NOTE(@ostera): before we analyze the sources, we will create a temporary output directory
  % where we will save all the .beam files as we compile them, so they become available for other
  % modules that need them.
  %
  % This is important to make parse_transforms work correctly.
  %
  OutDir = outdir(),
  ok = filelib:ensure_path(OutDir),
  true = code:add_path(OutDir),

  Opts = #{
           compiler_opts => [{outdir, OutDir} | IncludeDirs],
           include_paths => IncludeDirs
          },
  Results = lists:foldl(fun (Source, Acc) ->
                            maps:put(Source, analyze(Source, Opts), Acc) end, #{}, Sources),

  _ = file:del_dir(OutDir),

  {ok, Results}.


-spec analyze(path:t(), opts()) -> result:t(mod_desc(), err()).
analyze(Path, #{ compiler_opts := CompileOpts, include_paths := IncludeDirs }) when is_binary(Path) ->
  ParseTrans = find_required_transforms(Path, IncludeDirs),

  % Compile Sources into AST
  case do_compile(Path, CompileOpts ++ ParseTrans) of
    {ok, Mod, Core} ->
      do_analyze(Path, Mod, Core);
    {error, Reasons, Other} ->
      #{ path => Path,
         error => clean_compile_error(Reasons, Other) }
  end.

find_required_transforms(Path, IncludeDirs) -> 
  {ok, Forms} = erl_ast:parse_file(Path, IncludeDirs),
  lists:filtermap(
    fun
      ({attribute, _, compile, {parse_transform, Mod}}) -> {true, {parse_transform, Mod}};
      (_) -> false
    end,
    Forms
   ).
  

do_compile(Path, CompileOpts) ->
  % FIXME(@ostera): split this so we compile only once, and then manually write the file.
  File = binary:bin_to_list(Path),
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

do_analyze(Path, Mod, Core) ->
  Tree = cerl:from_records(Core),
  Attrs = cerl:module_attrs(Core),

  #{
    name => Mod,
    path => Path,
    local_calls => local_calls(Mod, Tree),
    external_calls => external_calls(Mod, Tree),
    exports => exports(Mod, Tree),
    type_exports => type_exports(Mod, Attrs),
    includes => includes(Mod, Attrs)
    % attrs => Attrs
   }.

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
uniq(Xs) -> sets:to_list(sets:from_list(Xs, [{version, 2}])).

tag_files(Files) -> tag_files(Files,  [], [], []).
tag_files([], Srcs, Hdrs, Others) -> #{ sources => Srcs, headers => Hdrs, others => Others };
tag_files([<<"">>|Files], Srcs, Hdrs, Others) -> tag_files(Files, Srcs, Hdrs, Others);
tag_files([<<".">>|Files], Srcs, Hdrs, Others) -> tag_files(Files, Srcs, Hdrs, Others);
tag_files([File|Files], Srcs, Hdrs, Others) ->
  case (catch filename:extension(File)) of
    <<".erl">> -> tag_files(Files, [File|Srcs], Hdrs, Others);
    <<".hrl">> -> tag_files(Files, Srcs, [File|Hdrs], Others);
    _ -> tag_files(Files, Srcs, Hdrs, [File|Others])
  end.

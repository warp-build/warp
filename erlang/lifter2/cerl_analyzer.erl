%% @doc The `cerl_analyzer` analyzes a file by compiling it to Core Erlang and
%% traversing the resulting Core Trees.
%%
-module(cerl_analyzer).

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

%%--------------------------------------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------------------------------------

-spec analyze(path:t()) -> result:t(mod_desc(), err()).
analyze(Path) ->
	% Compile Sources into AST
	case compile:noenv_file(Path, [to_core, binary, return_errors]) of
		{ok, Mod, Core} -> {ok, do_analyze(Path, Mod, Core)};
		{error, Reason} -> {error, {compilation_error, Reason}}
	end.

do_analyze(Path, Mod, Core) ->
	Tree = cerl:from_records(Core),
	Attrs = cerl:module_attrs(Core),

	#{
		name => Mod,
		path => binary:list_to_bin(Path),
		local_calls => local_calls(Mod, Tree),
		external_calls => external_calls(Mod, Tree),
		exports => exports(Mod, Tree),
		type_exports => type_exports(Mod, Attrs),
		includes => includes(Mod, Attrs)
		% attrs => Attrs
	 }.

includes(Mod, Attrs) ->
	Includes0 = [{cerl:concrete(L1), cerl:concrete(L2)} || {L1, L2} <- Attrs,
																												 cerl:is_literal(L1), cerl:is_literal(L2),
																												 cerl:concrete(L1) =:= 'include' ],
	Includes1 = [{cerl:concrete(L1), cerl:concrete(L2)} || {L1, L2} <- Attrs,
																												 cerl:is_literal(L1), cerl:is_literal(L2),
																												 cerl:concrete(L1) =:= 'include_lib' ],
	sets:to_list(sets:from_list([{Mod, Kind, AST} || {Kind, AST} <- Includes0 ++ Includes1], [{version, 2}])).

type_exports(Mod, Attrs) ->
	ExpTypes1 = [cerl:concrete(L2) || {L1, L2} <- Attrs,
																		cerl:is_literal(L1),
																		cerl:is_literal(L2),
																		cerl:concrete(L1) =:= 'export_type'],
	ExpTypes2 = lists:flatten(ExpTypes1),
	sets:to_list(sets:from_list([mfa(Mod, F, A) || {F, A} <- ExpTypes2], [{version, 2}])).

exports(Mod, Tree) ->
	Exports1 = cerl:module_exports(Tree),
	Exports2 = [cerl:var_name(V) || V <- Exports1],
	[ mfa(Mod, F, A) || {F, A} <- Exports2, F =/= module_info ].

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
			Edges = cerl_trees:fold(fun extract_call/2, [], Function),
			MFA = mfa(Mod, FunName, Arity),
			[ #{ mfa => MFA, calls => Edges } |Acc]
	end.

extract_call(Tree, Acc) -> extract_call(cerl:type(Tree), Tree, Acc).
extract_call(call, Tree, Acc) ->
	M = cerl:atom_val(cerl:call_module(Tree)),
	F = cerl:atom_val(cerl:call_name(Tree)),
	A = length(cerl:call_args(Tree)),
	[mfa(M, F, A)|Acc];
extract_call(_, _, Acc) -> Acc.

mfa(M, F, A) -> #{ m => M, f => F, a => A }.

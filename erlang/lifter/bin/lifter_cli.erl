-module(lifter_cli).

-include_lib("kernel/include/logger.hrl").

-export([lift/1]).
-export([analyze_files/2]).
-export([sort_deps/1]).
-export([missing_deps/1]).
-export([find_rebar_dependencies/1]).

-define(JSON(X), jsone:encode(X, [{indent, 2}, {space, 1}])).
-define(PRINT_JSON(X), io:format("~s\n", [jsone:encode(X, [{indent, 2}, {space, 1}])])).

%===================================================================================================
% @doc Lift the current directory.
% information about the dependencies, and a place to set up overrides.
%===================================================================================================
lift(WorkspaceRoot0) ->
  WorkspaceRoot = binary:list_to_bin(WorkspaceRoot0),

  % 1. find all rebar.configs to find all deps
  ?LOG_INFO("Searching for Rebar3 Projects in " ++ WorkspaceRoot),
  {ok, Projects} = lifter_rebar3:find_all_rebar_projects(WorkspaceRoot),

  % 1-b. extract from the project configurations all the info we need, like modules to ignore
  IgnoreMods= sets:from_list(lists:foldl(
                   fun ({_, #{ proj := Proj }}, Acc) ->
                       CoverIgnore = maps:get(cover_excl_mods, Proj, []),
                       XrefIgnore = [ begin
                                        case Ig of
                                          {Mod, _, _} -> Mod;
                                          Mod when is_atom(Mod) -> Mod
                                        end
                                      end
                                      || Ig <- maps:get(xref_ignores, Proj, []) ],
                       Acc ++ XrefIgnore ++ CoverIgnore
                   end, [], maps:to_list(Projects))),

  % 2. flatten all deps
  ?LOG_INFO("Flattening transitive dependencies..."),
  {ok, ExternalDeps} = lifter_rebar3:download_and_flatten_dependencies(WorkspaceRoot, Projects),

  % 3. build lookup table from module/include to dep
  ExternalTable = lists:foldl(
            fun (Dep = #{ url := Url, name := Name, files := Files, root := Prefix } , Acc) ->
                AllFiles = lists:flatten(maps:values(Files)),
                #{ headers := Headers, sources := Sources } = source_tagger:tag(AllFiles),

                SrcEntries = lists:flatmap(
                               fun (Src) ->
                                   {ok, ModName} = erl_stdlib:file_to_module(Src),
                                   [{ModName, Url}]
                               end, Sources),

                HdrEntries = lists:flatmap(
                               fun (Hdr) ->
                                   Include = path:filename(Hdr),
                                   RelInclude = path:strip_prefix(Prefix, Hdr),
                                   LibInclude = path:join(Name, RelInclude),
                                   TailInclude = path:tail(Hdr),
                                   [
                                    {Include, Url},
                                    {RelInclude, Url},
                                    {LibInclude, Url},
                                    {TailInclude, Hdr}
                                   ]
                               end, Headers),

                DepMap = maps:from_list(SrcEntries ++ HdrEntries),

                maps:merge(Acc, DepMap)
            end, #{}, ExternalDeps),

  % 4. tag all sources 
  AllFiles = [ P || P <- glob:glob(path:join(WorkspaceRoot, "**/*.{erl,hrl}")),
                    not path:contains(P, "_build"),
                    not path:contains(P, "warp-outputs"),
                    not path:contains(P, ".warp")
             ],
  ?LOG_INFO("Lifting workspace with ~p files", [length(AllFiles)]),
  #{ headers := Headers, sources := Sources } = source_tagger:tag(AllFiles),
  SourceTable = lists:foldl(
            fun (Src, Acc) ->
                {ok, ModName} = erl_stdlib:file_to_module(Src),
                maps:merge(Acc, maps:from_list([{ModName, Src}]))
            end, #{}, Sources),

  HeaderTable = lists:foldl(
            fun (Hdr, Acc) ->
                Include = path:filename(Hdr),
                RelInclude = path:strip_prefix(WorkspaceRoot, Hdr),
                SrcInclude = path:tail(RelInclude),
                LibInclude = path:join(path:filename(WorkspaceRoot), RelInclude),
                TailInclude = path:tail(Hdr),
                Entries = maps:from_list([
                                          {Hdr, Hdr},
                                          {Include, Hdr},
                                          {RelInclude, Hdr},
                                          {SrcInclude, Hdr},
                                          {LibInclude, Hdr},
                                          {TailInclude, Hdr}
                                         ]),
                maps:merge(Acc, Entries)
            end, #{}, Headers),

  ModMap = maps:merge(maps:merge(ExternalTable, SourceTable), HeaderTable),

  IncludePaths = uniq([
                  path:join(WorkspaceRoot, "apps"),
                  path:join(WorkspaceRoot, ".warp/_rebar_tmp/rebar3/default/lib")
                 ] ++ [ begin 
                          PAbs = case path:is_prefix(WorkspaceRoot, P) of
                                      true -> P;
                                      false -> path:join(WorkspaceRoot, P)
                                 end,
                          P0 = path:dirname(PAbs),
                          P1 = path:dirname(P0),
                          P2 = path:dirname(P1),
                          [P0,P1,P2]
                        end || P <- maps:keys(HeaderTable) ]),

  % 5. analyze all the sources to get their signatures
  {ok, Signatures} = source_analyzer:analyze(AllFiles, ModMap, IgnoreMods, IncludePaths),

  % 6. generate warp signatures
  % ?LOG_INFO("Writing Warp signature files..."),
  % maps:foreach(fun (Path, WarpSig) ->
  %                   ?LOG_INFO("- ~s\n", [Path]),
  %                   ok = file:write_file(Path, ?JSON(WarpSig))
  %               end, Signatures),

  % 7. group and generate build files
  ?LOG_INFO("Writing Build.toml files..."),
  Buildfiles = lists:foldl(
                 fun ({Path, WarpSig}, Acc) ->
                     BuildPath = path:join(path:dirname(Path), "Build.json"),
                     LastTargets = maps:get(BuildPath, Acc, []), 
                     maps:put(BuildPath, LastTargets ++ WarpSig, Acc)
                 end, #{}, maps:to_list(Signatures)),

  maps:foreach(fun (Path, BuildFile) ->
                    ?LOG_INFO("- ~s\n", [Path]),
                    ok = file:write_file(Path, ?JSON(BuildFile))
                end, Buildfiles),

  ?LOG_INFO("OK").



%===================================================================================================
% @doc Creates the 3rdparty/rebar.manifest file that includes all the
% information about the dependencies, and a place to set up overrides.
%===================================================================================================

find_rebar_dependencies(WorkspaceRoot0) ->
  ?LOG_INFO("Searching for Rebar3 Projects in " ++ WorkspaceRoot0),
  WorkspaceRoot = binary:list_to_bin(WorkspaceRoot0),
  {ok, Projects} = lifter_rebar3:find_all_rebar_projects(WorkspaceRoot),
  ?LOG_INFO("Flattening transitive dependencies..."),
  {ok, ExternalDeps} = lifter_rebar3:download_and_flatten_dependencies(WorkspaceRoot, Projects),
  ?PRINT_JSON(ExternalDeps).

%===================================================================================================
% @doc Computes the missing dependencies for a list of Erlang source and header
% files.
%===================================================================================================

missing_deps(Files) ->
  {ok, Analysis0} = erl_analyzer:analyze([ binary:list_to_bin(Path) || Path <- Files ]),
  Analysis = maps:to_list(Analysis0),

  Missing = #{
              modules => uniq([ Mod || {_, #{ missing_modules := Mod }} <- Analysis ]),
              headers => uniq([ Inc || {_, #{ missing_includes := Inc }} <- Analysis ])
            },

  io:format("~s\n", [?JSON(Missing)]),
  ok.

%===================================================================================================
% @doc Sort all input files that are Erlang sources by the order in which they
% should be processed/analyzed/compiled.
%
% NOTE: this is a topological sort over a dependency graph where edges go from
% a module to its dependencies.
%===================================================================================================

sort_deps(Files) ->
  case lifter_depgraph:sort_files([ binary:list_to_bin(Path) || Path <- Files ]) of
    {ok, Sort} -> 
     [ io:format("~s\n", [ S ]) || S <- Sort ];
    {error, {found_cycles, Cycles}} ->
      io:format("~s\n", [?JSON(#{ dependency_cycles => Cycles })])
  end.

%===================================================================================================
% @doc Analyze all the Core Erlang trees of all the Erlang sources in the input
% files, and output a list of analyses that includes:
%     * external calls to other modules
%     * exports from this module
%     * type exports from this module
%     * files included as headers
%     * other attributes
%
% These can be used to build a dependency graph.
%===================================================================================================

analyze_files(_WorkspaceRoot, Files) ->
  {ok, Results} = cerl_analyzer:analyze([ binary:list_to_bin(Path) || Path <- Files ]),

  {Oks, Errs} = lists:partition(fun
                                ({_, #{error := _ }}) -> false;
                                ({_, _}) -> true
                               end, maps:to_list(Results)),

  StringyErrs = lists:map(
                  fun ({_, #{ error := Err, path := Path }}) ->
                      {Path, #{ path => Path, error =>
                                binary:list_to_bin(lists:flatten(io_lib:format("~p", [Err]))) }}
                  end,
                  Errs),

  Json = ?JSON(#{
                 oks => maps:from_list(Oks),
                 errs => maps:from_list(StringyErrs)
                }),

  io:format("~s\n", [ Json ]),
  ok.

%===================================================================================================
% Utilities
%===================================================================================================

uniq([]) -> [];
uniq([X]) -> [X];
uniq(Xs) -> lists:sort(sets:to_list(sets:from_list(lists:flatten(Xs), [{version, 2}]))).

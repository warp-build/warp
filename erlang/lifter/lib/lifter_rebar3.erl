-module(lifter_rebar3).

-define(JSON(X), jsone:encode(X, [{indent, 2}, {space, 1}])).
-include_lib("kernel/include/logger.hrl").

-export([download/2]).
-export([download_and_flatten_dependencies/2]).
-export([find_all_rebar_projects/1]).
-export([locked_dependencies/2]).

-export_type([t/0]).

-type t() :: #{ path:t() => #{} }.

%===================================================================================================
% @doc Get the list of flattened transitive dependencies and the versions they are locked to.
%===================================================================================================

locked_dependencies(WorkspaceRoot, Projects) ->
  LockFile = path:join(WorkspaceRoot, "rebar.lock"),
  Rebar3File = path:join(WorkspaceRoot, "rebar.config"),
  case {filelib:is_file(Rebar3File), filelib:is_file(LockFile)} of
    {true, _} -> do_locked_dependencies(WorkspaceRoot, Projects);
    {false, true} -> do_locked_dependencies(WorkspaceRoot, Projects);
    {false, false} ->  {ok, [], []}
  end.

do_locked_dependencies(WorkspaceRoot, Projects) ->
  LockedDeps = case file:consult(path:join(WorkspaceRoot, "rebar.lock")) of
                 {ok, Lock} -> case proplists:to_map(Lock) of
                                 #{ "1.2.0" := X } -> X;
                                 _ -> []
                               end;
                 _ -> []
               end,

  ?LOG_INFO("Getting dependencies sources to facilitate analysis..."),
  {ok, ExternalDeps} = 
    case LockedDeps of
      [] ->
        % 2. flatten all deps
        ?LOG_INFO("Lock file was empty. Attempting to flatten transitive dependencies..."),
        download_and_flatten_dependencies(WorkspaceRoot, Projects);
      _ ->
        RebarDeps =  [
                      case Vsn of
                        {pkg, PkgName, SemVer} -> {erlang:binary_to_atom(PkgName), SemVer};
                        Git -> {erlang:binary_to_atom(Name), Git}
                      end
                      || {Name, Vsn, _} <- LockedDeps ],
        download(#{ deps => RebarDeps }, WorkspaceRoot)
    end,

  FinalLockedDeps = case LockedDeps of
                      [] -> 
                        {ok, NewLock} = file:consult(path:join(WorkspaceRoot, ".warp/_rebar_tmp/rebar.lock")),
                        case proplists:to_map(NewLock) of
                          #{ "1.2.0" := Y } -> Y;
                          _ -> []
                        end;

                      _ ->
                        LockedDeps
                    end,

  {ok, ExternalDeps, FinalLockedDeps}.


%===================================================================================================
% @doc Find all dependencies in a workspace by folding over all rebar.config
% files and "consulting" them.
%===================================================================================================

-spec find_all_rebar_projects(path:t()) -> result:t(t(), term()).
find_all_rebar_projects(Root0) when is_binary(Root0) ->
  Root = binary:bin_to_list(Root0),
  MixProjects = filelib:fold_files(Root, "hex_metadata\.config$", true, fun read_hex_metadata/2, #{}),
  RebarProjects = filelib:fold_files(Root, "rebar\.config$", true, fun read_rebar_project/2, #{}),
  ErlMkProjects = filelib:fold_files(Root, "erlang\.mk", true, fun read_erlmk_project/2, #{}),
  Final = maps:merge(maps:merge(RebarProjects, ErlMkProjects), MixProjects),
  {ok, clean(Final)}.

read_hex_metadata(HexMetadata, Acc) ->
  ProjectRoot = path:relativize(filename:dirname(HexMetadata)),
  ProjectName = path:filename(ProjectRoot),

  {ok, Project} = file:consult(HexMetadata),
  ProjMap = maps:from_list(Project),

  FinalProject = [
                  {name, ProjectName},
                  {url, hexpm:pkg_to_url(ProjectName)},
                  {root, ProjectRoot},
                  {files, get_sources(ProjectRoot)},
                  {deps, []},
                  {proj, ProjMap}
                 ],

  maps:put(HexMetadata, maps:from_list(FinalProject), Acc).

read_rebar_project(RebarConfig, Acc) ->
  ProjectRoot = filename:dirname(RebarConfig),
  ProjectName = path:filename(ProjectRoot),

  case ProjectName of
    "3rdparty" -> Acc;
    _ -> do_read_rebar_project(RebarConfig, Acc)
  end.

do_read_rebar_project(RebarConfig, Acc) ->
  ProjectRoot = path:relativize(filename:dirname(RebarConfig)),
  ProjectName = path:filename(ProjectRoot),

  {ok, Project} = file:consult(RebarConfig),
  ProjMap = maps:from_list(Project),

  FinalProject = [
                  {name, ProjectName},
                  {url, hexpm:pkg_to_url(ProjectName)},
                  {root, ProjectRoot},
                  {files, get_sources(ProjectRoot)},
                  {deps, clean_deps(get_deps(ProjMap))},
                  {proj, ProjMap}
                 ],

  maps:put(RebarConfig, maps:from_list(FinalProject), Acc).

read_erlmk_project(ErlangMkConfig, Acc) ->
  ProjectRoot = filename:dirname(ErlangMkConfig),
  ProjectName = path:filename(ProjectRoot),

  case ProjectName of
    "3rdparty" -> Acc;
    _ -> do_read_erlmk_project(ErlangMkConfig, Acc)
  end.

do_read_erlmk_project(ErlangMkConfig, Acc) ->
  ProjectRoot = path:relativize(filename:dirname(ErlangMkConfig)),
  ProjectName = path:filename(ProjectRoot),

  {ok, Project} = lifter_erlangmk:from_file(filename:join([ProjectRoot, "Makefile"])),
  ProjMap = maps:from_list(Project),

  FinalProject = [
                  {name, ProjectName},
                  {url, hexpm:pkg_to_url(ProjectName)},
                  {root, ProjectRoot},
                  {files, get_sources(ProjectRoot)},
                  {deps, clean_deps(get_deps(ProjMap))},
                  {proj, ProjMap}
                 ],

  maps:put(ErlangMkConfig, maps:from_list(FinalProject), Acc).

clean_deps(Deps) -> clean_deps(Deps, []).
clean_deps([], Acc) -> Acc;
clean_deps([{_, {path, _}}|Rest], Acc) -> clean_deps(Rest, Acc);
clean_deps([Dep|Rest], Acc) -> clean_deps(Rest, [Dep|Acc]).


get_deps(Pkg) when is_list(Pkg) -> get_deps(proplists:to_map(Pkg));
get_deps(_Pkg=#{ deps := Deps, profiles := Profiles}) when is_list(Profiles) ->
  Deps ++ lists:flatmap(fun get_deps/1, maps:values(maps:from_list(Profiles)));
get_deps(_Pkg=#{ deps := Deps, profiles := Profiles}) when is_map(Profiles) ->
  Deps ++ lists:flatmap(fun get_deps/1, maps:values(Profiles));
get_deps(_Pkg=#{ deps := Deps }) -> Deps;
get_deps(_) -> [].

get_sources(ProjectRoot) ->
  SrcGlob = path:join(ProjectRoot, "src/**/*.{erl,hrl}"),
  TestGlob = path:join(ProjectRoot, "test/**/*.{erl,hrl}"),
  IncludeGlob = path:join(ProjectRoot, "include/**/*.{erl,hrl}"),
  PrivGlob = path:join(ProjectRoot, "priv/**/*"),

  Srcs = #{
           srcs => glob:glob(ProjectRoot,SrcGlob),
           tests => glob:glob(ProjectRoot,TestGlob),
           includes => glob:glob(ProjectRoot,IncludeGlob),
           priv => glob:glob(ProjectRoot,PrivGlob)
          },

  Srcs.

clean(Map) when is_map(Map) -> clean(maps:to_list(Map), []);

clean([]) -> [];
clean(Term=[{_,_}|_]) -> clean(proplists:to_map(Term));
clean(Term) when is_list(Term) ->
  case (catch string:to_graphemes(Term)) of
    % NOTE(@ostera): if the graphemes of a list are the same as the list, then this is just a string
    % masquerading as a list! 
    %   string:to_graphemes("hello") == "hello"
    %   string:to_graphemes(["1","2"]) == "12"
    Term -> binary:list_to_bin(Term);

    _ -> lists:map(fun clean/1, Term)
  end;

clean(Term) -> Term.

clean([], Acc) -> maps:from_list(Acc);
clean([{K, V}|Rest], Acc) -> clean(Rest, [{clean(K), clean(V)} | Acc]).
  
%===================================================================================================
% @doc Given a set of dependencies, install them all.
%===================================================================================================

download(Rebar3Config, WorkspaceRoot)  ->
  Root = path:join(WorkspaceRoot,<<"./.warp/_rebar_tmp">>),
  path:ensure(Root),

  Rebar3File = [ {base_dir, "rebar3"} ] ++ maps:to_list(Rebar3Config),

  ok = write_terms(filename:join(Root, "rebar.config"), Rebar3File),
  ok = in_dir(Root, fun () -> run_rebar("get-deps") end),

  {ok, NewProjects} = find_all_rebar_projects(Root),

  {ok, maps:values(NewProjects)}.

write_terms(Filename, List) ->
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = unicode:characters_to_binary(lists:map(Format, List)),
    file:write_file(Filename, Text).


%===================================================================================================
% @doc Given a map of project files, flatten all dependencies by calling rebar2
% repeatedly until we have listed all transitive dependencies.
%===================================================================================================

-spec download_and_flatten_dependencies(path:t(), t()) -> result:t(t(), term()).
download_and_flatten_dependencies(WorkspaceRoot, Projects) ->
  Root = path:join(WorkspaceRoot,<<"./.warp/_rebar_tmp">>),
  path:ensure(Root),
  ?LOG_INFO("Dowloading and flattening deps at ~p", [Root]),
  loop_flatten(Root, 0, Projects).

loop_flatten(Root, Iter, Projects) ->
  {ok, CurrentDeps} = extract_all_deps(Projects),
  ?LOG_INFO("Found ~p dependencies: ~p\n", [maps:size(CurrentDeps), lists:sort(maps:keys(CurrentDeps))]),

  ok = install(Root, CurrentDeps),

  {ok, NewProjects} = find_all_rebar_projects(Root),
  {ok, FreshDeps} = extract_all_deps(NewProjects),

  ?LOG_INFO("Verifying all transitive dependencies are specified..."),
  case CurrentDeps == FreshDeps of
    true ->
      ?LOG_INFO("Saving manifest..."),
      Rebar3File = rebar3_file(CurrentDeps),
      ok = file:write_file(filename:join(Root, "rebar.config"), Rebar3File),
      {ok, maps:values(NewProjects)};
    false ->
      loop_flatten(Root, Iter + 1, NewProjects)
  end.


extract_all_deps(Projects) ->
  AllDeps = do_extract_deps(Projects),
  {ok, collect_and_choose_newer(AllDeps)}.

do_extract_deps(Projects) ->
  Pkgs = maps:values(Projects),
  uniq(lists:flatmap(
    fun
      (_Pkg=#{ deps := Deps, profiles := #{ test := #{ deps := TestDeps }}})
        when Deps =/= [], is_map(Deps), TestDeps =/= [], is_map(TestDeps) ->
        maps:to_list(Deps) ++ maps:to_list(TestDeps);

      (_Pkg=#{ deps := Deps }) when Deps =/= [], is_map(Deps)->
        maps:to_list(Deps);

      (_) -> []
    end, Pkgs)).

collect_and_choose_newer(Deps) -> collect_and_choose_newer(Deps, #{}).
collect_and_choose_newer([], Acc) -> maps:from_list(lists:sort(maps:values(Acc)));
collect_and_choose_newer([D|T], Acc0) ->
  Name = name(D),
  Dep = choose_dep(D, maps:get(Name, Acc0, none)),
  Acc1 = maps:put(Name, Dep, Acc0),
  collect_and_choose_newer(T, Acc1).

choose_dep(A, none) -> A;
choose_dep(A, B) ->
  case compare_v(version(A), version(B)) of
    lt -> B;
    _ -> A
  end.

compare_v({override, _}, _) -> gt;
compare_v(_, {override, _}) -> lt;
compare_v({semver, A}, {semver, B}) -> verl:compare(A, B).

name(Dep) when is_tuple(Dep) -> name(element(1, Dep));
name(Name) when is_list(Name) -> list_to_atom(Name);
name(Name) when is_binary(Name) -> binary_to_atom(Name, utf8);
name(Name) when is_atom(Name) -> Name.

version(Dep) ->
  RawV = v(Dep),
  CleanV = clean_v(RawV),
  case verl:parse(CleanV) of
    {ok, V} -> {semver, V};
    _ -> {override, RawV}
  end.
v({_Name, {git_subdir, _Repo, {tag, Version}, _Subdir}}) -> Version;
v({_Name, {git_subdir, _Repo, {branch, Version}, _Subdir}}) -> Version;
v({_Name, {git_subdir, _Repo, {ref, Version}, _Subdir}}) -> Version;
v({_Name, {git, _Repo, {tag, Version}}}) -> Version;
v({_Name, {git, _Repo, {branch, Version}}}) -> Version;
v({_Name, {git, _Repo, {ref, Version}}}) -> Version;
v({_Name, {path, _Path}}) -> "0.0.0";
v({_Name, Version}) when is_binary(Version) -> Version;
v({_Name, Version}) when is_list(Version) -> Version.

clean_v(Version) when is_list(Version) -> clean_v(binary:list_to_bin(Version));
clean_v(<<"v", Version/binary>>) -> Version;
clean_v(Version) -> Version.


install(Root, Deps) ->
  Rebar3File = rebar3_file(Deps),
  ok = file:write_file(filename:join(Root, "rebar.config"), Rebar3File),
  ok = in_dir(Root, fun () -> run_rebar("get-deps") end).

rebar3_file(Deps) ->
<<"

{base_dir, \"rebar3\"}.

{deps, \n", (binary:list_to_bin(lists:flatten(io_lib:format("~p", [lists:sort(maps:to_list(Deps))]))))/binary, "\n}.

">>.

run_rebar(Cmd) ->
  ?LOG_INFO("Running `rebar3 " ++ Cmd ++"`...this could take a while!"),

  Output = os:cmd("rebar3 "++Cmd),
  ?LOG_INFO("~s\n", [Output]),

  ok.

in_dir(Dir, Fn) ->
  {ok, Cwd} = file:get_cwd(),
  ok = file:set_cwd(Dir),
  Res = Fn(),
  ok = file:set_cwd(Cwd),
  Res.

uniq([]) -> [];
uniq([X]) -> [X];
uniq(Xs) -> lists:flatten(sets:to_list(sets:from_list(Xs, [{version, 2}]))).

-module(lifter_rebar3).


-export([find_all_rebar_projects/1]).

%===============================================================================
% Find all dependencies in a workspace by folding over all rebar.config files
% and "consulting" them.
%===============================================================================

find_all_rebar_projects(Root) ->
  RebarProjects = filelib:fold_files(Root, "rebar\.config$", true, fun read_rebar_project/2, #{}),
  ErlMkProjects = filelib:fold_files(Root, "erlang\.mk", true, fun read_erlmk_project/2, #{}),
  Final = maps:merge(RebarProjects, ErlMkProjects),
  {ok, clean(Final)}.

read_rebar_project(RebarConfig) -> read_rebar_project(RebarConfig, #{}).
read_rebar_project(RebarConfig, Acc) ->
  ProjectRoot = filename:dirname(RebarConfig),
  ProjectName = filename:basename(ProjectRoot),

  case ProjectName of
    "3rdparty" -> Acc;
    _ -> do_read_rebar_project(RebarConfig, Acc)
  end.

do_read_rebar_project(RebarConfig, Acc) ->
  ProjectRoot = filename:dirname(RebarConfig),
  ProjectName = filename:basename(ProjectRoot),

  {ok, Project} = file:consult(RebarConfig),

  FinalProject = [
                  {name, binary:list_to_bin(ProjectName)},
                  {root, binary:list_to_bin(ProjectRoot)},
                  {srcs, get_sources(ProjectRoot)}
                  | Project
                 ],

  maps:put(RebarConfig, maps:from_list(FinalProject), Acc).

read_erlmk_project(ErlangMkConfig, Acc) ->
  ProjectRoot = filename:dirname(ErlangMkConfig),
  ProjectName = filename:basename(ProjectRoot),

  case ProjectName of
    "3rdparty" -> Acc;
    _ -> do_read_erlmk_project(ErlangMkConfig, Acc)
  end.

do_read_erlmk_project(ErlangMkConfig, Acc) ->
  ProjectRoot = filename:dirname(ErlangMkConfig),
  ProjectName = filename:basename(ProjectRoot),

  {ok, Project} = lifter_erlangmk:from_file(filename:join([ProjectRoot, "Makefile"])),

  FinalProject = [
                  {name, binary:list_to_bin(ProjectName)},
                  {root, binary:list_to_bin(ProjectRoot)},
                  {srcs, get_sources(ProjectRoot)}
                  | Project
                 ],

  maps:put(ErlangMkConfig, maps:from_list(FinalProject), Acc).

get_sources(ProjectRoot) ->
  SrcGlob = ProjectRoot ++ "/src/**/*.{erl,hrl}",
  TestGlob = ProjectRoot ++ "/test/**/*.{erl,hrl}",
  IncludeGlob = ProjectRoot ++ "/include/**/*.{erl,hrl}",
  PrivGlob = ProjectRoot ++ "/priv/**/*",

  Srcs = #{
           srcs => glob(SrcGlob),
           tests => glob(TestGlob),
           includes => glob(IncludeGlob),
           priv => glob(PrivGlob)
          },

  Srcs.


glob(Glob) -> [ binary:list_to_bin(P) || P <- filelib:wildcard(Glob) ].

clean(Map) when is_map(Map) -> clean(maps:to_list(Map), []);

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
  

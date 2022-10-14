-module(main).

-mode(compile).
-export([main/1]).

-include("lifter.hrl").
-include_lib("kernel/include/logger.hrl").

main(Args) ->
  ok = setup(),

  {ok, WorkspaceRoot} = file:get_cwd(),
  ?LOG_INFO("Running lifter on ~s", [WorkspaceRoot]),

  case Args of
    [] -> lifter_cli:lift(WorkspaceRoot);
    ["lift", Root] -> lifter_cli:lift(Root);
    ["help"] -> show_help();
    ["find-rebar-deps"] -> lifter_cli:find_rebar_dependencies(WorkspaceRoot);
    ["find-rebar-deps" | Root] -> lifter_cli:find_rebar_dependencies(Root);
    ["analyze-file", File] -> lifter_cli:analyze_files(WorkspaceRoot, [File]);
    ["analyze-files" | Files] -> lifter_cli:analyze_files(WorkspaceRoot, Files);
    ["sort-deps" | Files] -> lifter_cli:sort_deps(Files);
    ["missing-deps" | Files] -> lifter_cli:missing_deps(Files);
    _ -> show_help()
  end.

setup() ->
  logger:set_primary_config(#{ level => all }),
  logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
  [HandlerId] = logger:get_handler_ids(),
  logger:remove_handler(HandlerId),
  logger:add_handler(default_h, logger_std_h, #{ config => #{ type => standard_error }}),
  ok.

show_help() ->
  io:format("~s", [<<"

Usage:

* lifter
* lifter lift ./path/to/dir
* lifter analyze-file path/to/file.erl
* lifter analyze-files a.erl b.erl c.erl
* lifter sort-deps a.erl b.erl c.erl
* lifter missing-deps a.erl b.erl c.erl
* lifter find-rebar-deps [./path/to/dir = $cwd]

  ">>]),
  ok.

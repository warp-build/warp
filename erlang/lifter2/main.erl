-module(main).

-mode(compile).
-export([main/1]).

-include("lifter.hrl").
-include_lib("kernel/include/logger.hrl").

main(Args) ->
  ok = setup(),

  {ok, WorkspaceRoot} = file:get_cwd(),

  Result = case Args of
             ["help"] -> show_help();
             ["analyze-file", File] -> lifter_cli:analyze_files(WorkspaceRoot, [File]);
             ["analyze-files" | Files] -> lifter_cli:analyze_files(WorkspaceRoot, Files);
             _ -> show_help()
           end,

  case Result of
    ok -> ok;
    {error, Reason} -> ?LOG_ERROR("Uh-oh! Something went wrong:\n\n~p\n", [Reason])
  end.

setup() ->
  logger:set_primary_config(#{ level => all }),
  logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
  ok.

show_help() ->
  io:format("~s", [<<"

Usage:

* lifter analyze-file path/to/file.erl
* lifter analyze-files a.erl b.erl c.erl
* lifter dep-graph a.erl b.erl c.erl

  ">>]),
  ok.

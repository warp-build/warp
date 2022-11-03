%39
-module(main).

-mode(compile).
-export([main/1]).

-include_lib("kernel/include/logger.hrl").

main(Args) ->
  ok = setup(),
  case Args of
    [] -> lifter_cli:lift(".");
    ["lift", Root] -> lifter_cli:lift(Root);

    ["analyze-file", File] -> lifter_cli:analyze_files(".", [File]);
    ["analyze-files" | Files] -> lifter_cli:analyze_files(".", Files);

    ["find-rebar-deps"] -> lifter_cli:find_rebar_dependencies(".");
    ["find-rebar-deps" | Root] -> lifter_cli:find_rebar_dependencies(Root);

    ["generate-signature", Symbol, File] -> lifter_cli:generate_signature(Symbol, File);
    ["generate-signatures", Root] -> lifter_cli:generate_signatures(Root);

    ["missing-deps" | Files] -> lifter_cli:missing_deps(Files);
    ["sort-deps" | Files] -> lifter_cli:sort_deps(Files);

    ["help"] -> show_help();
    _ -> show_help()
  end,
  timer:sleep(100).

setup() ->
  logger:set_primary_config(#{ level => all }),
  logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
  [HandlerId] = logger:get_handler_ids(),
  logger:remove_handler(HandlerId),
  logger:add_handler(default_h, logger_std_h, #{ config => #{ type => standard_error }}),
  ok.

show_help() ->
  io:format("~s\n", [<<"

Usage:

* lifter
* lifter lift ./path/to/dir
* lifter generate-signatures ./path/to/dir
* lifter generate-signature all ./path/to/file.erl

Other commands:

* lifter analyze-file path/to/file.erl
* lifter analyze-files a.erl b.erl c.erl
* lifter sort-deps a.erl b.erl c.erl
* lifter missing-deps a.erl b.erl c.erl
* lifter find-rebar-deps [./path/to/dir = $cwd]


">>]),
	erlang:halt().

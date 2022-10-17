-module(main).

-mode(compile).
-export([main/1]).

-include_lib("kernel/include/logger.hrl").

-define(PRINT_JSON(X), io:format("~s\n", [jsone:encode(X, [{indent, 2}, {space, 1}])])).

main(Args) ->
  ok = setup(),
  case Args of
    ["resolve", Url] -> ?PRINT_JSON(resolve(Url));
    ["help"] -> show_help()
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
* lifter analyze-file path/to/file.erl
* lifter analyze-files a.erl b.erl c.erl
* lifter sort-deps a.erl b.erl c.erl
* lifter missing-deps a.erl b.erl c.erl
* lifter find-rebar-deps [./path/to/dir = $cwd]

">>]),
	erlang:halt().

resolve(Url0) -> 
  Url = binary:list_to_bin(Url0),
  {ok, PkgSpec = {PkgName, _PkgVsn}} = hexpm:parse_url(Url),
  #{
    archive_url => hexpm:archive_url(PkgSpec),
    signatures => [
                   #{
                     name => hexpm:package_name(PkgSpec), 
                     rule => <<"mix_library">>
                    }
                  ]
   }.

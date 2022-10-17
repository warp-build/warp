-module(main).

-mode(compile).
-export([main/1]).

-include_lib("kernel/include/logger.hrl").

-define(PRINT_JSON(X), io:format("~s\n", [jsone:encode(X, [{indent, 2}, {space, 1}])])).

main(Args) ->
  ok = setup(),
  case Args of
    ["resolve", Url, Vsn] -> ?PRINT_JSON(resolve(Url, Vsn));
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

* resolver resolver https://hex.pm/packages/proper 1.4.0

">>]),
	erlang:halt().

resolve(Url0, Vsn0) -> 
  Url = binary:list_to_bin(Url0),
  Vsn = binary:list_to_bin(Vsn0),
  {ok, PkgSpec = {PkgName, _PkgVsn}} = hexpm:parse_url(<<Url/binary, Vsn/binary>>),
  #{
    archive_url => hexpm:archive_url(PkgSpec),
    signatures => [
                   #{
                     name => hexpm:package_name(PkgSpec), 
                     rule => <<"mix_library">>
                    }
                  ]
   }.

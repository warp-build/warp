-module(main).

-mode(compile).
-export([main/1]).

-include_lib("kernel/include/logger.hrl").

-define(JSON(X), jsone:encode(X, [{indent, 2}, {space, 1}, native_utf8, native_forward_slash])).
-define(PRINT_JSON(X), io:format("~s\n", [?JSON(X)])).

main(Args) ->
  ok = setup(),
  % {ok, Cwd} = file:get_cwd(),
  case Args of
    ["resolve", Url, Ref] -> ?PRINT_JSON(resolve(Url, Ref));
    ["help"] -> show_help()
  end.

setup() ->
  logger:set_primary_config(#{ level => all }),
  logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
  [HandlerId] = logger:get_handler_ids(),
  logger:remove_handler(HandlerId),
  logger:add_handler(default_h, logger_std_h, #{
                                                config => #{
                                                            type => standard_error,
                                                            sync_mode_qlen => 0
                                                           }
                                               }),
  ok.

show_help() ->
  io:format("~s\n", [<<"

Usage:

* resolver resolver https://github.com/user/repo <git-ref>
* prepare .

">>]),
	erlang:halt().

resolve(Url0, Ref0) -> 
  Url = binary:list_to_bin(Url0),
  Ref = binary:list_to_bin(Ref0),

  Tokens = [ binary:list_to_bin(T) || T <- string:tokens(binary:bin_to_list(Url), "/") ],
  RepoName = case Tokens of
               [_Http, _Github, _Username, Repo] -> Repo
             end,

  #{
    version => 0,
    archive => #{ 
                 url => <<Url/binary, "/archive/", Ref/binary, ".tar.gz">>,
                 strip_prefix => << RepoName/binary, "-", Ref/binary >>
                }
   }.

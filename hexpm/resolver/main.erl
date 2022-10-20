-module(main).

-mode(compile).
-export([main/1]).

-include_lib("kernel/include/logger.hrl").

-define(PRINT_JSON(X), io:format("~s\n", [jsone:encode(X, [{indent, 2}, {space, 1}])])).

main(Args) ->
  ok = setup(),
  {ok, Cwd} = file:get_cwd(),
  case Args of
    ["resolve", Url, Vsn] -> ?PRINT_JSON(resolve(Url, Vsn));
    ["prepare", Root] -> ?PRINT_JSON(prepare(Root));
    ["prepare"] -> ?PRINT_JSON(prepare(Cwd));
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
* analyze .

">>]),
	erlang:halt().

resolve(Url0, Vsn0) -> 
  Url = binary:list_to_bin(Url0),
  Vsn = binary:list_to_bin(Vsn0),
  {ok, PkgSpec = {PkgName, _PkgVsn}} = hexpm:parse_url(<<Url/binary, "/", Vsn/binary>>),
  #{
    version => 0,
    archive => #{ url => hexpm:archive_url(PkgSpec) }
   }.

prepare(Root) -> 
  % 1. read CHECKSUM and validate?
  % 2. unpack contents.tar.gz
  % 3. read metadata.config 
  %
  {ok, Metadata0} = file:consult(path:join(Root, "metadata.config")),
  Metadata = proplists:to_map(Metadata0),

  PkgName = maps:get(<<"app">>, Metadata),

  ok = erl_tar:extract(path:join(Root, "contents.tar.gz"), [compressed, {cwd, path:join(Root, PkgName)}]),

  Deps = [ req_to_dep(proplists:to_map(Req)) || Req <- maps:get(<<"requirements">>, Metadata, []) ],

  #{
    version => 0,
    metadata => Metadata,
    signatures => [
                   #{
                     name => maps:get(<<"app">>, Metadata),
                     rule => case maps:get(<<"build_tools">>, Metadata) of
                               [<<"rebar3">> | _] -> <<"rebar3_library">>;
                               _ -> <<"mix_library">>
                             end,
                     srcs => maps:get(<<"files">>, Metadata, []),
                     deps => Deps
                    }
                  ]
   }.

req_to_dep(#{ <<"name">> := Name, <<"repository">> := <<"hexpm">> }) -> hexpm:pkg_to_url(Name).

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

tools() -> [mix, make, rebar3].

prepare(Root) -> 
  ?LOG_DEBUG("Opening metadata.config in ~p", [Root]),
  % 1. read CHECKSUM and validate?
  % 2. unpack contents.tar.gz
  % 3. read metadata.config 
  %
  {ok, Metadata0} = file:consult(path:join(Root, "metadata.config")),
  Metadata = proplists:to_map(Metadata0),

  PkgName = maps:get(<<"app">>, Metadata),
  Srcs = maps:get(<<"files">>, Metadata, []),

  ?LOG_DEBUG("Found ~p and expecint ~i sources", [PkgName, length(Srcs)]),
  ?LOG_DEBUG("Extracting..."),
  ok = erl_tar:extract(path:join(Root, "contents.tar.gz"), [compressed, {cwd, path:join(Root, PkgName)}]),
  ?LOG_DEBUG("Finished extracting files to ~p", [path:join(Root, PkgName)]),

  _AllTools = tools(),
  Tools = proplists:to_map([ erlang:binary_to_existing_atom(T, utf8)
                             || T <- maps:get(<<"build_tools">>, Metadata, []) ]),

  Reqs = requirements(Metadata),
  Deps = get_deps(Reqs),


  Rule = case Tools of
           #{ mix := true } -> <<"mix_library">>;
           #{ make := true } -> <<"erlangmk_library">>;
           #{ rebar3 := true } -> <<"rebar3_library">>;
           _ => <<"mix_library">>
         end,

  #{
    version => 0,
    metadata => Metadata,
    signatures => [
                   #{
                     name => PkgName,
                     rule => Rule,
                     srcs => Srcs, 
                     deps => Deps
                    }
                  ]
   }.

requirements(Metadata) ->
  Reqs0 = maps:get(<<"requirements">>, Metadata, []),
  Reqs = lists:map(fun
                     ({Name, Req}) -> [ {<<"name">>, Name} | Req ];
                     (Req) when is_list(Req) -> Req
                   end, Reqs0).

get_deps(Reqs) -> [ req_to_dep(proplists:to_map(Req)) || Req <- Reqs ].

req_to_dep(#{ <<"name">> := Name }) -> hexpm:pkg_to_url(Name).

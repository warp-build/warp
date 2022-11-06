-module(main).

-mode(compile).
-export([main/1]).

-include_lib("kernel/include/logger.hrl").

-define(JSON(X), jsone:encode(X, [{indent, 2}, {space, 1}, native_utf8, native_forward_slash])).
-define(PRINT_JSON(X), io:format("~s\n", [?JSON(X)])).

main(Args) ->
  ok = setup(),
  {ok, Cwd} = file:get_cwd(),
  case Args of
    ["resolve", Url, Ref, _Pkg] -> ?PRINT_JSON(resolve(Url, Ref));
    ["prepare", Root, _Url, _Ref, Pkg] -> ?PRINT_JSON(prepare(Root, Pkg));
    ["prepare"] -> ?PRINT_JSON(prepare(Cwd, path:filename(Cwd)));
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

* resolve <url> <version> <pkg-name>
* analyze <root> <url> <version> <pkg-name>

Examples:

* To resolve the final archive url of the library `proper` at version `1.4.0`:

  resolve https://hex.pm/packages/proper 1.4.0 proper

* analyze ./path/to/proper/sources https://hex.pm/packages/proper 1.4.0 proper

">>]),
	erlang:halt().

resolve(Url0, Vsn0) -> 
  Url = binary:list_to_bin(Url0),
  Vsn = binary:list_to_bin(Vsn0),
  {ok, PkgSpec = {_PkgName, _PkgVsn}} = hexpm:parse_url(<<Url/binary, "/", Vsn/binary>>),
  #{
    version => 0,
    archive => #{ url => hexpm:archive_url(PkgSpec) }
   }.

tools() -> [mix, make, rebar3].

prepare(Root0, Pkg0) -> 
  {ok, Root} = path:new(Root0),
  Pkg = str:new(Pkg0),
	case file:consult(path:join(Root, "metadata.config")) of
		{ok, Metadata} -> do_prepare_from_hex_metadata(Metadata, Root);
		_ -> 
				case file:consult(path:join(Root, "rebar.config")) of
					{ok, RebarConf} -> generate_signatures_from_rebar_config(Pkg, RebarConf);
					_ -> #{ error => missing_config }
				end
	end.

generate_signatures_from_rebar_config(Pkg, RebarConf) ->
	Rebar = maps:from_list(RebarConf),
  #{
    version => 0,
    signatures => [
                   #{
										 name => <<":", Pkg/binary>>,
                     rule => <<"rebar3_library">>,
                     deps => [ begin
                                 case Vsn of
                                   {git, Repo, _Ref} -> str:new(string:replace(Repo, ".git", "", all));
                                   _ -> hexpm:pkg_to_url(Dep)
                                 end
                               end
                               || {Dep, Vsn} <- maps:get(deps, Rebar, []) ]
                    }
                  ]
   }.

do_prepare_from_hex_metadata(Metadata0, Root) -> 
  ?LOG_DEBUG("Opening metadata.config in ~p", [Root]),
  % 1. read CHECKSUM and validate?
  % 2. unpack contents.tar.gz
  % 3. read metadata.config 
  %
  Metadata = proplists:to_map(Metadata0),

  PkgName = maps:get(<<"app">>, Metadata),
  Srcs = get_files(Metadata, path:join(Root, PkgName)),

  ?LOG_DEBUG("Found ~p and expecting ~p sources", [PkgName, length(Srcs)]),
  ?LOG_DEBUG("Extracting..."),
  ok = erl_tar:extract(path:join(Root, "contents.tar.gz"), [compressed, {cwd, path:join(Root, PkgName)}]),
  ?LOG_DEBUG("Finished extracting files to ~p", [path:join(Root, PkgName)]),

  _AllTools = tools(),
  Tools = proplists:to_map([ erlang:binary_to_existing_atom(T, utf8)
                             || T <- maps:get(<<"build_tools">>, Metadata, []) ]),

  Reqs = requirements(Metadata),
  Deps = get_deps(Reqs),


  Rule = case Tools of
           #{ rebar3 := true } -> <<"rebar3_library">>;
           #{ mix := true } -> <<"mix_library">>;
           #{ make := true } -> <<"erlangmk_library">>;
           _ -> <<"mix_library">>
         end,

  #{
    version => 0,
    % NOTE(@ostera): useful for debugging
    % metadata => Metadata,
    signatures => [
                   #{
                     name => PkgName,
                     rule => Rule,
                     srcs => Srcs, 
                     deps => Deps
                    }
                  ]
   }.

get_files(Metadata, Root) -> handle_files(maps:get(<<"files">>, Metadata, []), _Acc = [], Root).

handle_files([], Acc, _Root) -> Acc;
handle_files([{Path, _Src}|T], Acc, Root) ->
  case filelib:is_file(path:join(Root, Path)) of
		true -> handle_files(T, [Path | Acc], Root);
		false -> handle_files(T, Acc, Root)
	end;
handle_files([Path|T], Acc, Root) -> handle_files(T, [Path | Acc], Root);
handle_files(Files, _Acc, _Root) when is_map(Files) -> maps:keys(Files).

requirements(Metadata) ->
  Reqs0 = case maps:get(<<"requirements">>, Metadata, []) of
            X when is_map(X) -> maps:to_list(X);
            Y -> Y
          end,
  Reqs = lists:map(fun
                     ({Name, Req}) -> [ {<<"name">>, Name} | Req ];
                     (Req) when is_list(Req) -> Req
                   end, Reqs0).

get_deps(Reqs) -> [ req_to_dep(proplists:to_map(Req)) || Req <- Reqs ].

req_to_dep(#{ <<"name">> := Name }) -> hexpm:pkg_to_url(Name).

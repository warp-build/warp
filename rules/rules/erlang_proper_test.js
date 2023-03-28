import ErlangToolchain, {
  BEAM_EXT,
  ERL_EXT,
  HEADER_EXT,
} from "https://rules.warp.build/toolchains/erlang.js";

const impl = (ctx) => {
  const { target, name, deps, test, props } = ctx.cfg();

  const extraLibPaths = ctx
    .transitiveDeps()
    .flatMap((dep) => dep.outs)
    .filter((path) => path.endsWith(BEAM_EXT))
    .map(File.parent)
    .map((path) => `"${path}"`)
    .unique();

  const cwd = Target.path(target);
  const runner = `${cwd}/${name}_prop_runner.erl`;

  /*
   * NOTE(@ostera): we create a script that runs proper and spits out the results.
   */
  ctx.action().writeFile({
    dst: runner,
    data: `
-module(${name}_prop_runner).

-include_lib("kernel/include/logger.hrl").

-export([main/1]).

main(_argv) ->
  logger:set_primary_config(#{ level => all }),
  logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
  ?LOG_INFO("Starting prop runner for: ${cwd}/${name}"),

  CodePaths = [ ${extraLibPaths.join(", \n")} ],
  [ code:add_path(P) || P <- CodePaths ],

  Props = [
    ${
      props
        .map((prop) =>
          `{${File.filename(test).replace(ERL_EXT, "")} , ${prop}}`
        )
        .join(",\n")
    }
  ],

  ?LOG_INFO("Running ~p properties:\n", [length(Props)]),

  Opts = [],

  Failed = lists:filtermap(fun ({Mod, Fun}) ->
              ?LOG_INFO("* ~p:~p/0\n", [Mod, Fun]),
              Result = catch proper:quickcheck(Mod:Fun(), Opts),
              case Result of
                true -> false;
                _ ->
                  ?LOG_ERROR("Test failed: ~p\n", [Result]),
                  {true, {Mod, Fun, Result, proper:counterexample()}}
              end
            end, Props),

  case Failed of
    [] -> 
      ?LOG_INFO("OK"),
      ok;

    _ ->
      ?LOG_ERROR("Failed tests: \n~p\n", [Failed]),
      erlang:halt(1)
  end.
  
`,
  });

  ctx.action().runShell({
    script: `

escript ${runner} || exit 1

  `,
  });

  ctx.action().declareOutputs([]);
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/erlang_proper_test",
  mnemonic: "PropErTest",
  impl,
  cfg: {
    name: target(),
    test: file(),
    props: [string()],
    deps: [target()],
  },
  defaults: {
    deps: [],
    props: [],
  },
  toolchains: [ErlangToolchain],
});

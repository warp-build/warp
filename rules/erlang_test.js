import ErlangToolchain, {HEADER_EXT, BEAM_EXT, ERL_EXT} from "https://pkgs.warp.build/toolchains/erlang.js";

const impl = ctx => {
  const { name, deps, test, cwd } = ctx.cfg();

  const transitiveDeps = ctx.transitiveDeps().flatMap(dep => dep.outs);
  const extraLibPaths = transitiveDeps
    .filter(path => path.endsWith(BEAM_EXT))
    .map(File.parent)
    .unique()
    .flatMap(dir => ["-pa", dir])

  ctx.action().runShell({ script: `

ls -la ./tools/erlang/lift/semver/*

ct_run \
  -dir ${cwd()} \
  -suite ${name} \
  -erl_args \
    -sname ${name} \
    ${extraLibPaths.join(" ")}

  `});
  ctx.action().declareOutputs([]);
};

export default Warp.Rule({
  name: "https://pkgs.warp.build/rules/erlang_test",
  mnemonic: "ErlTest",
  impl,
  cfg: {
    name: label(),
    test: file(),
    deps: [label()],
  },
  defaults: {
    deps: [],
  },
  toolchains: [ErlangToolchain]
});

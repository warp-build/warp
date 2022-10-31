import ErlangToolchain, {HEADER_EXT, BEAM_EXT, ERL_EXT} from "https://rules.warp.build/toolchains/erlang.js";

const impl = ctx => {
  const { name, deps, test, cases, cwd } = ctx.cfg();

  const transitiveDeps = ctx.transitiveDeps().flatMap(dep => dep.outs);
  const runtimeDeps = ctx.runtimeDeps().flatMap(dep => dep.outs);

  const extraLibPaths = transitiveDeps
    .concat(runtimeDeps)
    .filter(path => path.endsWith(BEAM_EXT))
    .map(File.parent)
    .unique()
    .flatMap(dir => ["-pa", ctx.path(dir)])

  ctx.action().runShell({ script: `

ct_run \
  -dir ${cwd()} \
  -verbosity 100 \
  -no-auto-compile \
  -suite ${test.replace(ERL_EXT, "")} \
  ${(cases.length > 0) ? `-case ${cases.join(" ")}` : ""} \
  -erl_args \
    -sname ${name}-$(uuidgen) \
    ${extraLibPaths.join(" ")}

  `});

  ctx.action().declareOutputs([]);
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/erlang_test",
  mnemonic: "ErlTest",
  impl,
  cfg: {
    name: label(),
    test: file(),
    cases: [string()],
    deps: [label()],
  },
  defaults: {
    deps: [],
    cases: [],
  },
  toolchains: [ErlangToolchain]
});

import {TAR_EXT} from "../rules/archive.js";
import ElixirToolchain from "../toolchains/elixir.js";
import ErlangToolchain, {BEAM_EXT} from "../toolchains/erlang.js";

const impl = ctx => {
  const { label, name, srcs, deps, dot_iex, elixirc_opts} = ctx.cfg();

  const { IEX } = ElixirToolchain.provides()

  const transitiveDeps = ctx.transitiveDeps()

  const extraPaths = transitiveDeps
    .flatMap(dep => [
      `${Label.path(dep.label)}/_build/prod/lib/${Label.name(dep.label)}/ebin`,
      ...dep.outs.filter(out => out.endsWith(BEAM_EXT)) .map(path => File.parent(path)),
    ])
    .unique()
    .sort()
  // NOTE(@ostera): how do we get zap-outputs, but nice?
    .flatMap(path => ["-pa", `zap-outputs/${path}`])
    .join(" ")

  const run = `${Label.path(label)}/run_shell`

  ctx.action().declareOutputs([run, dot_iex])
  ctx.action().declareRunScript(run)
  ctx.action().writeFile({
    dst: run,
    data: `#!/bin/bash -e

${
  transitiveDeps.flatMap(dep => dep.outs.flatMap(out => {
      if (out.endsWith(TAR_EXT)) {
        return [`cd zap-outputs/${Label.path(dep.label)}; tar xf ${File.filename(out)}; cd - > /dev/null;`];
      } else {
        return []
      }
  })).join("\n")
}

${IEX} \
  --dot-iex $(dirname "\${BASH_SOURCE[0]}")/${File.filename(dot_iex)} \
  ${extraPaths}
  
`,
  })
  ctx.action().runShell({
    needsTty: true,
    script: `#!/bin/bash -xe

chmod +x ${run}

`
  })
};

export default Zap.Rule({
  runnable: true,
  name: "iex_shell",
  mnemonic: "IExShell",
  impl,
  cfg: {
    name: label(),
    dot_iex: file(),
    deps: [label()],
  },
	defaults: {
    dot_iex: "",
		deps: [],
	},
  toolchains: [ElixirToolchain, ErlangToolchain]
});

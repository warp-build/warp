import ElixirToolchain, {BEAM_EXT, EX_EXT} from "../toolchains/elixir.js";

const impl = ctx => {
  const { label, name, srcs, deps, main } = ctx.cfg();

  const { ELIXIR } = ElixirToolchain.provides()

  const transitiveDeps = ctx.transitiveDeps()

  const extraPaths = transitiveDeps
    .flatMap(dep => [
      `${Label.path(dep.label)}/_build/dev/lib/${Label.name(dep.label)}/ebin`,
      ...dep.outs.filter(out => out.endsWith(BEAM_EXT)) .map(path => File.parent(path)),
    ])
    .unique()
    .flatMap(path => ["-pa", path])
    .join(" ")

  transitiveDeps.forEach(dep => {
    dep.outs.forEach(out => {
      if (out.endsWith(TAR_EXT)) {
        ctx.action().exec({
          cmd: "tar",
          args: ["xf", File.filename(out)],
          cwd: Label.path(dep.label)
        })
      }
    })
  });

  const run = `${Label.path(label)}/run_script`

  ctx.action().declareOutputs([run, main])
  ctx.action().declareRunScript(run)
  ctx.action().writeFile({
    dst: run,
    data: `#!/bin/bash -e

${ELIXIR} \
  ${extraPaths} \
  $(dirname "\${BASH_SOURCE[0]}")/${File.filename(main)}

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
  name: "elixir_script",
  mnemonic: "ExScript",
  impl,
  cfg: {
    name: label(),
    main: file(),
  },
	defaults: {
	},
  toolchains: [ElixirToolchain]
});


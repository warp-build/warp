import {TAR_EXT} from "../rules/archive.js";
import ElixirToolchain from "../toolchains/elixir.js";

const impl = ctx => {
  const { label, name, srcs, deps, dot_iex, elixirc_opts} = ctx.cfg();

  const { IEX } = ElixirToolchain.provides()

  const transitiveDeps = ctx.transitiveDeps()

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

  const run = `${Label.path(label)}/run2`

  ctx.action().declareOutputs([run])
  ctx.action().writeFile({
    dst: run,
    data: `#!/bin/bash -e

# echo $(pwd)
# tree -L 9 -C .

${IEX} \
  --dot-iex ${dot_iex} \
  $(find . -name "ebin" -type d | sort -u | sed 's/\\(.*\\)/-pa \\1/' | xargs)
`,
  })
  ctx.action().runShell({
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
  toolchains: [ElixirToolchain]
});

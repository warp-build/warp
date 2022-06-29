const impl = ctx => {
  const { label, name, deps, srcs, outs } = ctx.cfg();

  const root = Label.path(label);
  ctx.action().declareOutputs(outs.map(out => File.join(root, out)));

  ctx.action().runShell({
    script: `#!/bin/bash

cd ${Label.path(label)}
make clean all

`,
  })
};

export default Warp.Rule({
  name: "make_library",
  mnemonic: "MakeLib",
  impl,
  cfg: {
    name: label(),
    srcs: [file()],
    outs: [string()],
    deps: [label()],
  },
	defaults: {
    srcs: [ "./**/*" ],
    outs: [],
    deps: [],
	},
  toolchains: []
});


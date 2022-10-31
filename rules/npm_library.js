const impl = ctx => {
  const { label, name, deps, srcs, } = ctx.cfg();
  const cwd = Label.path(label)

  const tarball = `${name}.node_module.tar`;
  const outputs = [`${cwd}/${tarball}`];
  ctx.action().declareOutputs(outputs);

  ctx.action().runShell({
    script: `#!/bin/bash -xe

cd ${cwd}
tar cf ${tarball} .

`,
  });
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/npm_library",
  mnemonic: "NpmLib",
  impl,
  cfg: {
    name: label(),
    srcs: [file()],
    deps: [label()],
  },
  defaults: {
    srcs: [
      "package.json",
    ],
    deps: [],
  }
});


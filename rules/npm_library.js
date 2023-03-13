const impl = (ctx) => {
  const { target, name, deps, srcs } = ctx.cfg();
  const cwd = Target.path(target);

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
    name: target(),
    srcs: [file()],
    deps: [target()],
  },
  defaults: {
    srcs: ["package.json"],
    deps: [],
  },
});

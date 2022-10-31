import RustToolchain from "https://rules.warp.build/toolchains/rust.js";

const impl = ctx => {
  const { cwd, label, name, src, bin } = ctx.cfg();

  const exe = `${cwd()}/target/debug/${name}`
  ctx.action().declareOutputs([exe]);
  ctx.action().declareRunScript(exe);

  const { cargo } = RustToolchain.provides()
  ctx.action().runShell({
    script: `#!/bin/bash

cd ${cwd}
${cargo} build --bin ${bin}

`,
  })
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/cargo_binary",
  mnemonic: "CargoBin",
  impl,
  cfg: {
    name: label(),
    src: file(),
    bin: string(),
    deps: [label()],
  },
  defaults: {
    deps: [],
  },
  toolchains: [RustToolchain]
});

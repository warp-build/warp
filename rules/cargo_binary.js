import RustToolchain from "https://rules.warp.build/toolchains/rust.js";

const impl = ctx => {
  const { cwd, label, name, bin } = ctx.cfg();

  const exe = `${cwd()}/target/debug/${bin}`
  ctx.action().declareOutputs([exe]);
  ctx.action().declareRunScript(exe);

  ctx.action().runShell({
    script: `#!/bin/bash

cd ${cwd()}
cargo build

`,
  })
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/cargo_binary",
  mnemonic: "CargoBin",
  impl,
  cfg: {
    name: label(),
    bin: string(),
    srcs: [file()],
    deps: [label()],
  },
  defaults: {
    deps: [],
  },
  toolchains: [RustToolchain]
});

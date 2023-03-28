import RustToolchain from "https://rules.warp.build/toolchains/rust.js";

const impl = (ctx) => {
  const { cwd, target, name, bin } = ctx.cfg();

  const exe = `${cwd()}/target/debug/${bin}`;
  ctx.action().declareOutputs([exe]);
  ctx.provides({ [bin]: exe });

  ctx.action().runShell({
    script: `#!/bin/bash

cd ${cwd()}
cargo build

`,
  });
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/cargo_binary",
  mnemonic: "CargoBin",
  impl,
  cfg: {
    name: target(),
    bin: string(),
    srcs: [file()],
    deps: [target()],
  },
  defaults: {
    srcs: [
      "src/**/*",
      "test/**/*",
      "build.rs",
      "Cargo.toml",
    ],
    deps: [],
  },
  toolchains: [RustToolchain],
});

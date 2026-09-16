import RustToolchain from "https://rules.warp.build/toolchains/rust.js";

const impl = (ctx) => {
  const { cwd, target, name, bin } = ctx.cfg();

  ctx
    .action()
    .declareOutputs([
      `${cwd()}/target/debug/lib${bin}.d`,
      `${cwd()}/target/debug/lib${bin}.rlib`,
    ]);

  ctx.action().runShell({
    script: `#!/bin/bash

cd ${cwd()}
cargo build --lib

`,
  });
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/cargo_library",
  mnemonic: "CargoLib",
  impl,
  cfg: {
    name: target(),
    srcs: [file()],
    deps: [target()],
  },
  defaults: {
    deps: [],
  },
  toolchains: [RustToolchain],
});

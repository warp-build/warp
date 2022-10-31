export const JS_EXT = ".js";
export const TS_EXT = ".ts";

const impl = ctx => {
  const { host } = ctx.env();
  const { version, sha1_aarch64, sha1_x86_64 } = ctx.cfg();

  let arch = host.arch
  let sha1 = sha1_aarch64
  if (arch === "x86_64") {
    sha1 = sha1_x86_64
  }

  const url = `https://static.rust-lang.org/rustup/dist/aarch64-apple-darwin/rustup-init`
  const rustup_init = "rustup-init"

  ctx.action().download({ url, sha1, output: rustup_init })
  ctx.action().setPermissions({ file: rustup_init, executable: true })

  ctx.action().writeFile({ dst: "version", data: "0" });
  ctx.action().runShell({
    script: `

RUSTUP_HOME=$(pwd)/.rustup
./${rustup_init} -y --no-modify-path

`
  });
};

export default Warp.Toolchain({
  name: "https://rules.warp.build/toolchains/rust",
  mnemonic: "Rust",
  impl,
  cfg: {
    version: string(),
    sha1_aarch64: string(),
  }
});

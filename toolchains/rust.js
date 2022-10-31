export const JS_EXT = ".js";
export const TS_EXT = ".ts";

const impl = ctx => {
  const { host } = ctx.env();
  const { cwd, sha1_aarch64, toolchains, targets, profile, components } = ctx.cfg();

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

export RUSTUP_HOME=$(pwd)/.rustup

./${rustup_init} -y --no-modify-path \
  --profile ${profile} \
  --default-toolchain ${toolchains[0]} \
  ${targets.map(t => `-t ${t}`).join("\ \n")}
  ${components.map(c => `-c ${c}`).join("\ \n")}

`
  });

  let outputs = [
    `.rustup/settings.toml`,
    ...toolchains.flatMap(t => [
      `.rustup/toolchains/${t}/bin`,
      `.rustup/toolchains/${t}/lib`,
      `.rustup/toolchains/${t}/libexec`,
    ])
  ]

  ctx.action().declareOutputs(outputs);

  let root = `.rustup/toolchains/${toolchains[0]}/bin`;
  ctx.provides({
    "cargo": `${root}/cargo`,
    "cargo-clippy": `${root}/cargo-clippy`,
    "cargo-fmt": `${root}/cargo-fmt`,
    "clippy-driver": `${root}/clippy-driver`,
    "rust-gdb": `${root}/rust-gdb`,
    "rust-gdbgui": `${root}/rust-gdbgui`,
    "rust-lldb": `${root}/rust-lldb`,
    "rustc": `${root}/rustc`,
    "rustdoc": `${root}/rustdoc`,
    "rustfmt": `${root}/rustfmt`,
  });
};

export default Warp.Toolchain({
  name: "https://rules.warp.build/toolchains/rust",
  mnemonic: "Rust",
  impl,
  cfg: {
    profile: string(),
    toolchains: [string()],
    targets: [string()],
    components: [string()],
    sha1_aarch64: string(),
  },
  defaults: {
    components: [],
    profile: "default",
  },
});

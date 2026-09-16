export const RS_EXT = ".rs";
export const RLIB_EXT = ".rlib";
export const TOML_EXT = ".toml";

const impl = (ctx) => {
  const { host } = ctx.env();
  const {
    channel,
    components,
    cwd,
    sha256_linux_aarch64,
    sha256_linux_x86_64,
    sha256_macos_aarch64,
    sha256_macos_x86_64,
    targets,
  } = ctx.cfg();

  let arch = host.arch;
  let sha256 = sha256_macos_aarch64;
  if (arch === "x86_64" && host.os == "darwin") {
    sha256 = sha256_macos_x86_64;
  }
  if (arch === "x86_64" && host.os == "linux") {
    sha256 = sha256_linux_x86_64;
  }
  if (arch === "aarch64" && host.os == "linux") {
    sha256 = sha256_linux_aarch64;
  }

  const url =
    `https://static.rust-lang.org/rustup/dist/${host.triple}/rustup-init`;
  const rustup_init = "rustup-init";

  ctx.action().download({ url, sha256, output: rustup_init });
  ctx.action().setPermissions({ file: rustup_init, executable: true });

  const defaultToolchain = `${channel}-${host.triple}`;
  ctx.action().writeFile({ dst: "version", data: "0" });
  ctx.action().runShell({
    script: `

export RUSTUP_HOME=$(pwd)/.rustup

./${rustup_init} -y --no-modify-path \
  --default-toolchain ${defaultToolchain} \
  --target ${targets.join(" ")} \
  --component ${components.join(" ")}
`,
  });

  let outputs = [
    `.rustup/settings.toml`,
    `.rustup/toolchains/${defaultToolchain}/bin`,
    `.rustup/toolchains/${defaultToolchain}/lib`,
    `.rustup/toolchains/${defaultToolchain}/libexec`,
  ];

  ctx.action().declareOutputs(outputs);

  let root = `.rustup/toolchains/${defaultToolchain}/bin`;
  ctx.provides({
    cargo: `${root}/cargo`,
    "cargo-clippy": `${root}/cargo-clippy`,
    "cargo-fmt": `${root}/cargo-fmt`,
    "clippy-driver": `${root}/clippy-driver`,
    "rust-gdb": `${root}/rust-gdb`,
    "rust-gdbgui": `${root}/rust-gdbgui`,
    "rust-lldb": `${root}/rust-lldb`,
    rustc: `${root}/rustc`,
    rustdoc: `${root}/rustdoc`,
    rustfmt: `${root}/rustfmt`,
  });
};

export default Warp.Toolchain({
  name: "https://rules.warp.build/toolchains/rust",
  mnemonic: "Rust",
  impl,
  cfg: {
    channel: string(),
    profile: string(),
    targets: [string()],
    components: [string()],
    sha256_macos_aarch64: string(),
    sha256_macos_x86_64: string(),
    sha256_linux_aarch64: string(),
    sha256_linux_x86_64: string(),
  },
  defaults: {
    channel: "nightly-2023-04-11",
    profile: "default",
    sha256_macos_aarch64: "71939cab8697adf952e7021bb2e89cd61aca55cc",
    sha256_macos_x86_64: "2d31e91ea76580511281c1d5d9fac81ea42db78d",
    sha256_linux_aarch64: "5fd9e67e4fa696d35fa8393b0ad4f742267d1d52",
    sha256_linux_x86_64: "682fd896e1f0cbad9f6689ba7f8a8f3d21efb4df",
    targets: [
      "aarch64-apple-darwin",
      "aarch64-unknown-linux-gnu",
      "x86_64-apple-darwin",
      "x86_64-unknown-linux-gnu",
    ],
    components: [
      "clippy",
    ],
  },
});

export const RS_EXT = ".rs";
export const TOML_EXT = ".toml";

const impl = (ctx) => {
  const { host } = ctx.env();
  const {
    cwd,
    sha1_aarch64,
    sha1_x86_64,
    toolchains,
    targets,
    components,
  } = ctx.cfg();

  let arch = host.arch;
  let sha1 = sha1_aarch64;
  if (arch === "x86_64") {
    sha1 = sha1_x86_64;
  }

  const url = `https://static.rust-lang.org/rustup/dist/${host.triple}/rustup-init`;
  const rustup_init = "rustup-init";

  ctx.action().download({ url, sha1, output: rustup_init });
  ctx.action().setPermissions({ file: rustup_init, executable: true });

  const defaultToolchain = toolchains.find((t) => t.includes(host.triple));
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
    toolchains: [string()],
    targets: [string()],
    components: [string()],
    sha1_aarch64: string(),
    sha1_x86_64: string(),
  },
});

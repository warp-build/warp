import ErlangToolchain from "https://rules.warp.build/toolchains/erlang.js";

export const BEAM_EXT = ".beam";
export const EX_EXT = ".ex";
export const EXS_EXT = ".exs";

const cleanVersion = (version) => version.split(".").slice(0, 2).join("-");

const impl = (ctx) => {
  const { host } = ctx.env();
  const {
    kind,
    version,
    sha256_macos_aarch64,
    sha256_macos_x86_64,
    sha256_linux_x86_64,
    sha256_linux_aarch64,
  } = ctx.cfg();

  const output = "Precompiled.zip";

  const url = kind === "source"
    ? `https://github.com/elixir-lang/elixir/archive/v${version}.tar.gz`
    : `https://github.com/elixir-lang/elixir/releases/download/v${version}/Precompiled.zip`;

  let sha256 = sha256_macos_aarch64;
  if (host.arch === "x86_64" && host.os == "darwin") {
    sha256 = sha256_macos_x86_64;
  }
  if (host.arch === "x86_64" && host.os == "linux") {
    sha256 = sha256_linux_x86_64;
  }
  if (host.arch === "aarch64" && host.os == "linux") {
    sha256 = sha256_linux_aarch64;
  }

  ctx.action().download({ url, sha256, output });

  ctx.action().extract({ src: output, dst: "." });

  let elixir_path = `elixir-${version}`;
  let MIX_HOME = `.mix`;
  let MIX_ARCHIVES = `${MIX_HOME}/archives`;
  let MIX_REBAR3 = `${MIX_HOME}/elixir/${cleanVersion(version)}/rebar3`;

  if (kind === "source") {
    ctx.action().runShell({
      env: {
        MIX_HOME,
        MIX_ARCHIVES,
      },
      script: `

mkdir -p ${MIX_ARCHIVES}

make -C ${elixir_path}

# NOTE(@ostera): to avoid separately handling this, we will force-install hex
# at this stage.
${elixir_path}/bin/mix local.hex --force
${elixir_path}/bin/mix local.rebar --force

`,
    });
  }

  const binRoot = `elixir-${version}/bin`;
  const elixir = File.join(binRoot, "elixir");
  const elixirc = File.join(binRoot, "elixirc");
  const iex = File.join(binRoot, "iex");
  const mix = File.join(binRoot, "mix");

  ctx.action().setPermissions({ file: elixir, executable: true });
  ctx.action().setPermissions({ file: elixirc, executable: true });
  ctx.action().setPermissions({ file: iex, executable: true });
  ctx.action().setPermissions({ file: mix, executable: true });

  ctx.action().declareOutputs([elixir_path, MIX_HOME]);

  ctx.provides({ elixir, elixir, elixirc, iex, mix });

  ctx.setEnv({
    ELIXIR_HOME: ctx.path(binRoot),
    MIX_ARCHIVES: ctx.path(MIX_ARCHIVES),
    MIX_REBAR3: ctx.path(MIX_REBAR3),
    MIX_HOME: ctx.path(MIX_HOME),
  });
};

export default Warp.Toolchain({
  name: "https://rules.warp.build/toolchains/elixir",
  mnemonic: "Elixir/OTP",
  impl,
  cfg: {
    version: string(),
    sha256_macos_aarch64: string(),
    sha256_linux_aarch64: string(),
    sha256_macos_x86_64: string(),
    sha256_linux_x86_64: string(),
    kind: string(),
  },
  defaults: {
    kind: "source",
    sha256_macos_aarch64: "9f754f439a0d280b0e13803eb9263d3051142b00",
    sha256_linux_aarch64: "0c777d4fa21819b86dc55818bd5bad59e73a4167",
    sha256_macos_x86_64: "0c777d4fa21819b86dc55818bd5bad59e73a4167",
    sha256_linux_x86_64: "0c777d4fa21819b86dc55818bd5bad59e73a4167",
    version: "1.14",
  },
  toolchains: [ErlangToolchain],
});

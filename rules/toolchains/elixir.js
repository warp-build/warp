import ErlangToolchain from "https://rules.warp.build/toolchains/erlang.js";

export const BEAM_EXT = ".beam";
export const EX_EXT = ".ex";
export const EXS_EXT = ".exs";

const cleanVersion = (version) => version.split(".").slice(0, 2).join("-");

const impl = (ctx) => {
  const { kind, version, sha256 } = ctx.cfg();

  const output = "Precompiled.zip";

  const url = kind === "source"
    ? `https://github.com/elixir-lang/elixir/archive/v${version}.tar.gz`
    : `https://github.com/elixir-lang/elixir/releases/download/v${version}/Precompiled.zip`;

  ctx.action().download({ url, sha1: sha256, output });

  ctx.action().extract({ src: output, dst: "." });

  let elixir_path = `elixir-${version}`;
  let MIX_HOME = `${elixir_path}/.mix`;
  let MIX_ARCHIVES = `${elixir_path}/.mix/archives`;
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

  ctx.action().declareOutputs([elixir_path]);

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
    sha256: string(),
    kind: string(),
  },
  defaults: {
    kind: "source",
    sha256: "6addcf88ec333fc9d0468e84abc5c8fb053d8f79",
    version: "1.14",
  },
  toolchains: [ErlangToolchain],
});

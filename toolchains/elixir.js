import ErlangToolchain from "https://pkgs.warp.build/toolchains/erlang.js";

export const BEAM_EXT = ".beam";
export const EX_EXT = ".ex";
export const EXS_EXT = ".exs";

const impl = ctx => {
  const { kind, version, sha1 } = ctx.cfg();

  const output = "Precompiled.zip"

  const url =
    kind === "source"
    ? `https://github.com/elixir-lang/elixir/archive/v${version}.tar.gz`
    : `https://github.com/elixir-lang/elixir/releases/download/v${version}/Precompiled.zip`

  ctx.action().download({ url, sha1, output })

  ctx.action().extract({ src: output, dst: "." })

  let elixir_path = `elixir-${version}`
  let MIX_ARCHIVES = `${elixir_path}/.mix/archives`;

  if (kind === "source") {
    ctx.action().runShell({
      env: {
        MIX_ARCHIVES,
      },
      script: `

mkdir -p ${MIX_ARCHIVES}

make -C ${elixir_path}

# NOTE(@ostera): to avoid separately handling this, we will force-install hex
# at this stage.
${elixir_path}/bin/mix local.hex --force
${elixir_path}/bin/mix local.rebar --force

`
    })
  }

  const binRoot = `elixir-${version}/bin`;
  const elixir = File.join(binRoot, "elixir");
  const elixirc = File.join(binRoot, "elixirc");
  const iex = File.join(binRoot, "iex");
  const mix = File.join(binRoot, "mix");

  ctx.action().setPermissions({ file: elixir, executable: true })
  ctx.action().setPermissions({ file: elixirc, executable: true })
  ctx.action().setPermissions({ file: iex, executable: true })
  ctx.action().setPermissions({ file: mix, executable: true })

  ctx.action().declareOutputs([elixir_path]);

  ctx.provides({ elixir, elixir, elixirc, iex, mix });

  ctx.setEnv({
    ELIXIR_HOME: ctx.path(binRoot),
    MIX_ARCHIVES: ctx.path(MIX_ARCHIVES),
  });
};

export default Warp.Toolchain({
  name: "https://pkgs.warp.build/toolchains/elixir",
  mnemonic: "Elixir/OTP",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
    kind: string(),
  },
  toolchains: [ErlangToolchain]
});

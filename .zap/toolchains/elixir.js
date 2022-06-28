import ErlangToolchain from "./erlang.js";

export const BEAM_EXT = ".beam";
export const EX_EXT = ".ex";
export const EXS_EXT = ".exs";

const impl = ctx => {
  const { version, sha1 } = ctx.cfg();

  const output = "Precompiled.zip"

  const url = `https://github.com/elixir-lang/elixir/releases/download/v${version}/Precompiled.zip`

  ctx.action().download({ url, sha1, output })

  ctx.action().extract({ src: output, dst: "." })

  const binRoot = "bin";
  const ELIXIR = File.join(binRoot, "elixir");
  const ELIXIRC = File.join(binRoot, "elixirc");
  const IEX = File.join(binRoot, "iex");
  const MIX = File.join(binRoot, "mix");

  ctx.action().setPermissions({ file: ELIXIR, executable: true })
  ctx.action().setPermissions({ file: ELIXIRC, executable: true })
  ctx.action().setPermissions({ file: IEX, executable: true })
  ctx.action().setPermissions({ file: MIX, executable: true })

  ctx.action().declareOutputs([]);

  ctx.provides({ ELIXIR, ELIXIRC, IEX, MIX });
};

export default Zap.Toolchain({
  name: "//zap.build/toolchains:elixir",
  mnemonic: "Elixir/OTP",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
  },
  toolchains: [ErlangToolchain]
});

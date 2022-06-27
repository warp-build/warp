import ErlangToolchain from "./erlang.js";

export const BEAM_EXT = ".beam";
export const EX_EXT = ".ex";
export const EXS_EXT = ".exs";

const impl = ctx => {
  const { unarchivedRoot, archiveKind } = ctx.cfg();

  console.log(ctx.cfg());

  if (archiveKind === "source") {
    ctx.action().exec({
      cmd: "make",
      args: [],
      cwd: unarchivedRoot
    });
  }

  const binRoot = File.join(unarchivedRoot, "bin");
  const ELIXIR = File.join(binRoot, "elixir");
  const ELIXIRC = File.join(binRoot, "elixirc");
  const IEX = File.join(binRoot, "iex");
  const MIX = File.join(binRoot, "mix");
  ctx.provides({ ELIXIR, ELIXIRC, IEX, MIX });
  ctx.action().declareOutputs([]);
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

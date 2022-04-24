export const BEAM_EXT = ".beam";
export const EX_EXT = ".ex";
export const EXS_EXT = ".exs";

const impl = ctx => {
  const { unarchivedRoot, archiveKind } = ctx.cfg();

  if (archiveKind === "source") {
    ctx.action().exec({
      cmd: "make",
      args: [],
      cwd: unarchivedRoot
    });
  }

  const binRoot = File.join(unarchivedRoot, "bin");
  const ELIXIRC = File.join(binRoot, "elixirc");
  const IEX = File.join(binRoot, "iex");
  ctx.provides({ ELIXIRC, IEX });
  ctx.action().declareOutputs([]);
};

export default Zap.Toolchain({
  name: "//zap.build/toolchains:elixir",
  mnemonic: "Elixir/OTP",
  impl,
});

export const BEAM_EXT = ".beam";
export const EX_EXT = ".ex";
export const EXS_EXT = ".exs";

const impl = ctx => {
  const { unarchivedRoot } = ctx.cfg();
  const binRoot = unarchivedRoot.join("bin");
  const ELIXIRC = binRoot.join("elixirc");
  const IEX = binRoot.join("iex");
  ctx.provides({ ELIXIRC, IEX });
  ctx.action().declareOutputs([]);
};

export default Zap.Toolchain({
  name: "//zap.build/toolchains:elixir",
  mnemonic: "Elixir/OTP",
  impl,
});

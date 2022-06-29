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

export default Warp.Toolchain({
  name: "//warp.build/toolchains:elixir",
  mnemonic: "Elixir/OTP",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
  },
});

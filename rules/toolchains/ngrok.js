const impl = (ctx) => {
  const { unarchivedRoot } = ctx.cfg();

  let binRoot = unarchivedRoot;
  const GLEAM = File.join(binRoot, "gleam");
  ctx.provides({ GLEAM });
  ctx.action().declareOutputs([]);
};

export default Warp.Toolchain({
  name: "https://rules.warp.build/toolchains/ngrok",
  mnemonic: "ngrok",
  impl,
  cfg: {
    version: string(),
    sha256: string(),
  },
});

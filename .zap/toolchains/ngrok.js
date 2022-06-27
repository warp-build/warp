const impl = ctx => {
  const { unarchivedRoot } = ctx.cfg();

  let binRoot = unarchivedRoot
  const GLEAM = File.join(binRoot, "gleam");
  ctx.provides({ GLEAM });
  ctx.action().declareOutputs([]);
};

export default Zap.Toolchain({
  name: "//zap.build/toolchains:ngrok",
  mnemonic: "ngrok",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
  },
});



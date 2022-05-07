const impl = ctx => {
  const { srcs } = ctx.cfg();
  ctx.action().declareOutputs(srcs);
};

export default Zap.Rule({
  name: "files",
  mnemonic: "Files",
  impl,
  cfg: {
    name: label(),
    srcs: [file()],
  }
});

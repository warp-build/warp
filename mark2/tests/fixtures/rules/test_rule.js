const impl = (ctx) => {
  const { srcs } = ctx.cfg();
  ctx.action().declareOutputs(srcs);
};

export default Warp.Rule({
  name: "test_rule",
  mnemonic: "TestRule",
  impl,
  cfg: {
    name: target(),
    srcs: [file()],
  },
});

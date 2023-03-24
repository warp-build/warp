const impl = (ctx) => {
  const { srcs } = ctx.cfg();
  ctx.action().declareOutputs(srcs);
};

export default Warp.Rule({
  name: "{URL}/test_rule",
  mnemonic: "TestRule",
  impl,
  cfg: {
    name: target(),
    srcs: [file()],
  },
});

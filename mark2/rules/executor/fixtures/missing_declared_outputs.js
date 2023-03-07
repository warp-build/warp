const impl = (ctx) => {
  // NOTE(@ostera): this rule should fail precisely because we are not calling:
  //  ctx.action().declareOutputs(srcs);
};

export default Warp.Rule({
  name: "test_rule",
  mnemonic: "TestRule",
  impl,
  cfg: {
    name: target(),
  },
});

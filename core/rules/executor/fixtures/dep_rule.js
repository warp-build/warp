const impl = (ctx) => {
  const { srcs } = ctx.cfg();
  ctx.action().declareOutputs(srcs);
};

export default Warp.Rule({
  name: "{URL}/dep_rule",
  mnemonic: "DepRule",
  impl,
  cfg: {
    name: target(),
    srcs: [file()],
  },
});

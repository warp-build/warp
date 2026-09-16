const impl = (ctx) => {
  ctx.action().declareOutputs([]);
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/dummy",
  mnemonic: "Dummy",
  impl,
  cfg: {
    name: label(),
  },
  toolchains: [],
});

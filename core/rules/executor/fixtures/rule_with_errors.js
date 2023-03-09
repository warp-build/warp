const impl = (ctx) => {
  panic("oh no");
};

export default Warp.Rule({
  name: "test_rule",
  mnemonic: "TestRule",
  impl,
  cfg: {
    name: target(),
  },
});

const impl = (ctx) => {
  panic("oh no");
};

export default Warp.Rule({
  name: "{URL}/test_rule",
  mnemonic: "TestRule",
  impl,
  cfg: {
    name: target(),
  },
});

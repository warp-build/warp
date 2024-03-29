import ElixirToolchain from "https://rules.warp.build/toolchains/elixir.js";

const impl = (ctx) => {
  const { name, deps, srcs } = ctx.cfg();

  ctx.action().declareOutputs(srcs);
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/eex_library",
  mnemonic: "EExLib",
  impl,
  cfg: {
    name: target(),
    deps: [target()],
    srcs: [file()],
  },
  defaults: {
    srcs: ["*.eex", "lib/**/*.eex"],
    deps: [],
  },
  toolchains: [ElixirToolchain],
});

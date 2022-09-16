import ElixirToolchain from "../toolchains/elixir.js";

const impl = ctx => {
  const { name, deps, srcs, } = ctx.cfg();

  ctx.action().declareOutputs(srcs);
};

export default Warp.Rule({
  name: "eex_library",
  mnemonic: "EExLib",
  impl,
  cfg: {
    name: label(),
    deps: [label()],
    srcs: [file()],
  },
	defaults: {
    srcs: [ "*.eex", "lib/**/*.eex" ],
		deps: [],
	},
  toolchains: [ElixirToolchain]
});


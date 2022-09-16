import ElixirToolchain, {BEAM_EXT} from "../toolchains/elixir.js";

const impl = ctx => {
  const { name, deps, srcs, } = ctx.cfg();
  let img = `${name}.image}`
  ctx.action().writeFile({
    dst: img,
    data: `${name}`
  })
  ctx.action().declareOutputs([img]);
};

export default Warp.Rule({
  name: "elixir_image",
  mnemonic: "ExImage",
  impl,
  cfg: {
    name: label(),
    deps: [label()],
  },
	defaults: {
		deps: [],
	},
  toolchains: [ElixirToolchain]
});


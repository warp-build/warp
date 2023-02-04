import ElixirToolchain, {
  BEAM_EXT,
} from "https://rules.warp.build/toolchains/elixir.js";

const impl = (ctx) => {
  const { name, deps, srcs } = ctx.cfg();
  let img = `${name}.image}`;
  ctx.action().writeFile({
    dst: img,
    data: `${name}`,
  });
  ctx.action().declareOutputs([img]);
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/elixir_image",
  mnemonic: "ExImage",
  impl,
  cfg: {
    name: label(),
    deps: [label()],
  },
  defaults: {
    deps: [],
  },
  toolchains: [ElixirToolchain],
});

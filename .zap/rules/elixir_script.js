import ElixirToolchain, {BEAM_EXT, EX_EXT} from "../toolchains/elixir.js";

const impl = ctx => {
  const { main } = ctx.cfg();
  ctx.action().declareOutputs([main]);
};

export default Zap.Rule({
  name: "elixir_script",
  mnemonic: "ExScript",
  impl,
  cfg: {
    name: label(),
    main: file(),
  },
	defaults: {
	},
  toolchains: [ElixirToolchain]
});


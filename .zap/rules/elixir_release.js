import ElixirToolchain, {EX_EXT} from "../toolchains/elixir.js";
import ErlangToolchain, {BEAM_EXT} from "../toolchains/erlang.js";

const impl = ctx => {
  const { name, deps, } = ctx.cfg();
  ctx.action().writeFile({
    dst: name,
    data: `${name}`
  })
  ctx.action().declareOutputs([name]);
};

export default Zap.Rule({
  name: "elixir_release",
  mnemonic: "ExRel",
  impl,
  cfg: {
    name: label(),
    deps: [label()],
  },
	defaults: {
		deps: [],
	},
  toolchains: [ElixirToolchain, ErlangToolchain]
});


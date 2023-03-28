import ElixirToolchain, {
  EX_EXT,
} from "https://rules.warp.build/toolchains/elixir.js";
import ErlangToolchain, {
  BEAM_EXT,
} from "https://rules.warp.build/toolchains/erlang.js";

const impl = (ctx) => {
  const { name, deps } = ctx.cfg();
  ctx.action().writeFile({
    dst: name,
    data: `${name}`,
  });
  ctx.action().declareOutputs([name]);
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/elixir_release",
  mnemonic: "ExRel",
  impl,
  cfg: {
    name: target(),
    deps: [target()],
  },
  defaults: {
    deps: [],
  },
  toolchains: [ElixirToolchain, ErlangToolchain],
});

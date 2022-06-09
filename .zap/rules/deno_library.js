import DenoToolchain, {JS_EXT} from "../toolchains/deno.js";

const impl = ctx => {
  const { label, name, srcs } = ctx.cfg();
  ctx.action().declareOutputs(srcs);
};

export default Zap.Rule({
  name: "deno_library",
  mnemonic: "DenoLib",
  impl,
  cfg: {
    name: label(),
    srcs: [file()],
    deps: [label()],
  },
	defaults: {
    srcs: [ "*.js" ],
    deps: [],
	},
  toolchains: [DenoToolchain]
});

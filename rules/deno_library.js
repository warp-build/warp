import DenoToolchain, {JS_EXT} from "https://pkgs.warp.build/toolchains/deno.js";

const impl = ctx => {
  const { label, name, srcs } = ctx.cfg();
  ctx.action().declareOutputs(srcs);
};

export default Warp.Rule({
  name: "https://pkgs.warp.build/rules/deno_library",
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

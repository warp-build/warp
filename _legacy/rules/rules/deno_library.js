import DenoToolchain, {
  JS_EXT,
} from "https://rules.warp.build/toolchains/deno.js";

const impl = (ctx) => {
  const { target, name, srcs } = ctx.cfg();
  ctx.action().declareOutputs(srcs);
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/deno_library",
  mnemonic: "DenoLib",
  impl,
  cfg: {
    name: target(),
    srcs: [file()],
    deps: [target()],
  },
  defaults: {
    srcs: ["*.js"],
    deps: [],
  },
  toolchains: [DenoToolchain],
});

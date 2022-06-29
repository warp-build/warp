import ElixirToolchain, {BEAM_EXT} from "../toolchains/elixir.js";

const impl = ctx => {
  const { name, deps, srcs, } = ctx.cfg();

  const tarball = `${name}.tar.gz`
  ctx.action().declareOutputs([tarball]);

  const args = ["compile"]
  ctx.action().exec({ cmd: ElixirToolchain.provides().MIX, args });
  ctx.action().tar({ srcs: [`_build/dev/lib/${name}`], dst: tarball});
};

export default Warp.Rule({
  name: "mix_library",
  mnemonic: "MixLib",
  impl,
  cfg: {
    name: label(),
    srcs: [file()],
  },
	defaults: {
    srcs: [ "*.ex", "lib/**/*.ex" ],
	},
  toolchains: [ElixirToolchain]
});


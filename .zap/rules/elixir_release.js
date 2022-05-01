import ElixirToolchain, {BEAM_EXT} from "../toolchains/elixir.js";

const impl = ctx => {
  const { name, deps, srcs, } = ctx.cfg();

  const outputs = srcs
    .map(label => {
      let path = label.split("/").slice(0, -1).join("/")
      let modName = label.split("/").map(part => part[0].toUpperCase() + part.slice(1)).join(".")
      let out = File.join(path, File.withExtension(`Elixir.${modName}`, BEAM_EXT))
      return out
    });

  ctx.action().declareOutputs(outputs);

  const paths = {};
  srcs.forEach( src => {
    const parent = File.parent(src);
    paths[parent] = paths[parent] || [];
    paths[parent].push(src);
  });

  Object.entries(paths).forEach( ([out, srcs]) => {
    const args = [ "-o", out, ...srcs ];

    ctx.action().exec({ cmd: ElixirToolchain.provides().ELIXIRC, args });
  });
};

export default Zap.Rule({
  name: "elixir_release",
  mnemonic: "ExRel",
  impl,
  cfg: {
    name: label(),
    deps: [label()],
    srcs: [file()],
  },
	defaults: {
		srcs: [ "*.ex" ],
		deps: [],
	},
  toolchains: [ElixirToolchain]
});


import ElixirToolchain, {BEAM_EXT} from "https://zap.build/toolchains/elixir";

const impl = ctx => {
  const { name, deps, srcs, } = ctx.cfg();

  const transitiveDeps = ctx.transitiveDeps().flatMap(dep => dep.outs);

  const finalSrcs = transitiveDeps
    .filter(path => path.endsWith(ERL_EXT))
    .concat(srcs);

  const outputs = srcs
    .map(erl => File.withExtension(erl, BEAM_EXT));

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
  name: "elixir_library",
  mnemonic: "ExLibrary",
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


import ErlangToolchain, {HEADER_EXT, BEAM_EXT, ERL_EXT} from "https://pkgs.warp.build/toolchains/erlang.js";

const impl = ctx => {
  const { name, deps, test } = ctx.cfg();

  const transitiveDeps = ctx.transitiveDeps().flatMap(dep => dep.outs);

  const includePaths = transitiveDeps
    .filter(path => path.endsWith(HEADER_EXT))
    .concat(headers)
    .flatMap(path => ["-I", File.parent(path)])
    .unique();

  const extraLibPaths = transitiveDeps
    .filter(path => path.endsWith(BEAM_EXT))
    .map(File.parent)
    .unique()
    .flatMap(dir => ["-pa", dir])

  const finalSrcs = transitiveDeps
    .filter(path => path.endsWith(ERL_EXT))
    .concat(behaviors)
    .concat(srcs);

  const outputs = behaviors
    .concat(srcs)
    .map(erl => File.withExtension(erl, BEAM_EXT))
    .concat(headers);

  ctx.action().declareOutputs(outputs);

  const paths = {};
  [test].forEach( src => {
    const parent = File.parent(src);
    paths[parent] = paths[parent] || [];
    paths[parent].push(src);
  });

  Object.entries(paths).forEach( ([out, srcs]) => {
    const args = [
      "-b", "beam", "-Wall",
      ...includePaths,
      ...extraLibPaths,
      "-o", out, "--",
      ...srcs
    ];

    ctx.action().exec({ cmd: ErlangToolchain.provides().ERLC, args });
  });
};

export default Warp.Rule({
  name: "https://pkgs.warp.build/rules/erlang_test",
  mnemonic: "ErlTest",
  impl,
  cfg: {
    name: label(),
    test: file(),
    deps: [label()],
  },
  defaults: {
    deps: [],
  },
  toolchains: [ErlangToolchain]
});

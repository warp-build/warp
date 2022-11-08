import ErlangToolchain, {HEADER_EXT, BEAM_EXT, ERL_EXT} from "https://rules.warp.build/toolchains/erlang.js";

const impl = ctx => {
  const { name, deps, srcs, headers, behaviors } = ctx.cfg();

  const srcsErl = srcs.filter(p => p.endsWith(ERL_EXT));
  const srcsHrl = headers.concat(srcs).filter(p => p.endsWith(HEADER_EXT));

  const transitiveDeps = ctx.transitiveDeps().flatMap(dep => dep.outs);

  const includePaths = transitiveDeps
    .filter(path => path.endsWith(HEADER_EXT))
    .concat(srcsHrl)
    .unique()
    .flatMap(path => ["-I", File.parent(path)]);

  const extraLibPaths = transitiveDeps
    .filter(path => path.endsWith(BEAM_EXT))
    .map(File.parent)
    .unique()
    .flatMap(dir => ["-pa", dir])

  const finalSrcs = transitiveDeps
    .filter(path => path.endsWith(ERL_EXT))
    .concat(behaviors)
    .concat(srcsErl);

  const outputs = behaviors
    .concat(srcsErl)
    .flatMap(erl => [erl, File.withExtension(erl, BEAM_EXT)])
    .concat(srcsHrl);

  ctx.action().declareOutputs(outputs);

  const paths = {};
  srcsErl.forEach( src => {
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

    ctx.action().runShell({ script: `erlc ${args.join(" ")}` });
  });
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/erlang_library",
  mnemonic: "ErlLibrary",
  impl,
  cfg: {
    name: label(),
    deps: [label()],
    srcs: [file()],
    headers: [file()],
    behaviors: [file()],
  },
  defaults: {
    srcs: [ "*.erl", "src/*.erl", "src/*.hrl" ],
    headers: [ "*.hrl", "include/*.hrl" ],
    behaviors: [],
    deps: [],
  },
  toolchains: [ErlangToolchain]
});

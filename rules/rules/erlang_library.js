import ErlangToolchain, {
  BEAM_EXT,
  ERL_EXT,
  HEADER_EXT,
} from "https://rules.warp.build/toolchains/erlang.js";

const impl = (ctx) => {
  const { name, deps, srcs, headers, behaviors } = ctx.cfg();

  const srcsErl = srcs.filter((p) => p.endsWith(ERL_EXT));
  const srcsHrl = headers.concat(srcs).filter((p) => p.endsWith(HEADER_EXT));

  const transitiveDeps = ctx.transitiveDeps().flatMap((dep) => dep.outs);

  const includePaths = transitiveDeps
    .concat(srcsHrl)
    .filter((path) => path.endsWith(HEADER_EXT))
    .flatMap((path) => {
      // NOTE(@ostera): EXCEPT when we have a _build dependency, which is a 3rdparty dependency
      // then we only allow `-include_lib("emqx/include/types.hrl")`
      // if (path.startsWith("_build")) {
      //   return [File.parent(File.parent(File.parent(path)))];
      // }

      // NOTE(@ostera): for every header file we find, we want a series of
      // paths to be made available to the compiler. This complexity is
      // unfortunately required because `-include` and `-include_lib` are super
      // flexible.
      //
      // So if the file `apps/emqx/include/types.hrl` is a dependency to this current erlang library,
      // it can be included in many ways, depending on where the includer is located:
      //
      //   -include("types.hrl")
      //   -include("include/types.hrl")
      //   -include("emqx/include/types.hrl")
      //   -include("apps/emqx/include/types.hrl")
      //   -include_lib("emqx/include/types.hrl")
      //
      // So we do some path juggling here to make sure `erlc` finds the file.
      //
      return [
        File.parent(path),
        File.parent(File.parent(path)),
      ];
    })
    .unique()
    .sort()
    .flatMap((path) => ["-I", path]);

  const extraLibPaths = transitiveDeps
    .filter((path) => path.endsWith(BEAM_EXT))
    .map(File.parent)
    .unique()
    .flatMap((dir) => ["-pa", dir]);

  const finalSrcs = transitiveDeps
    .filter((path) => path.endsWith(ERL_EXT))
    .concat(behaviors)
    .concat(srcsErl);

  const outputs = behaviors
    .concat(srcsErl)
    .flatMap((erl) => [erl, File.withExtension(erl, BEAM_EXT)])
    .concat(srcsHrl);

  ctx.action().declareOutputs(outputs);

  const paths = {};
  srcsErl.forEach((src) => {
    const parent = File.parent(src);
    paths[parent] = paths[parent] || [];
    paths[parent].push(src);
  });

  Object.entries(paths).forEach(([out, srcs]) => {
    const args = [
      "-b",
      "beam",
      "-Wall",
      ...includePaths,
      ...extraLibPaths,
      "-o",
      out,
      "--",
      ...srcs,
    ];

    ctx.action().runShell({ script: `erlc ${args.join(" ")}` });
  });
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/erlang_library",
  mnemonic: "ErlLibrary",
  impl,
  cfg: {
    name: target(),
    deps: [target()],
    srcs: [file()],
    headers: [file()],
    behaviors: [file()],
  },
  defaults: {
    srcs: [],
    headers: [],
    behaviors: [],
    deps: [],
  },
  toolchains: [ErlangToolchain],
});

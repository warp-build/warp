import ErlangToolchain, {
  BEAM_EXT,
  ERL_EXT,
  HEADER_EXT,
} from "https://rules.warp.build/toolchains/erlang.js";

const impl = (ctx) => {
  const { name, deps, test, cases, cwd } = ctx.cfg();

  const transitiveDeps = ctx.transitiveDeps().flatMap((dep) => dep.outs);
  const runtimeDeps = ctx.runtimeDeps().flatMap((dep) => dep.outs);

  const includePaths = transitiveDeps
    .filter((path) => path.endsWith(HEADER_EXT))
    .flatMap((path) => {
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
      const parts = File.parent(path).split("/");

      // NOTE(@ostera): EXCEPT when we have a _build dependency, which is a 3rdparty dependency
      // then we only allow `-include_lib("emqx/include/types.hrl")`
      if (path.startsWith("_build")) {
        return [File.parent(File.parent(File.parent(path)))];
      }

      return [
        // this is the full path to the folder, so if the file is `apps/emqx/include/types.hrl`
        // it becomes `apps/emqx/include`
        File.parent(path),

        // this is an array of paths starting at the root of the full path, and walking downwards.
        // so for `apps/emqx/include/types.hrl` this is:
        //   [
        //     "apps",
        //     "apps/emqx",
        //     "apps/emqx/include"
        //   ]
        //
        ...new Array(parts.length - 1).fill(true).flatMap((_, idx) => {
          const path = parts.slice(0, idx + 1).join("/");
          return [path, `${path}/include`];
        }),
      ];
    })
    .unique()
    .sort()
    .flatMap((path) => ["-include", path]);

  const extraLibPaths = transitiveDeps
    .concat(runtimeDeps)
    .filter((path) => path.endsWith(BEAM_EXT))
    .map(File.parent)
    .unique()
    .flatMap((dir) => ["-pa", ctx.path(dir)]);

  const sname = name.replace("/", "-");

  ctx.action().runShell({
    script: `

ct_run \
  -dir ${cwd()} \
  -verbosity 100 \
  -no-auto-compile \
  -suite ${test.replace(ERL_EXT, "")} \
  ${cases.length > 0 ? `-case ${cases.join(" ")}` : ""} \
  ${includePaths.join(" ")} \
  -erl_args \
    -sname test-$(uuidgen) \
    ${extraLibPaths.join(" ")}

  `,
  });

  ctx.action().declareOutputs([]);
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/erlang_test",
  mnemonic: "ErlTest",
  impl,
  cfg: {
    name: target(),
    test: file(),
    cases: [string()],
    deps: [target()],
  },
  defaults: {
    deps: [],
    cases: [],
  },
  toolchains: [ErlangToolchain],
});

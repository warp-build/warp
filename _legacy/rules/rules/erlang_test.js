import ErlangToolchain, {
  BEAM_EXT,
  ERL_EXT,
  HEADER_EXT,
} from "https://rules.warp.build/toolchains/erlang.js";

const impl = (ctx) => {
  const { name, deps, test, cases, groups, cwd } = ctx.cfg();

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
      // NOTE(@ostera): EXCEPT when we have a _build dependency, which is a 3rdparty dependency
      // then we only allow `-include_lib("emqx/include/types.hrl")`
      if (path.startsWith("_build")) {
        return [File.parent(File.parent(File.parent(path)))];
      }

      return [
        // this is the full path to the folder, so if the file is `apps/emqx/include/types.hrl`
        // it becomes `apps/emqx/include`
        File.parent(path),
        File.parent(File.parent(path)),
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
    .flatMap((dir) => ["-pa", dir]);

  const sname = name.replace("/", "-");

  const testRunner = `${name}.runtest`;
  ctx.action().writeFile({
    dst: testRunner,
    data: `set -e

cd $(dirname "\${BASH_SOURCE[0]}")

ct_run \\
  -dir . \\
  -verbosity 100 \\
  -no-auto-compile \\
  -suite ${test.replace(ERL_EXT, "")} \\
  ${groups.length > 0 ? `-group ${groups.join(" \n")}` : ""} \\
  ${cases.length > 0 ? `-case ${cases.join(" \n")}` : ""} \\
  ${includePaths.join(" \n")} \\
  -erl_args \\
    -sname test-$(uuidgen) \\
    ${extraLibPaths.join(" ")}

  `,
  });
  ctx.action().declareOutputs([]);
  ctx.action().setPermissions({ file: testRunner, executable: true });
  ctx.action().declareTestRunner(testRunner);
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/erlang_test",
  mnemonic: "ErlTest",
  impl,
  cfg: {
    name: target(),
    test: file(),
    groups: [string()],
    cases: [string()],
    deps: [target()],
  },
  defaults: {
    deps: [],
    cases: [],
    groups: [],
  },
  toolchains: [ErlangToolchain],
});

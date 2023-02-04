import { TAR_EXT } from "https://rules.warp.build/rules/archive.js";
import ElixirToolchain, {
  EX_EXT,
} from "https://rules.warp.build/toolchains/elixir.js";
import ErlangToolchain, {
  BEAM_EXT,
} from "https://rules.warp.build/toolchains/erlang.js";

const impl = (ctx) => {
  const { label, name, srcs, deps, main } = ctx.cfg();

  const transitiveDeps = ctx.transitiveDeps();

  const elixirLibraries = transitiveDeps
    .filter(
      (dep) => dep.ruleName == "https://rules.warp.build/rules/elixir_library"
    )
    .flatMap((dep) =>
      dep.outs
        .filter((out) => out.endsWith(BEAM_EXT))
        .map((path) => File.parent(path))
        .unique()
    );

  const mixLibraries = transitiveDeps
    .filter(
      (dep) =>
        dep.ruleName == "https://rules.warp.build/rules/mix_library" ||
        dep.ruleName == "https://rules.warp.build/rules/rebar3_library" ||
        dep.ruleName == "https://rules.warp.build/rules/erlangmklibrary"
    )
    .map(
      (dep) =>
        `${Label.path(dep.label)}/_build/default/lib/${Label.name(
          dep.label
        )}/ebin`
    )
    .unique();

  const extraPaths = [...mixLibraries, ...elixirLibraries]
    .map((path) => ` -pa warp-outputs/${path} \\\n`)
    .sort()
    .unique()
    .join("");

  const run = `${Label.path(label)}/${name}.run_script`;

  ctx.action().declareOutputs([run, main]);
  ctx.action().declareRunScript(run);
  ctx.action().writeFile({
    dst: run,
    data: `#!/bin/bash -e

elixir \
  ${extraPaths} \
  $(dirname "\${BASH_SOURCE[0]}")/${File.filename(main)} $*

`,
  });

  ctx.action().setPermissions({ file: run, executable: true });
  ctx.provides({ [name]: run });
};

export default Warp.Rule({
  runnable: true,
  name: "https://rules.warp.build/rules/elixir_script",
  mnemonic: "ExScript",
  impl,
  cfg: {
    name: label(),
    main: file(),
    deps: [label()],
  },
  defaults: {
    deps: [],
  },
  toolchains: [ElixirToolchain, ErlangToolchain],
});

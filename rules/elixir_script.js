import { TAR_EXT } from "https://rules.warp.build/rules/archive.js";
import ElixirToolchain, { EX_EXT } from "https://rules.warp.build/toolchains/elixir.js";
import ErlangToolchain, { BEAM_EXT } from "https://rules.warp.build/toolchains/erlang.js";

const impl = (ctx) => {
  const { label, name, srcs, deps, main } = ctx.cfg();

  const transitiveDeps = ctx.transitiveDeps();

  const elixirLibraries = transitiveDeps.filter(dep => dep.ruleName == "https://rules.warp.build/rules/elixir_library");
  const mixLibraries = transitiveDeps.filter(dep => dep.ruleName == "https://rules.warp.build/rules/mix_library");

  const extraPaths = [
    ...mixLibraries
      .map((dep) => `${Label.path(dep.label)}/_build/prod/lib/${Label.name(dep.label)}/ebin`)
      .unique(),
    ...elixirLibraries
      .flatMap(dep =>
        dep.outs
          .filter((out) => out.endsWith(BEAM_EXT))
          .map((path) => File.parent(path))
          .unique()
      ),
  ]
    .flatMap((path) => ["-pa", `warp-outputs/${path}`])
    .join(" ");

  mixLibraries.forEach((dep) => {
    dep.outs.forEach((out) => {
      if (out.endsWith(TAR_EXT)) {
        ctx.action().exec({
          cmd: "tar",
          args: ["xf", File.filename(out)],
          cwd: Label.path(dep.label),
        });
      }
    });
  });

  const run = `${Label.path(label)}/${name}.run_script`;

  ctx.action().declareOutputs([run, main]);
  ctx.action().declareRunScript(run);
  ctx.action().writeFile({
    dst: run,
    data: `#!/bin/bash -e

${mixLibraries
  .flatMap((dep) => {
    return dep.outs.flatMap((out) => {
      if (out.endsWith(TAR_EXT)) {
        return [
          `cd warp-outputs/${Label.path(dep.label)}; tar xf ${File.filename(
            out
          )}; cd - > /dev/null;`,
        ];
      } else {
        return [];
      }
    })
  })
  .join("\n")}

elixir \
  ${extraPaths} \
  $(dirname "\${BASH_SOURCE[0]}")/${File.filename(main)} $*

`,
  });
  ctx.action().runShell({
    needsTty: true,
    script: `#!/bin/bash -xe

chmod +x ${run}

`,
  });
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
    deps: []
  },
  toolchains: [ElixirToolchain, ErlangToolchain],
});

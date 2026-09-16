import { TAR_EXT } from "https://rules.warp.build/rules/archive.js";
import ElixirToolchain from "https://rules.warp.build/toolchains/elixir.js";
import ErlangToolchain, {
  BEAM_EXT,
} from "https://rules.warp.build/toolchains/erlang.js";

const impl = (ctx) => {
  const { target, name, srcs, deps, args, dot_iex, elixirc_opts } = ctx.cfg();

  const transitiveDeps = ctx.transitiveDeps();

  const extraPaths = transitiveDeps
    .flatMap((dep) => [
      `${Target.path(dep.target)}/_build/prod/lib/${
        Target.name(dep.target)
      }/ebin`,
      ...dep.outs
        .filter((out) => out.endsWith(BEAM_EXT))
        .map((path) => File.parent(path)),
    ])
    .unique()
    .sort()
    // NOTE(@ostera): how do we get warp-outputs, but nice?
    .flatMap((path) => ["-pa", `warp-outputs/${path}`])
    .join(" ");

  const run = `${Target.path(target)}/${name}.run_shell`;

  ctx.action().declareOutputs([run, dot_iex]);
  ctx.action().declareRunScript(run);
  ctx.action().writeFile({
    dst: run,
    data: `#!/bin/bash -e

${
      transitiveDeps
        .flatMap((dep) =>
          dep.outs.flatMap((out) => {
            if (out.endsWith(TAR_EXT)) {
              return [
                `cd warp-outputs/${Target.path(dep.target)}; tar xf ${
                  File.filename(
                    out,
                  )
                }; cd - > /dev/null;`,
              ];
            } else {
              return [];
            }
          })
        )
        .join("\n")
    }

iex \
  ${args.join(" ")} \
  --dot-iex $(dirname "\${BASH_SOURCE[0]}")/${File.filename(dot_iex)} \
  ${extraPaths} -- $*
  
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
  name: "https://rules.warp.build/rules/iex_shell",
  mnemonic: "IExShell",
  impl,
  cfg: {
    name: target(),
    args: [string()],
    dot_iex: file(),
    deps: [target()],
  },
  defaults: {
    args: [],
    dot_iex: "",
    deps: [],
  },
  toolchains: [ElixirToolchain, ErlangToolchain],
});

import { TAR_EXT } from "../rules/archive.js";
import ElixirToolchain from "../toolchains/elixir.js";
import ErlangToolchain, { BEAM_EXT } from "../toolchains/erlang.js";

const impl = (ctx) => {
  const { label, name, deps, srcs, skip_deps, deps_args, compile_args } = ctx.cfg();
  const cwd = Label.path(label);

  const appTarball = `${name}.app.tar`;
  const outputs = [`${cwd}/${appTarball}`];
  ctx.action().declareOutputs(outputs);

  const { MIX } = ElixirToolchain.provides();

  const depsGet =
    skip_deps === "true" ? "" : `${MIX} deps.get --only \$MIX_ENV ${deps_args.join(" ")}`

  ctx.action().runShell({
    script: `#!/bin/bash -xe

export MIX_ENV=prod

cd ${cwd}

${
  ctx.transitiveDeps().flatMap(dep => dep.outs.flatMap(out => {
      if (out.endsWith(TAR_EXT)) {
        return [`tar xf ${Label.path(dep.label)}/${File.filename(out)}`];
      } else {
        return []
      }
  })).join("\n")
}

${depsGet}
${MIX} compile --no-deps-check ${compile_args.join(" ")} \
&& tar cf ${appTarball} _build/prod/lib/${name}

`,
  });
};

export default Zap.Rule({
  name: "mix_library",
  mnemonic: "MixLib",
  impl,
  cfg: {
    name: label(),
    srcs: [file()],
    extra_srcs: [file()],
    deps: [label()],
    deps_args: [string()],
    skip_deps: string(),
    compile_args: [string()],
  },
  defaults: {
    srcs: [
      "lib/**/*.eex",
      "lib/**/*.ex",
      "mix.exs",
      "src/**/*.app.src",
      "src/**/*.erl",
      "src/**/*.hrl",
      "include/**/*.hrl",
      "priv/**/*",
    ],
    extra_srcs: [],
    deps: [],
    deps_args: [],
    skip_deps: "false",
    compile_args: [],
  },
  toolchains: [ElixirToolchain, ErlangToolchain],
});

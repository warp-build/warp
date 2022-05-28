import ElixirToolchain from "../toolchains/elixir.js";
import ErlangToolchain, { BEAM_EXT } from "../toolchains/erlang.js";

const impl = (ctx) => {
  const { label, name, deps, srcs, deps_args, compile_args } = ctx.cfg();
  const cwd = Label.path(label);

  const appTarball = `${name}.app.tar`;
  const outputs = [`${cwd}/${appTarball}`];
  ctx.action().declareOutputs(outputs);

  const { MIX } = ElixirToolchain.provides();
  ctx.action().runShell({
    script: `#!/bin/bash -xe

export MIX_ENV=prod

cd ${cwd}
rm -rf _build deps
${MIX} deps.get --only \$MIX_ENV ${deps_args.join(" ")} \
&& ${MIX} compile --no-deps-check ${compile_args.join(" ")} \
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
    compile_args: [],
  },
  toolchains: [ElixirToolchain, ErlangToolchain],
});

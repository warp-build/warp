import ElixirToolchain, {BEAM_EXT} from "../toolchains/elixir.js";

const impl = ctx => {
  const { label, name, deps, srcs, } = ctx.cfg();
  const cwd = Label.path(label)

  const srcTarball = `${name}.src.tar`
  const appTarball = `${name}.app.tar`
  const outputs = [
    `${cwd}/${srcTarball}`,
    `${cwd}/${appTarball}`,
    ]
  ctx.action().declareOutputs(outputs);

  const { MIX } = ElixirToolchain.provides()
  ctx.action().runShell({
    script: `#!/bin/bash -xe

cd ${cwd}
tar cf ${srcTarball} . \
&& ${MIX} deps.get \
&& ${MIX} compile \
&& tar cf ${appTarball} _build/dev/lib/${name}

`,
  })
};

export default Zap.Rule({
  name: "mix_library",
  mnemonic: "MixLib",
  impl,
  cfg: {
    name: label(),
    srcs: [file()],
  },
	defaults: {
    srcs: [
      "*.eex",
      "*.ex",
      "*.erl",
      "*.hrl",
      "lib/**/*.eex",
      "lib/**/*.ex",
      "mix.exs",
      "src/**/*.app.src",
      "src/**/*.erl",
      "src/**/*.hrl",
    ],
	},
  toolchains: [ElixirToolchain]
});


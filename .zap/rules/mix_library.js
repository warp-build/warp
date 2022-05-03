import ElixirToolchain from "../toolchains/elixir.js";
import ErlangToolchain, {BEAM_EXT} from "../toolchains/erlang.js";

const impl = ctx => {
  const { label, name, deps, srcs, } = ctx.cfg();
  const cwd = Label.path(label)

  const appTarball = `${name}.app.tar`
  const outputs = [ `${cwd}/${appTarball}` ]
  ctx.action().declareOutputs(outputs);

  const { MIX } = ElixirToolchain.provides()
  ctx.action().runShell({
    script: `#!/bin/bash -xe

cd ${cwd}
${MIX} deps.get \
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
    deps: [label()],
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
    deps: [],
	},
  toolchains: [ElixirToolchain, ErlangToolchain]
});


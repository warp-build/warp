import ErlangToolchain, {BEAM_EXT} from "../toolchains/erlang.js";

const impl = ctx => {
  const { label, name, deps, srcs, } = ctx.cfg();
  const cwd = Label.path(label)

  const appTarball = `${name}.app.tar`
  const outputs = [ `${cwd}/${appTarball}` ]
  ctx.action().declareOutputs(outputs);

  const { REBAR } = ErlangToolchain.provides()
  ctx.action().runShell({
    script: `#!/bin/bash -xe

cd ${cwd}
rm -rf _build
${REBAR} compile \
&& tar cf ${appTarball} _build/default/lib/${name}

`,
  })
};

export default Zap.Rule({
  name: "rebar3_library",
  mnemonic: "Rebar3Lib",
  impl,
  cfg: {
    name: label(),
    srcs: [file()],
  },
	defaults: {
    srcs: [
      "c_src/**/*",
      "src/**/*.app.src",
      "src/**/*.erl",
      "src/**/*.hrl",
      "include/**/*.hrl",
      "rebar.config",
      "priv/**/*",
    ],
	},
  toolchains: [ErlangToolchain]
});


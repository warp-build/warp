import ErlangToolchain, { BEAM_EXT } from "https://pkgs.warp.build/toolchains/erlang.js";
import Rebar3Toolchain from "https://pkgs.warp.build/toolchains/rebar3.js";

const impl = (ctx) => {
  const { label, name, deps, srcs } = ctx.cfg();
  const cwd = Label.path(label);

  const appTarball = `${name}.app.tar`;
  const outputs = [`${cwd}/${appTarball}`];
  ctx.action().declareOutputs(outputs);

  const { REBAR3 } = Rebar3Toolchain.provides();

  const ERL_ENV = ErlangToolchain.provides();

  ctx.action().runShell({
    script: `#!/bin/bash -x

export PATH="${ERL_ENV.ERL_ROOT}:$PATH"
export C_INCLUDE_PATH="${ERL_ENV.INCLUDE_PATH}:$C_INCLUDE_PATH"
export CPLUS_INCLUDE_PATH="${ERL_ENV.INCLUDE_PATH}:$CPLUS_INCLUDE_PATH"
export C_LIB_PATH="${ERL_ENV.LIB_PATH}:$C_LIB_PATH"
export CPLUS_LIB_PATH="${ERL_ENV.LIB_PATH}:$CPLUS_LIB_PATH"

cd ${cwd}
rm -rf _build
${REBAR3} compile || status=$?

if [ \${status:-0} -gt 1 ]; then
  echo "Rebar exited with status $status"
  exit $status
fi

tar cf ${appTarball} _build/default/lib/${name}

`,
  });
};

export default Warp.Rule({
  name: "rebar3_library",
  mnemonic: "Rebar3Lib",
  impl,
  cfg: {
    name: label(),
    deps: [label()],
    srcs: [file()],
    extra_srcs: [file()],
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
    extra_srcs: [],
    deps: [],
  },
  toolchains: [ErlangToolchain, Rebar3Toolchain],
});

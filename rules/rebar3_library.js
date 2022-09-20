import CMakeToolchain from "https://pkgs.warp.build/toolchains/cmake.js";
import ErlangToolchain, { BEAM_EXT } from "https://pkgs.warp.build/toolchains/erlang.js";
import Rebar3Toolchain from "https://pkgs.warp.build/toolchains/rebar3.js";

const impl = (ctx) => {
  const { label, name, deps, srcs } = ctx.cfg();
  const cwd = Label.path(label);

  ctx.action().declareOutputs([
    `${cwd}/_build/default/lib/${name}`
  ]);

  ctx.action().runShell({
    script: `

export C_INCLUDE_PATH="$ERL_INCLUDE_PATH:$C_INCLUDE_PATH"
export CPLUS_INCLUDE_PATH="$ERL_INCLUDE_PATH:$CPLUS_INCLUDE_PATH"
export C_LIB_PATH="$ERL_LIB_PATH:$C_LIB_PATH"
export CPLUS_LIB_PATH="$ERL_LIB_PATH:$CPLUS_LIB_PATH"

cd ${cwd}
rm -rf _build
rebar3 compile || status=$?

if [ \${status:-0} -gt 1 ]; then
  echo "Rebar exited with status $status"
  exit $status
fi

`,
  });
};

export default Warp.Rule({
  name: "https://pkgs.warp.build/rules/rebar3_library",
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
  toolchains: [ErlangToolchain, Rebar3Toolchain, CMakeToolchain],
});

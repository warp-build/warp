import ErlangToolchain, { BEAM_EXT } from "https://pkgs.warp.build/toolchains/erlang.js";
import Rebar3Toolchain from "https://pkgs.warp.build/toolchains/rebar3.js";
import CMakeToolchain from "https://pkgs.warp.build/toolchains/cmake.js";

const impl = (ctx) => {
  const { cwd, label, name, deps, srcs } = ctx.cfg();

  ctx.action().declareOutputs([
    `${cwd()}/_build/default/lib/${name}`
  ]);

  ctx.action().runShell({
    script: `

export C_INCLUDE_PATH="$ERL_INCLUDE_PATH:$C_INCLUDE_PATH"
export CPLUS_INCLUDE_PATH="$ERL_INCLUDE_PATH:$CPLUS_INCLUDE_PATH"
export C_LIB_PATH="$ERL_LIB_PATH:$C_LIB_PATH"
export CPLUS_LIB_PATH="$ERL_LIB_PATH:$CPLUS_LIB_PATH"

cd ${cwd()}
rm -rf _build

maybe_copy() {
  if [[ -d $1 ]]; then
    cp -R $1 _build/default/lib/${name}/;
  else
    mkdir -p _build/default/lib/${name}/$1;
  fi
}
maybe_copy priv;
maybe_copy include;
maybe_copy src;

rebar3 compile || status=$?

# NOTE(@ostera): need to investigate more why but some libraries *need* 2 passes of rebar3 compile
# to succeed.
if [[ ! -d "./_build/default/lib/${name}" ]]; then
  rebar3 compile || status=$?
fi

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

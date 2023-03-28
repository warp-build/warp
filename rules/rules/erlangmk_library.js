import ErlangToolchain, {
  BEAM_EXT,
  ERL_EXT,
} from "https://rules.warp.build/toolchains/erlang.js";

const impl = (ctx) => {
  const { target, name, deps, srcs, headers } = ctx.cfg();

  ctx.action().declareOutputs([`_build/default/lib/${name}`]);

  ctx.action().runShell({
    script: `#!/bin/bash

export PATH="\${ERL_ROOT}:$PATH"
export C_INCLUDE_PATH="\${ERL_INCLUDE_PATH}:$C_INCLUDE_PATH"
export CPLUS_INCLUDE_PATH="\${ERL_INCLUDE_PATH}:$CPLUS_INCLUDE_PATH"
export C_LIB_PATH="\${ERL_LIB_PATH}:$C_LIB_PATH"
export CPLUS_LIB_PATH="\${ERL_LIB_PATH}:$CPLUS_LIB_PATH"

cd ${Target.path(target)}
make clean app
mkdir -p _build/default/lib/${name}
mv ebin _build/default/lib/${name}

`,
  });
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/erlangmk_library",
  mnemonic: "ErlMkLib",
  impl,
  cfg: {
    name: target(),
    srcs: [file()],
    deps: [target()],
    headers: [file()],
  },
  defaults: {
    srcs: ["*.erl", "src/**/*.erl", "erlang.mk", "Makefile"],
    deps: [],
    headers: ["*.hrl", "src/**/*.hrl", "include/**/*.hrl"],
  },
  sandbox: {
    mode: "copy",
  },
  toolchains: [ErlangToolchain],
});

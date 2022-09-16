import ErlangToolchain, {BEAM_EXT, ERL_EXT} from "../toolchains/erlang.js";

const impl = ctx => {
  const { label, name, deps, srcs, headers } = ctx.cfg();

  const outputs = srcs
    .filter(file => file.endsWith(ERL_EXT))
    .map(erl => {
      let parent = File.parent(File.parent(erl))
      if (parent.endsWith("src")) {
        parent = parent.replace(/\/src$/, "")
      }
      const name = File.withExtension(File.filename(erl), BEAM_EXT)
      return File.join(parent, File.join("ebin", name))
    })
    .concat(headers);

  ctx.action().declareOutputs([
    `${Label.path(label)}/ebin/${name}.app`,
    `${Label.path(label)}/${name}.d`,
    ...outputs
  ]);

  const ERL_ENV = ErlangToolchain.provides();
  ctx.action().runShell({
    script: `#!/bin/bash

export PATH="${ERL_ENV.ERL_ROOT}:$PATH"
export C_INCLUDE_PATH="${ERL_ENV.INCLUDE_PATH}:$C_INCLUDE_PATH"
export CPLUS_INCLUDE_PATH="${ERL_ENV.INCLUDE_PATH}:$CPLUS_INCLUDE_PATH"
export C_LIB_PATH="${ERL_ENV.LIB_PATH}:$C_LIB_PATH"
export CPLUS_LIB_PATH="${ERL_ENV.LIB_PATH}:$CPLUS_LIB_PATH"

cd ${Label.path(label)}
make clean app

`,
  })
};

export default Warp.Rule({
  name: "erlangmk_library",
  mnemonic: "ErlMkLib",
  impl,
  cfg: {
    name: label(),
    srcs: [file()],
    deps: [label()],
    headers: [file()]
  },
	defaults: {
    srcs: [ "*.erl", "src/**/*.erl", "erlang.mk", "Makefile" ],
    deps: [],
    headers: [ "*.hrl", "src/**/*.hrl", "include/**/*.hrl" ],
	},
  sandbox: {
    mode: "copy",
  },
  toolchains: [ErlangToolchain]
});


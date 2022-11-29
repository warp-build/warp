import ErlangToolchain, {BEAM_EXT, ERL_EXT} from "https://rules.warp.build/toolchains/erlang.js";

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
    `ebin/${name}.app`,
    ...outputs
  ]);

  ctx.action().runShell({
    script: `#!/bin/bash

export PATH="\${ERL_ROOT}:$PATH"
export C_INCLUDE_PATH="\${ERL_INCLUDE_PATH}:$C_INCLUDE_PATH"
export CPLUS_INCLUDE_PATH="\${ERL_INCLUDE_PATH}:$CPLUS_INCLUDE_PATH"
export C_LIB_PATH="\${ERL_LIB_PATH}:$C_LIB_PATH"
export CPLUS_LIB_PATH="\${ERL_LIB_PATH}:$CPLUS_LIB_PATH"

cd ${Label.path(label)}
make clean app

`,
  })
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/erlangmk_library",
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


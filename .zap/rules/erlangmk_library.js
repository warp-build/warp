import ErlangToolchain, {BEAM_EXT, ERL_EXT} from "../toolchains/erlang.js";

const impl = ctx => {
  const { label, name, deps, srcs, headers } = ctx.cfg();

  const outputs = srcs
    .filter(file => file.endsWith(ERL_EXT))
    .map(erl => {
      const parent = File.parent(File.parent(erl))
      const name = File.withExtension(File.filename(erl), BEAM_EXT)
      return File.join(parent, File.join("ebin", name))
    })
    .concat(headers);
  ctx.action().declareOutputs([
    `${Label.path(label)}/ebin/${name}.app`,
    ...outputs
  ]);

  ctx.action().runShell({
    script: `#!/bin/bash -xe

cd ${Label.path(label)}
make

`,
  })
};

export default Zap.Rule({
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
  toolchains: [ErlangToolchain]
});


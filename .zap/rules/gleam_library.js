import ErlangToolchain, {BEAM_EXT} from "../toolchains/erlang.js"
import GleamToolchain, {GLEAM_EXT} from "../toolchains/gleam.js"

const impl = ctx => {
  const { label, name, deps, srcs} = ctx.cfg()

  const outputs = srcs
    .map(erl => File.withExtension(erl, BEAM_EXT))

  ctx.action().declareOutputs(outputs)

  ctx.action().runShell({
    script: `#!/bin/bash -xe

    # Create temporary .toml file
    echo 'name = "${name}"' > ${Label.path(label)}/gleam.toml

    ${GleamToolchain.provides().GLEAM} \
      compile-package \
      --lib . \
      --out ${Label.path(label)} \
      --package ${Label.path(label)} \
      --target erlang

    `
  })
}

export default Zap.Rule({
  name: "gleam_library",
  mnemonic: "GleamLibrary",
  impl,
  cfg: {
    name: label(),
    deps: [label()],
    srcs: [file()],
  },
	defaults: {
		srcs: [ "src/*.gleam", ],
		deps: [],
	},
  toolchains: [GleamToolchain, ErlangToolchain]
})

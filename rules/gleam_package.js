import ErlangToolchain, {BEAM_EXT} from "../toolchains/erlang.js"
import GleamToolchain, {GLEAM_EXT} from "../toolchains/gleam.js"

const impl = ctx => {
  const { label, name, deps } = ctx.cfg()

  ctx.action().declareOutputs([])

  ctx.action().runShell({
    script: `#!/bin/bash -xe
    ${GleamToolchain.provides().GLEAM} build
    `
  })
}

export default Warp.Rule({
  name: "gleam_package",
  mnemonic: "GleamPackage",
  impl,
  cfg: {
    name: label(),
    deps: [label()],
  },
	defaults: {
		deps: [],
	},
  toolchains: [GleamToolchain, ErlangToolchain]
})

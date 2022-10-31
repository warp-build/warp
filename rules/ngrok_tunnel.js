import NgrokToolchain from "https://rules.warp.build/toolchains/ngrok.js"

const impl = ctx => {
  const { label, args} = ctx.cfg()
  ctx.action().declareOutputs([])
  ctx.action().runShell({
    script: `#!/bin/bash -xe

ngrok ${args.join(" ")}

    `
  })
}

export default Warp.Rule({
  runnable: true,
  name: "https://rules.warp.build/rules/ngrok_tunnel",
  mnemonic: "NgrokTunel",
  impl,
  cfg: {
    name: label(),
    args: [string()],
    deps: [label()],
  },
  defaults: {
    deps: [],
  },
  toolchains: [NgrokToolchain]
})

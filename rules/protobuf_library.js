import ProtobufToolchain from "https://rules.warp.build/toolchains/protobuf.js";

const impl = (ctx) => {
  const { cwd, name, deps, protos, flags, out_dir } = ctx.cfg();

  ctx.action().runShell({
    script: `#!/bin/bash

cd ${cwd()}
protoc ${flags.join(" ")} ${protos.join(" ")}

`,
  });

  ctx.action().declareOutputs([`${cwd()}/${out_dir}`]);
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/protobuf_library",
  mnemonic: "ProtoLib",
  impl,
  cfg: {
    name: label(),
    protos: [file()],
    flags: [string()],
    deps: [label()],
  },
  defaults: {
    deps: [],
  },
  toolchains: [ProtobufToolchain],
});

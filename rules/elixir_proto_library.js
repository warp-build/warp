import ElixirProtobufToolchain from "https://rules.warp.build/toolchains/elixir_protobuf.js";
import ProtobufToolchain from "https://rules.warp.build/toolchains/protobuf.js";
import ElixirToolchain from "https://rules.warp.build/toolchains/elixir.js";
import ErlangToolchain from "https://rules.warp.build/toolchains/erlang.js";

const impl = ctx => {
  const { cwd, name, deps, protos, flags, out_dir } = ctx.cfg();

  ctx.action().runShell({
    script: `#!/bin/bash

cd ${cwd()}
mkdir -p ./generated/elixir
protoc \
  --elixir_out=inline_docs=true,one_file_per_module=true,gen_descriptors=true,plugins=grpc:./generated/elixir \
  ${protos.join(" ")}

`,
  });

  ctx.action().declareOutputs([`${cwd()}/generated/elixir`]);
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/elixir_proto_library",
  mnemonic: "ExProtoLib",
  impl,
  cfg: {
    name: label(),
    protos: [file()],
    deps: [label()],
  },
  defaults: {
    deps: [],
  },
  toolchains: [ElixirProtobufToolchain, ProtobufToolchain, ElixirToolchain, ErlangToolchain]
});

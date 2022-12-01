import ProtobufToolchain from "https://rules.warp.build/toolchains/protobuf.js";
import ErlangToolchain from "https://rules.warp.build/toolchains/erlang.js";
import ErlangProtobufToolchain from "https://rules.warp.build/toolchains/erlang_protobuf.js";

const impl = ctx => {
  const { cwd, name, deps, protos, flags, out_dir } = ctx.cfg();

  let gen_dir = `${cwd()}/generated/erlang`;
  ctx.action().runShell({
    script: `

mkdir -p ${gen_dir}
protoc \
  --erlang_out=${gen_dir} \
  ${protos.join(" ")}

`,
  });

  ctx.action().declareOutputs([gen_dir]);
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/erlang_proto_library",
  mnemonic: "ErlProtoLib",
  impl,
  cfg: {
    name: label(),
    protos: [file()],
    deps: [label()],
  },
  defaults: {
    deps: [],
  },
  toolchains: [
    ErlangProtobufToolchain,
    ErlangToolchain,
    ProtobufToolchain,
  ]
});

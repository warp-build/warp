import ProtobufToolchain from "https://rules.warp.build/toolchains/protobuf.js";
import ElixirToolchain from "https://rules.warp.build/toolchains/elixir.js";
import ErlangToolchain from "https://rules.warp.build/toolchains/erlang.js";

const impl = ctx => {
  const { version } = ctx.cfg();

  ctx.action().runShell({
    script: `

mix escript.install github warp-build/protoc-erlang ${version} --force
mv /warp/home/.mix/escripts/protoc-gen-erlang . 

`
  })

  const protoc_gen_erlang = "protoc-gen-erlang";
  ctx.action().setPermissions({ file: protoc_gen_erlang, executable: true })

  ctx.action().declareOutputs([protoc_gen_erlang]);

  ctx.provides({ "protoc-gen-erlang": protoc_gen_erlang });
};

export default Warp.Toolchain({
  name: "https://rules.warp.build/toolchains/erlang_protobuf",
  mnemonic: "Erlang/Proto",
  impl,
  cfg: {
    version: string(),
  },
  defaults: {
    version: ""
  },
  toolchains: [ErlangToolchain, ElixirToolchain, ProtobufToolchain]
});

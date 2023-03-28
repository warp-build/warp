import ProtobufToolchain from "https://rules.warp.build/toolchains/protobuf.js";
import ElixirToolchain from "https://rules.warp.build/toolchains/elixir.js";
import ErlangToolchain from "https://rules.warp.build/toolchains/erlang.js";

const impl = (ctx) => {
  const { version } = ctx.cfg();

  ctx.action().runShell({
    script: `

mix escript.install hex protobuf ${version} --force
mv /warp/home/.mix/escripts/protoc-gen-elixir . 

`,
  });

  const protoc_gen_elixir = "protoc-gen-elixir";
  ctx.action().setPermissions({ file: protoc_gen_elixir, executable: true });

  ctx.action().declareOutputs([protoc_gen_elixir]);

  ctx.provides({ "protoc-gen-elixir": protoc_gen_elixir });
};

export default Warp.Toolchain({
  name: "https://rules.warp.build/toolchains/elixir_protobuf",
  mnemonic: "Elixir/Proto",
  impl,
  cfg: {
    version: string(),
  },
  toolchains: [ErlangToolchain, ElixirToolchain, ProtobufToolchain],
});

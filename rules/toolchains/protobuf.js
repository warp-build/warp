export const PROTO_EXT = ".proto";

const impl = (ctx) => {
  const env = ctx.env();
  const cfg = ctx.cfg();

  let arch = env.host.arch;

  if (arch == "aarch64") {
    arch = "aarch_64";
  }

  let sha1 = cfg.sha1_aarch_64;
  if (arch == "x86_64") {
    sha1 = cfg.sha1_x86_64;
  }

  trace(env.host.arch);

  let os = env.host.os;
  if (os == "darwin") {
    os = "osx";
  }

  const url =
    `https://github.com/protocolbuffers/protobuf/releases/download/v${cfg.version}/protoc-${cfg.version}-${os}-${arch}.zip`;

  const output = `protoc-${cfg.version}.zip`;

  ctx.action().download({ url, sha1, output });

  ctx.action().extract({ src: output, dst: "." });

  const protoc = `bin/protoc`;
  ctx.action().setPermissions({ file: protoc, executable: true });
  ctx.action().declareOutputs(["."]);
  ctx.provides({ protoc });
};

export default Warp.Toolchain({
  name: "https://rules.warp.build/toolchains/protobuf",
  mnemonic: "Protobuf",
  impl,
  cfg: {
    version: string(),
    sha1_aarch64: string(),
    sha1_x86_64: string(),
  },
});

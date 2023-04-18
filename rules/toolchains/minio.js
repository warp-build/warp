const impl = (ctx) => {
  const { host } = ctx.env();
  const { version, sha256 } = ctx.cfg();

  const url =
    `https://dl.min.io/server/minio/release/${host.os}-${host.arch}/minio`;

  const output = "minio.zip";

  ctx.action().download({ url, sha256, output });

  ctx.action().extract({ src: output, dst: "." });

  const MINIO = "minio";
  ctx.action().setPermissions({ file: MINIO, executable: true });

  ctx.action().declareOutputs([MINIO]);

  ctx.provides({ MINIO });
};

export default Warp.Toolchain({
  name: "https://rules.warp.build/toolchains/minio",
  mnemonic: "Minio",
  impl,
  cfg: {
    sha256: string(),
  },
});

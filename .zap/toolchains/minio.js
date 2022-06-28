const impl = ctx => {
  const { host } = ctx.env();
  const { version, sha1 } = ctx.cfg();

  const url = `https://dl.min.io/server/minio/release/${host.os}-${host.arch}/minio`

  const output = "minio.zip"

  ctx.action().download({ url, sha1, output })

  ctx.action().extract({ src: output, dst: "." })

  const MINIO = "minio";
  ctx.action().setPermissions({ file: MINIO, executable: true })

  ctx.action().declareOutputs([]);

  ctx.provides({ MINIO });

};

export default Zap.Toolchain({
  name: "//zap.build/toolchains:minio",
  mnemonic: "Minio",
  impl,
  cfg: {
    sha1: string(),
  }
});


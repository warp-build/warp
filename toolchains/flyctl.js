const impl = ctx => {
  const { version, sha1 } = ctx.cfg();

  const { host } = ctx.env();

  const output = "flyctl.zip"

  let arch = host.arch
  if (arch === "aarch64") {
    arch = "arm64"
  }

  let os = host.os[0].toUpperCase() + host.os.substr(1)
  if (os === "Darwin") {
    os = "macOS"
  }

  const url = `https://github.com/superfly/flyctl/releases/download/v${version}/flyctl_${version}_${os}_${arch}.tar.gz`

  ctx.action().download({ url, sha1, output })

  ctx.action().extract({ src: output, dst: "." })

  const FLYCTL = "flyctl"

  ctx.action().setPermissions({ file: FLYCTL, executable: true })

  ctx.action().declareOutputs([FLYCTL]);
  ctx.action().declareRunScript(FLYCTL);

  ctx.provides({ FLYCTL });
};

export default Warp.Toolchain({
  name: "https://pkgs.warp.build/toolchains/flyctl",
  mnemonic: "FlyCtl",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
  },
  toolchains: []
});

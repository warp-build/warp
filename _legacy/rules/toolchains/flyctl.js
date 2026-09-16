const impl = (ctx) => {
  const { version, sha256_aarch64, sha256_x86_64 } = ctx.cfg();

  const { host } = ctx.env();

  const output = "flyctl.zip";

  let arch = host.arch;
  let sha256 = sha256_aarch64;
  if (arch === "aarch64") {
    arch = "arm64";
  }

  if (arch === "x86_64") {
    sha256 = sha256_x86_64;
  }

  let os = host.os[0].toUpperCase() + host.os.substr(1);
  if (os === "Darwin") {
    os = "macOS";
  }

  const url =
    `https://github.com/superfly/flyctl/releases/download/v${version}/flyctl_${version}_${os}_${arch}.tar.gz`;

  ctx.action().download({ url, sha256, output });

  ctx.action().extract({ src: output, dst: "." });

  const FLYCTL = "flyctl";

  ctx.action().setPermissions({ file: FLYCTL, executable: true });

  ctx.action().declareOutputs([FLYCTL]);
  ctx.action().declareRunScript(FLYCTL);

  ctx.provides({ FLYCTL });
};

export default Warp.Toolchain({
  name: "https://rules.warp.build/toolchains/flyctl",
  mnemonic: "FlyCtl",
  impl,
  cfg: {
    version: string(),
    sha256_aarch64: string(),
    sha256_x86_64: string(),
  },
  toolchains: [],
});

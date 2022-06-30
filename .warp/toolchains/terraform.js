const impl = ctx => {
  const { version, sha1 } = ctx.cfg();

  const { host } = ctx.env();

  const output = "terraform.zip"

  let arch = host.arch
  if (arch === "aarch64") {
    arch = "arm64"
  }

  const url = `https://releases.hashicorp.com/terraform/${version}/terraform_${version}_${host.os}_${arch}.zip`

  ctx.action().download({ url, sha1, output })

  ctx.action().extract({ src: output, dst: "." })

  const terraform = "terraform"

  ctx.action().setPermissions({ file: terraform, executable: true })

  ctx.action().declareOutputs([terraform]);
  ctx.action().declareRunScript(terraform);

  ctx.provides({ terraform });
};

export default Warp.Toolchain({
  name: "//warp.build/toolchains:terraform",
  mnemonic: "terraform",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
  },
  toolchains: []
});


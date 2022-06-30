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

  const TERRAFORM = "terraform"

  ctx.action().setPermissions({ file: TERRAFORM, executable: true })

  ctx.action().declareOutputs([TERRAFORM]);
  ctx.action().declareRunScript(TERRAFORM);

  ctx.provides({ TERRAFORM });
};

export default Warp.Toolchain({
  name: "//warp.build/toolchains:terraform",
  mnemonic: "Terraform",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
  },
  toolchains: []
});


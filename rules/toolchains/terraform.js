const impl = (ctx) => {
  const { version, sha256_aarch64, sha256_x86_64 } = ctx.cfg();

  const { host } = ctx.env();

  const output = "terraform.zip";

  let arch = host.arch;
  let sha256 = sha256_aarch64;
  if (arch === "aarch64") {
    arch = "arm64";
  }

  if (arch === "x86_64") {
    arch = "amd64";
    sha256 = sha256_x86_64;
  }

  const url =
    `https://releases.hashicorp.com/terraform/${version}/terraform_${version}_${host.os}_${arch}.zip`;

  ctx.action().download({ url, sha256, output });

  ctx.action().extract({ src: output, dst: "." });

  const terraform = "terraform";

  ctx.action().setPermissions({ file: terraform, executable: true });

  ctx.action().declareOutputs([terraform]);
  ctx.action().declareRunScript(terraform);

  ctx.provides({ terraform });
};

export default Warp.Toolchain({
  name: "https://rules.warp.build/toolchains/terraform",
  mnemonic: "Terraform",
  impl,
  cfg: {
    version: string(),
    sha256_aarch64: string(),
    sha256_x86_64: string(),
  },
  toolchains: [],
});

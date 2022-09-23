import ErlangToolchain from "https://pkgs.warp.build/toolchains/erlang.js";

const impl = ctx => {
  const { version, release_date, sha1 } = ctx.cfg();
  const { host } = ctx.env();

  const output = "firefly.tar.gz";

  // NOTE(@ostera): this is lumen because that used to be the name of the firefly project
  const prefix = `lumen`;

  let platform = "";
  if (host.os === "darwin") {
    platform = `x86_64-apple-darwin`
  }

  const url = `https://github.com/GetFirefly/firefly/releases/download/${version}-${release_date}/lumen-${version}-nightly-${platform}.tar.gz`

  ctx.action().download({ url, sha1, output })

  ctx.action().extract({ src: output, dst: "." })

  ctx.action().declareOutputs([
    `lumen/bin`,
    `lumen/lib`,
    `lumen/etc`,
  ]);

  ctx.action().writeFile({ dst: "version", data: "1" })

  ctx.action().runShell({
    script: `mv lumen/bin/lumen lumen/bin/firefly`
  })

  ctx.provides({
    firefly: `lumen/bin/firefly`,
  })

  ctx.setEnv({
    FIREFLY_ROOT: ctx.path(`lumen`),
  })
};

export default Warp.Toolchain({
  name: "https://pkgs.warp.build/toolchains/firefly",
  mnemonic: "Firefly",
  impl,
  cfg: {
    version: string(),
    release_date: string(),
    sha1: string(),
  },
  toolchains: [ErlangToolchain]
});

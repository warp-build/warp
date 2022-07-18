import ErlangToolchain from "./erlang.js";

export const BEAM_EXT = ".beam";
export const EX_EXT = ".ex";
export const EXS_EXT = ".exs";

const impl = ctx => {
  const { kind, version, sha1 } = ctx.cfg();

  const output = "Precompiled.zip"

  const url =
    kind === "source"
    ? `https://github.com/elixir-lang/elixir/archive/v${version}.tar.gz`
    : `https://github.com/elixir-lang/elixir/releases/download/v${version}/Precompiled.zip`

  ctx.action().download({ url, sha1, output })

  ctx.action().extract({ src: output, dst: "." })

  if (kind === "source") {
    ctx.action().runShell({
      script: `#!/bin/bash -xe

export PATH="${ErlangToolchain.provides().ERL_ROOT}:$PATH"

cd elixir-${version}
make

`
    })
  }

  const binRoot = `elixir-${version}/bin`;
  const ELIXIR = File.join(binRoot, "elixir");
  const ELIXIRC = File.join(binRoot, "elixirc");
  const IEX = File.join(binRoot, "iex");
  const MIX = File.join(binRoot, "mix");

  ctx.action().setPermissions({ file: ELIXIR, executable: true })
  ctx.action().setPermissions({ file: ELIXIRC, executable: true })
  ctx.action().setPermissions({ file: IEX, executable: true })
  ctx.action().setPermissions({ file: MIX, executable: true })

  ctx.action().declareOutputs([]);

  ctx.provides({ ELIXIR, ELIXIRC, IEX, MIX, ELIXIR_HOME: binRoot });
};

export default Warp.Toolchain({
  name: "//warp.build/toolchains:elixir",
  mnemonic: "Elixir/OTP",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
    kind: string(),
  },
  toolchains: [ErlangToolchain]
});

const impl = ctx => {
  const { version, sha1 } = ctx.cfg();

  const { host } = ctx.env();

  const output = "cmake.zip"

  let os = host.os;
  if (os == "darwin") { os = "macos"; }
  if (os == "win32") { os = "windows"; }

  let arch = host.arch;
  if (os == "macos") { arch = "universal"; }

  let ext = "tar.gz";
  if (os == "win32") { ext = "zip" };

  const cmake = `cmake-${version}-${os}-${arch}`;
  const url = `https://github.com/Kitware/CMake/releases/download/v${version}/${cmake}.${ext}`

  ctx.action().download({ url, sha1, output })

  ctx.action().extract({ src: output, dst: "." })

  ctx.action().declareOutputs([cmake]);

  if (os == "macos") {
    ctx.provides({
      cmake: ctx.path(`${cmake}/CMake.app/Contents/bin/cmake`)
    });
  }
};

export default Warp.Toolchain({
  name: "https://pkgs.warp.build/toolchains/cmake",
  mnemonic: "CMake",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
  },
  toolchains: []
});


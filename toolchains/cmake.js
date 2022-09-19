const impl = ctx => {
  const { version, sha1 } = ctx.cfg();

  const { host } = ctx.env();


  let os = host.os;
  if (os == "darwin") { os = "macos"; }
  if (os == "win32") { os = "windows"; }

  let arch = host.arch;
  if (os == "macos") { arch = "universal"; }

  let ext = "tar.gz";
  if (os == "win32") { ext = "zip" };


  const cmake = `cmake-${version}-${os}-${arch}`;
  const url = `https://github.com/Kitware/CMake/releases/download/v${version}/${cmake}.${ext}`

  const output = `cmake.${ext}`
  ctx.action().download({ url, sha1, output })

  ctx.action().extract({ src: output, dst: "." })

  ctx.action().declareOutputs([
    `${cmake}/CMake.app/Contents/bin`,
  ]);

  if (os == "macos") {
    ctx.provides({
      cmake: `${cmake}/CMake.app/Contents/bin/cmake`,
      cpack: `${cmake}/CMake.app/Contents/bin/cpack`,
      ctest: `${cmake}/CMake.app/Contents/bin/ctest`,
      ccmake: `${cmake}/CMake.app/Contents/bin/ccmake`,
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


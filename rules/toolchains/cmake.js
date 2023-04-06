const impl = (ctx) => {
  const {
    version,
    sha1_macos_aarch64,
    sha1_macos_x86_64,
    sha1_linux_x86_64,
    sha1_linux_aarch64,
  } = ctx.cfg();

  const { host } = ctx.env();

  let os = host.os;
  if (os == "darwin") {
    os = "macos";
  }
  if (os == "win32") {
    os = "windows";
  }

  let arch = host.arch;
  if (os == "macos") {
    arch = "universal";
  }

  let ext = "tar.gz";
  if (os == "win32") {
    ext = "zip";
  }

  let sha1 = sha1_macos_aarch64;
  if (arch === "x86_64" && host.os == "darwin") {
    sha1 = sha1_macos_x86_64;
  }
  if (arch === "x86_64" && host.os == "linux") {
    sha1 = sha1_linux_x86_64;
  }
  if (arch === "aarch64" && host.os == "linux") {
    sha1 = sha1_linux_aarch64;
  }

  const cmake = `cmake-${version}-${os}-${arch}`;
  const url =
    `https://github.com/Kitware/CMake/releases/download/v${version}/${cmake}.${ext}`;

  const output = `cmake.${ext}`;
  ctx.action().download({ url, sha1, output });

  ctx.action().extract({ src: output, dst: "." });

  if (os == "macos") {
    ctx.action().declareOutputs([`${cmake}/CMake.app/Contents/bin`]);

    ctx.provides({
      cmake: `${cmake}/CMake.app/Contents/bin/cmake`,
      cpack: `${cmake}/CMake.app/Contents/bin/cpack`,
      ctest: `${cmake}/CMake.app/Contents/bin/ctest`,
      ccmake: `${cmake}/CMake.app/Contents/bin/ccmake`,
    });
  }

  if (os == "linux") {
    ctx.action().declareOutputs([`${cmake}/bin`]);

    ctx.provides({
      cmake: `${cmake}/bin/cmake`,
      cpack: `${cmake}/bin/cpack`,
      ctest: `${cmake}/bin/ctest`,
      ccmake: `${cmake}/bin/ccmake`,
    });
  }
};

export default Warp.Toolchain({
  name: "https://rules.warp.build/toolchains/cmake",
  mnemonic: "CMake",
  impl,
  cfg: {
    version: string(),
    sha1_macos_aarch64: string(),
    sha1_macos_x86_64: string(),
    sha1_linux_aarch64: string(),
    sha1_linux_x86_64: string(),
  },
  defaults: {
    sha1_macos_aarch64: "ecd0845ad5087211d1e73e41ecfa80eeb754ef2e",
    sha1_macos_x86_64: "12627c808674ee5f5bd3a6982176613bbd3ad9db",
    sha1_linux_aarch64: "e451586e5e2132b4fce9a701493ef74e31f2fa94",
    sha1_linux_x86_64: "0e4bb2905347aaf8634c7d048be69513999453c3",
    version: "3.26.2",
  },
  toolchains: [],
});

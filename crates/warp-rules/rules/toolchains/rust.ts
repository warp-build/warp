const Rustc = Warp.Toolchain({
  name: "rustc",
  program: configuration.compiler,
  versionArgs: ["--version"],
});

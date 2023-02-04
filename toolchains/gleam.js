export const GLEAM_EXT = ".gleam";

const impl = (ctx) => {
  const { unarchivedRoot, archiveKind } = ctx.cfg();

  let binRoot = unarchivedRoot;
  // if (archiveKind === "source") {
  //   ctx.action().exec({
  //     cmd: "make",
  //     args: ["build"],
  //     cwd: unarchivedRoot
  //   });
  //   binRoot = File.join(File.join(unarchivedRoot, "target"), "release")
  // }

  const GLEAM = File.join(binRoot, "gleam");
  ctx.provides({ GLEAM });
  ctx.action().declareOutputs([]);
};

export default Warp.Toolchain({
  name: "https://rules.warp.build/toolchains/gleam",
  mnemonic: "Gleam",
  impl,
  cfg: {
    version: string(),
    sha1: string(),
  },
});

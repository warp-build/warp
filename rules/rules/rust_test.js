import RustToolchain, {
  RS_EXT,
} from "https://rules.warp.build/toolchains/rust.js";

const impl = (ctx) => {
  const { name, test } = ctx.cfg();

  ctx.action().runShell({
    script: `
rustc --test ${name} -o test && \
./test ${test} \
`,
  });

  ctx.action().declareOutputs([]);
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/rust_test",
  mnemonic: "RustTest",
  impl,
  cfg: {
    name: target(),
    test: string(),
    srcs: [file()],
  },
  defaults: {
    test: "",
  },
  toolchains: [RustToolchain],
});

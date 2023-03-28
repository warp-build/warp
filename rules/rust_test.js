import RustToolchain, {
  RS_EXT,
} from "https://rules.warp.build/toolchains/rust.js";

const impl = (ctx) => {
  const { name, tests } = ctx.cfg();

  ctx.action().runShell({
    script: `
rustc --test ${name} -o test && \
./test  ${tests.length > 0 ? `${tests.join(" ")}` : ""} \
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
    tests: [string()],
    srcs: [file()],
  },
  defaults: {
    tests: [],
  },
  toolchains: [RustToolchain],
});

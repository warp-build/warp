import RustToolchain, {
  RS_EXT,
} from "https://rules.warp.build/toolchains/rust.js";

const impl = (ctx) => {
};

export default Warp.Rule({
  name: "https://rules.warp.build/rules/rust_application",
  mnemonic: "RustApp",
  impl,
  cfg: {
    name: target(),
    app_src: file(),
    app_name: string(),
    deps: [target()],
    mod: string(),
    apps: [string()],
  },
  defaults: {
    deps: [],
  },
  toolchains: [RustToolchain],
});

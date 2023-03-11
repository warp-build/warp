import RustToolchain, {
    RS_EXT
} from "https://rules.warp.build/toolchains/rust.js";

const impl = (ctx) => {
    
}

export default Warp.Rule({
    name: "https://rules.warp.build/rules/rust_application",
    mnemonic: "RustApp",
    impl,
    cfg: {
	name: label(),
	app_src: file(),
	app_name: string(),
	deps: [label()],
	mod: string(),
	apps: [string()],
    },
    defaults: {
	deps: [],
    },
    toolchains: [RustToolchain],
})

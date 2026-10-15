//! Rule definitions consumed by Warp's build engine.

pub const BUN_TARGET_RULE: &str = include_str!("../rules/bun/target.ts");
pub const BUN_TOOLCHAIN_RULE: &str = include_str!("../rules/toolchains/bun.ts");
pub const CARGO_TOOLCHAIN_RULE: &str = include_str!("../rules/toolchains/cargo.ts");
pub const CARGO_WORKSPACE_RULE: &str = include_str!("../rules/cargo/workspace.ts");
pub const RUST_TARGET_RULE: &str = include_str!("../rules/rust/target.ts");
pub const RUST_TOOLCHAIN_RULE: &str = include_str!("../rules/toolchains/rust.ts");

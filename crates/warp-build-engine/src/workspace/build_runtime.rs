use super::bun::BunToolchain;
use super::{WorkDirectory, Workspace};
use crate::action_cache::ActionCache;
use crate::fingerprint::rustc_for_toolchain;
use crate::RustToolchainChannel;
use std::path::PathBuf;
use std::sync::OnceLock;
use warp_store::Cas;

pub(super) struct BuildRuntime {
    pub(super) workspace: Workspace,
    pub(super) _work: WorkDirectory,
    pub(super) compiler: OnceLock<std::result::Result<(PathBuf, String), String>>,
    pub(super) bun: OnceLock<std::result::Result<BunToolchain, String>>,
    pub(super) action_cache: ActionCache,
    pub(super) cas: Cas,
    rust_toolchain: Option<RustToolchainChannel>,
}

impl BuildRuntime {
    pub(super) fn new(
        workspace: Workspace,
        work: WorkDirectory,
        action_cache: ActionCache,
        cas: Cas,
        rust_toolchain: Option<RustToolchainChannel>,
    ) -> Self {
        Self {
            workspace,
            _work: work,
            compiler: OnceLock::new(),
            bun: OnceLock::new(),
            action_cache,
            cas,
            rust_toolchain,
        }
    }

    pub(super) fn compiler(&self) -> std::result::Result<&(PathBuf, String), String> {
        self.compiler
            .get_or_init(|| {
                rustc_for_toolchain(self.rust_toolchain.as_ref()).map_err(|error| error.to_string())
            })
            .as_ref()
            .map_err(Clone::clone)
    }

    pub(super) fn bun_toolchain(&self) -> std::result::Result<&BunToolchain, String> {
        self.bun
            .get_or_init(|| BunToolchain::resolve().map_err(|error| error.to_string()))
            .as_ref()
            .map_err(Clone::clone)
    }
}

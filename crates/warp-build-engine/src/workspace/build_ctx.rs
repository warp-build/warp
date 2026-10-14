use super::{build_intent::BuildIntent, build_runtime::BuildRuntime};
use std::path::PathBuf;
use std::sync::{Mutex, OnceLock};
use std::time::Instant;

/// Per-invocation state shared by the build-intent node and its dynamically discovered tasks.
pub struct BuildCtx {
    pub(super) intent: BuildIntent,
    pub(super) started: Instant,
    runtime: OnceLock<std::result::Result<BuildRuntime, String>>,
    output: Mutex<Option<PathBuf>>,
}

impl BuildCtx {
    pub fn new(intent: BuildIntent) -> Self {
        Self {
            intent,
            started: Instant::now(),
            runtime: OnceLock::new(),
            output: Mutex::new(None),
        }
    }

    pub(super) fn runtime(
        &self,
        initialize: impl FnOnce() -> crate::error::Result<BuildRuntime>,
    ) -> std::result::Result<&BuildRuntime, String> {
        self.runtime
            .get_or_init(|| {
                let started = Instant::now();
                let result = initialize().map_err(|error| error.to_string());
                if self.intent.verbose {
                    eprintln!(
                        "[profile] workspace runtime initialization={}ms",
                        started.elapsed().as_millis()
                    );
                }
                result
            })
            .as_ref()
            .map_err(Clone::clone)
    }

    pub(super) fn set_output(&self, output: Option<PathBuf>) {
        *self.output.lock().expect("build output lock poisoned") = output;
    }

    pub(super) fn output(&self) -> Option<PathBuf> {
        self.output
            .lock()
            .expect("build output lock poisoned")
            .clone()
    }
}

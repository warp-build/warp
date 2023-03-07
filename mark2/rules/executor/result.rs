use crate::model::RunScript;
use fxhash::{FxHashMap, FxHashSet};
use std::path::PathBuf;

type Action = ();

#[derive(Default)]
pub struct ExecutionResult {
    pub(crate) actions: Vec<Action>,
    pub(crate) env: FxHashMap<String, String>,
    pub(crate) outs: FxHashSet<PathBuf>,
    pub(crate) provides: FxHashMap<String, PathBuf>,
    pub(crate) run_script: Option<RunScript>,
    pub(crate) srcs: FxHashSet<PathBuf>,
}

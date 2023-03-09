use crate::executor::actions::Action;
use crate::model::RunScript;
use fxhash::{FxHashMap, FxHashSet};
use std::collections::BTreeMap;
use std::path::PathBuf;

#[derive(Default, Debug)]
pub struct ExecutionResult {
    pub(crate) actions: Vec<Action>,
    pub(crate) env: FxHashMap<String, String>,
    pub(crate) outs: FxHashSet<PathBuf>,
    pub(crate) provides: BTreeMap<String, PathBuf>,
    pub(crate) run_script: Option<RunScript>,
    pub(crate) srcs: FxHashSet<PathBuf>,
}

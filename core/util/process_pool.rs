use dashmap::DashMap;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::path::PathBuf;
use std::process::Stdio;
use thiserror::Error;
use tokio::process::{Child, Command};

#[derive(Copy, Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct ProcessId<P>(uuid::Uuid, PhantomData<P>);

/// A Process Pool that can be used to manage processes that share the same interface.
pub struct ProcessPool<P> {
    processes: DashMap<uuid::Uuid, Child>,
    _process_type: PhantomData<P>,
}

impl<P> ProcessPool<P> {
    pub fn new() -> Self {
        Self {
            processes: Default::default(),
            _process_type: Default::default(),
        }
    }

    pub async fn spawn(
        &self,
        process_spec: ProcessSpec<P>,
    ) -> Result<ProcessId<P>, ProcessPoolError> {
        let mut cmd = Command::new(process_spec.bin.clone());

        let mut env = process_spec.env.clone();
        env.insert(
            "PATH".into(),
            format!(
                "{}:/usr/bin:/usr/sbin:/bin:/sbin",
                env.get("PATH").unwrap_or(&String::default())
            ),
        );

        if std::env::var("WARP_TRICORDER_LOGS")
            .unwrap_or_default()
            .is_empty()
        {
            cmd.stdout(Stdio::null()).stderr(Stdio::null());
        }

        cmd.env_clear()
            .envs(env)
            .args(process_spec.args.clone())
            .kill_on_drop(true);

        if let Some(current_dir) = &process_spec.current_dir {
            cmd.current_dir(current_dir.clone());
        }

        let handle = cmd.spawn()?;

        let next_id = uuid::Uuid::new_v4();
        self.processes.insert(next_id, handle);

        Ok(ProcessId(next_id, PhantomData::default()))
    }
}

impl<P> Default for ProcessPool<P> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Default)]
pub struct ProcessSpec<P: ?Sized> {
    pub bin: PathBuf,
    pub args: Vec<String>,
    pub env: HashMap<String, String>,
    pub current_dir: Option<PathBuf>,
    pub _process_type: PhantomData<P>,
}

#[derive(Error, Debug)]
pub enum ProcessPoolError {
    #[error("Something went wrong with the Process Pool")]
    Unknown,
    #[error(transparent)]
    CommandSpawningError(std::io::Error),
}

impl From<std::io::Error> for ProcessPoolError {
    fn from(value: std::io::Error) -> Self {
        Self::CommandSpawningError(value)
    }
}

use std::sync::RwLock;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Status {
    Running,
    Shutdown,
}

#[derive(Debug)]
pub struct BuildCoordinator {
    pub status: RwLock<Status>,
}

impl BuildCoordinator {
    pub fn new() -> BuildCoordinator {
        BuildCoordinator {
            status: RwLock::new(Status::Running),
        }
    }

    pub fn should_shutdown(&self) -> bool {
        *(self.status.read().unwrap()) == Status::Shutdown
    }

    pub fn signal_shutdown(&self) {
        *(self.status.write().unwrap()) = Status::Shutdown
    }
}

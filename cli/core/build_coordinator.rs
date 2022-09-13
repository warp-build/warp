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

impl Default for BuildCoordinator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn when_starting_it_should_not_shutdown() {
        let bc = BuildCoordinator::new();
        assert!(!bc.should_shutdown());
    }

    #[test]
    fn after_signaling_to_shutdown_it_should_shutdown() {
        let bc = BuildCoordinator::new();
        bc.signal_shutdown();
        assert!(bc.should_shutdown());
    }

    #[test]
    fn multiple_signalign_does_not_toggle_shutdown() {
        let bc = BuildCoordinator::new();
        bc.signal_shutdown();
        assert!(bc.should_shutdown());
        bc.signal_shutdown();
        assert!(bc.should_shutdown());
        bc.signal_shutdown();
        assert!(bc.should_shutdown());
    }
}

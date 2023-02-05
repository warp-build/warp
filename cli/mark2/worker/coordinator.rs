use std::sync::RwLock;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Status {
    Running,
    Shutdown,
}

#[derive(Debug)]
pub struct Coordinator {
    pub status: RwLock<Status>,
}

impl Coordinator {
    pub fn new() -> Self {
        Self {
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

impl Default for Coordinator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn when_starting_it_should_not_shutdown() {
        let bc = Coordinator::new();
        assert!(!bc.should_shutdown());
    }

    #[test]
    fn after_signaling_to_shutdown_it_should_shutdown() {
        let bc = Coordinator::new();
        bc.signal_shutdown();
        assert!(bc.should_shutdown());
    }

    #[test]
    fn multiple_signalign_does_not_toggle_shutdown() {
        let bc = Coordinator::new();
        bc.signal_shutdown();
        assert!(bc.should_shutdown());
        bc.signal_shutdown();
        assert!(bc.should_shutdown());
        bc.signal_shutdown();
        assert!(bc.should_shutdown());
    }
}

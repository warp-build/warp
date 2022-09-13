use super::*;
use std::sync::Arc;

#[derive(Clone, Debug, Default)]
pub struct EventChannel {
    bus: Arc<crossbeam::deque::Injector<Event>>,
}

impl EventChannel {
    pub fn new() -> EventChannel {
        EventChannel::default()
    }

    pub fn send(&self, event: Event) {
        self.bus.push(event)
    }

    pub fn recv(&self) -> Option<Event> {
        if let crossbeam::deque::Steal::Success(event) = self.bus.steal() {
            Some(event)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_only_receive_after_send() {
        let ec = EventChannel::new();
        assert!(matches!(ec.recv(), None));
        ec.send(Event::QueueingWorkspace);
        assert!(matches!(ec.recv(), Some(Event::QueueingWorkspace)));
    }

    #[test]
    fn receive_consumes_a_message() {
        let ec = EventChannel::new();
        ec.send(Event::QueueingWorkspace);
        assert!(matches!(ec.recv(), Some(Event::QueueingWorkspace)));
        assert!(matches!(ec.recv(), None));
    }
}

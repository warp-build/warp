use super::*;
use std::sync::Arc;

#[derive(Clone, Debug)]
pub struct EventChannel {
    bus: Arc<crossbeam::deque::Injector<Event>>,
}

impl EventChannel {
    pub fn new() -> EventChannel {
        let bus = Arc::new(crossbeam::deque::Injector::new());
        EventChannel { bus }
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

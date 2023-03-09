use super::*;
use std::sync::Arc;

pub struct EventConsumer {
    channel: Arc<crossbeam::deque::Injector<Event>>,
    queue: crossbeam::deque::Worker<Event>,
}

impl EventConsumer {
    pub fn fetch(&self) {
        let _steal = self.channel.steal_batch(&self.queue);
    }

    pub fn pop(&self) -> Option<Event> {
        self.queue.pop()
    }

    pub fn is_empty(&self) -> bool {
        let _steal = self.channel.steal_batch(&self.queue);
        self.queue.is_empty()
    }
}

impl Iterator for &EventConsumer {
    type Item = Event;

    fn next(&mut self) -> Option<Self::Item> {
        self.pop()
    }
}

#[derive(Clone, Debug, Default)]
pub struct EventChannel {
    bus: Arc<crossbeam::deque::Injector<Event>>,
}

impl EventChannel {
    pub fn new() -> EventChannel {
        EventChannel::default()
    }

    pub fn is_empty(&self) -> bool {
        self.bus.is_empty()
    }

    pub fn send(&self, event: Event) {
        self.bus.push(event)
    }

    pub fn consumer(&self) -> EventConsumer {
        EventConsumer {
            channel: self.bus.clone(),
            queue: crossbeam::deque::Worker::new_fifo(),
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

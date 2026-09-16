use super::event::Event;
use crate::sync::Arc;
use crossbeam::deque::{Injector, Worker};

pub struct EventConsumer {
    channel: Arc<Injector<Event>>,
    queue: Worker<Event>,
}

impl EventConsumer {
    pub fn new(channel: Arc<Injector<Event>>, queue: Worker<Event>) -> Self {
        Self { channel, queue }
    }

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

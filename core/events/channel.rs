use super::event::Event;
use super::EventConsumer;
use crate::sync::Arc;
use crossbeam::deque::{Injector, Worker};

#[derive(Clone, Debug, Default)]
pub struct EventChannel {
    bus: Arc<Injector<Event>>,
}

impl EventChannel {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn is_empty(&self) -> bool {
        self.bus.is_empty()
    }

    pub fn send<E>(&self, event: E)
    where
        E: Into<Event>,
    {
        self.bus.push(event.into())
    }

    pub fn consumer(&self) -> EventConsumer {
        EventConsumer::new(self.bus.clone(), Worker::new_fifo())
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_event_channel_is_empty() {
        let ec = EventChannel::new();
        assert!(ec.is_empty());
    }

    #[test]
    fn new_event_consumer_is_empty_on_empty_channel() {
        let ec = EventChannel::new();
        let c = ec.consumer();
        assert!(c.is_empty());
    }

    #[quickcheck]
    fn new_event_consumer_is_nonempty_on_nonempty_channel(event: Event) {
        let ec = EventChannel::new();
        let c = ec.consumer();
        ec.send(event);
        assert!(!c.is_empty());
    }

    #[quickcheck]
    fn event_consumer_needs_to_fetch_to_get_messages(event: Event) {
        let ec = EventChannel::new();
        let c = ec.consumer();
        ec.send(event);
        assert!(c.pop().is_none());
        c.fetch();
        assert!(c.pop().is_some());
    }

    #[quickcheck]
    fn events_are_received_in_the_order_they_are_sent(events: Vec<Event>) {
        let ec = EventChannel::new();
        let c = ec.consumer();

        // first we send all the events
        for event in &events {
            ec.send(event.clone());
        }

        // then we fetch and receive them
        for event in events {
            c.fetch();
            let received_event = c.pop().unwrap();
            assert_eq!(received_event, event)
        }

        // then the queue should be empty
        assert!(ec.is_empty());
        assert!(c.pop().is_none());
    }

    #[quickcheck]
    fn receive_consumes_a_message(event: Event) {
        let ec = EventChannel::new();
        let c = ec.consumer();

        assert!(c.pop().is_none());
        ec.send(event.clone());
        c.fetch();
        assert_matches!(c.pop(), Some(e) if e == event);
        assert!(c.pop().is_none());
    }
}

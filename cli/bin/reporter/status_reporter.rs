use indicatif::{ProgressBar, ProgressStyle};
use std::collections::HashMap;
use std::sync::Arc;
use zap_core::*;

#[derive(Default)]
pub struct ProgressBarManager {
    pbs: HashMap<Label, ProgressBar>,
}

impl ProgressBarManager {
    pub fn new() -> ProgressBarManager {
        ProgressBarManager::default()
    }

    pub fn add(&mut self, label: &Label, rule_mnemonic: String) {
        let pb = ProgressBar::new(100);
        pb.set_style(
            ProgressStyle::default_bar()
                .template("[{prefix:<0.cyan.bold}] {msg:<0.green.bold} [{bar:37}] {pos}/{len}")
                .progress_chars("=> "),
        );

        pb.set_prefix(rule_mnemonic);
        pb.set_message(label.name());

        self.pbs.insert(label.clone(), pb);
    }

    pub fn inc(&mut self, label: &Label, value: u64) {
        match self.pbs.get(label) {
            Some(pb) => pb.inc(value),
            None => (),
        }
    }

    pub fn finish_and_clear(&mut self, label: Label) {
        match self.pbs.get(&label) {
            Some(pb) => pb.finish_and_clear(),
            None => (),
        }
    }
}

pub struct StatusReporter {
    event_channel: Arc<EventChannel>,
}

impl StatusReporter {
    pub fn new(event_channel: Arc<EventChannel>) -> StatusReporter {
        StatusReporter { event_channel }
    }
    pub async fn run(self) {
        let mut status = ProgressBarManager::new();
        loop {
            tokio::time::sleep(std::time::Duration::from_millis(1)).await;
            if let Some(event) = self.event_channel.recv() {
                match event {
                    zap_core::Event::BuildingTarget {
                        label,
                        rule_mnemonic,
                    } => {
                        status.add(&label, rule_mnemonic);
                        status.inc(&label, 1);
                    }
                    zap_core::Event::ArchiveVerifying(label) => status.inc(&label, 1),
                    zap_core::Event::ArchiveUnpacking(label) => status.inc(&label, 1),
                    zap_core::Event::ActionRunning { label, .. } => status.inc(&label, 1),
                    zap_core::Event::ArchiveDownloading { label, .. } => status.inc(&label, 1),
                    zap_core::Event::RequeueingTarget(_, _) => (),
                    zap_core::Event::TargetBuilt(label) => status.finish_and_clear(label),
                    zap_core::Event::CacheHit(label, _) => status.finish_and_clear(label),
                    zap_core::Event::BuildCompleted => {
                        return;
                    }
                }
            }
        }
    }
}

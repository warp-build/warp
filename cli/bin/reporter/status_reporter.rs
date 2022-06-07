use indicatif::{ProgressBar, ProgressStyle};
use std::sync::Arc;
use std::collections::HashSet;
use zap_core::*;

pub struct StatusReporter {
    event_channel: Arc<EventChannel>,
}

impl StatusReporter {
    pub fn new(event_channel: Arc<EventChannel>) -> StatusReporter {
        StatusReporter { event_channel }
    }
    pub async fn run(self, target: Label) {
        let green_bold = console::Style::new().green().bold();
        let blue_dim = console::Style::new().blue().dim();
        let red_bold = console::Style::new().red().bold();

        let pb = ProgressBar::new(100);
        pb.set_style(
            ProgressStyle::default_bar()
                .template("{prefix:>12.cyan.bold} [{bar:25}] {pos}/{len} {wide_msg}")
                .progress_chars("=> "),
        );
        pb.set_prefix("Building");

        let mut current_targets: HashSet<Label> = HashSet::new();

        let mut errored = false;

        loop {
            tokio::time::sleep(std::time::Duration::from_millis(1)).await;

            let message = current_targets.iter().map(|l| l.name()).collect::<Vec<String>>().join(", ");
						pb.set_message(message);

            if let Some(event) = self.event_channel.recv() {
                match event {
                    zap_core::Event::BuildingTarget {
                        label,
                        rule_mnemonic,
                    } => {
                        current_targets.insert(label);
                    },
                    zap_core::Event::ArchiveVerifying(label) => (),
                    zap_core::Event::ArchiveUnpacking(label) => (),
                    zap_core::Event::ActionRunning { label, .. } => (),
                    zap_core::Event::ArchiveDownloading { label, .. } => (),
                    zap_core::Event::RequeueingTarget(_, _) => (),
                    zap_core::Event::CacheHit(label, _) => {
                        let line = format!(
                            "{:>12} {}",
                            blue_dim.apply_to("Cache-hit"),
                            label.to_string(),
                        );
                        current_targets.remove(&label);
                        pb.println(line);
                    }
                    zap_core::Event::TargetBuilt(label) => {
                        let line = format!(
                            "{:>12} {}",
                            green_bold.apply_to("Built"),
                            label.to_string(),
                        );
                        current_targets.remove(&label);
                        pb.println(line);
                    }
                    zap_core::Event::BuildError(label, err) => {
                        errored = true;
                        let line = format!(
                            "{:>12} {}",
                            red_bold.apply_to("ERROR"),
                            label.to_string(),
                        );
                        pb.println(line);
                        pb.println(format!("{}", err));
                    }
                    zap_core::Event::BuildCompleted => {
                        let line = format!(
                            "{:>12} {}",
                            if errored {
                                red_bold.apply_to("Finished with errors")
                            } else {
                                green_bold.apply_to("Finished")
                            },
                            target.to_string(),
                        );
                        pb.println(line);
                        return;
                    }
                }
            }
        }
    }
}

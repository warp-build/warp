use indicatif::{ProgressBar, ProgressStyle};
use std::sync::Arc;
use fxhash::*;
use zap_core::*;
use tracing::*;

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

        let pb = ProgressBar::new(0);
        pb.set_style(
            ProgressStyle::default_bar()
                .template("{prefix:>12.cyan.bold} [{bar:25}] {pos}/{len} {wide_msg}")
                .progress_chars("=> "),
        );
        pb.set_prefix("Building");

        let mut current_targets: FxHashSet<Label> = FxHashSet::default();
        let mut queued_targets: FxHashSet<Label> = FxHashSet::default();

        let mut errored = false;
        let mut target_count = 0;
        let mut cache_hits = 0;
        let mut action_count = 0;

        loop {
            tokio::time::sleep(std::time::Duration::from_millis(1)).await;

            let message = current_targets.iter().map(|l| l.name()).collect::<Vec<String>>().join(", ");
						pb.set_message(message);
            pb.set_length(queued_targets.len() as u64 + action_count);

            if let Some(event) = self.event_channel.recv() {
                debug!("{:#?}", event);

                use zap_core::Event::*;
                match event {
                    BuildingTarget { label, .. } => {
                        current_targets.insert(label);
                    },
                    QueuedTarget(label) => {
                        queued_targets.insert(label);
                    },
                    QueuedTargets(count) => pb.set_length(count as u64),
                    ArchiveVerifying(_label) => {
                        action_count += 1;
                        pb.inc(1)
                    },
                    ArchiveUnpacking(_label) => {
                        action_count += 1;
                        pb.inc(1)
                    },
                    ActionRunning { .. } => {
                        action_count += 1;
                        pb.inc(1)
                    },
                    ArchiveDownloading { label, .. } => {
                        let line = format!(
                            "{:>12} {}",
                            blue_dim.apply_to("Downloading"),
                            label.to_string(),
                        );
                        pb.println(line);
                        pb.set_length(pb.length()+1);
                        pb.inc(1)
                    },
                    RequeueingTarget(_, _) => (),
                    CacheMiss{ .. } => {
                        // let line = format!(
                        //     "{:>12} {}",
                        //     blue_dim.apply_to("Cache-miss"),
                        //     label.to_string(),
                        // );
                        // pb.println(line);
                        // pb.println(format!("{:?}", local_path));
                    }
                    CacheHit(label, _) => {
                        let line = format!(
                            "{:>12} {}",
                            blue_dim.apply_to("Cache-hit"),
                            label.to_string(),
                        );
                        current_targets.remove(&label);
                        pb.println(line);
                        pb.inc(1);
                        cache_hits += 1;
                    }
                    TargetBuilt(label) => {
                        let line = format!(
                            "{:>12} {}",
                            green_bold.apply_to("Built"),
                            label.to_string(),
                        );
                        current_targets.remove(&label);
                        pb.println(line);
                        pb.inc(1);
                        target_count += 1;
                    }
                    BuildError(label, err) => {
                        errored = true;
                        let line = format!(
                            "{:>12} {}",
                            red_bold.apply_to("ERROR"),
                            label.to_string(),
                        );
                        pb.println(line);
                        pb.println(format!("{}", err));
                    }
                    BuildCompleted => {
                        let line = format!(
                            "{:>12} {} ({} targets, {} cached)",
                            if errored {
                                red_bold.apply_to("Finished with errors")
                            } else {
                                green_bold.apply_to("Finished")
                            },
                            target.to_string(),
                            target_count,
                            cache_hits,
                        );
                        pb.println(line);
                        return;
                    }
                }
            }
        }
    }
}

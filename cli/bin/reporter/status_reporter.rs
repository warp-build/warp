use fxhash::*;
use indicatif::{ProgressBar, ProgressStyle};
use std::sync::Arc;
use tracing::*;
use warp_core::*;

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
        let yellow = console::Style::new().yellow();
        let red_bold = console::Style::new().red().bold();

        let pb = ProgressBar::new(0);
        pb.set_style(
            ProgressStyle::default_bar()
                .template("{prefix:>12.cyan.bold} [{bar:25}] {pos}/{len} {wide_msg}")
                .progress_chars("=> "),
        );
        pb.set_prefix("Building");

        let mut current_targets: FxHashSet<Label> = FxHashSet::default();

        let mut action_count = 0;
        let mut cache_hits = 0;
        let mut error_count = 0;
        let mut errored = false;
        let mut queued_targets = 0;
        let mut target_count = 0;

        let mut build_started = std::time::Instant::now();
        loop {
            tokio::time::sleep(std::time::Duration::from_micros(10)).await;

            pb.set_length(queued_targets + action_count);

            if let Some(event) = self.event_channel.recv() {
                debug!("{:#?}", event);

                use warp_core::Event::*;
                match event {
                    BuildingTarget { label, .. } => {
                        current_targets.insert(label);
                        let current_targets_names = current_targets
                            .iter()
                            .map(|l| l.name())
                            .collect::<Vec<String>>();
                        pb.set_message(format!("Pending: {}", current_targets_names.join(", ")));
                    }
                    QueueingWorkspace => {
                        let line = format!(
                            "{:>12} {}",
                            blue_dim.apply_to("Prepare"),
                            "Queueing entire workspace...",
                        );
                        pb.println(line);
                    }
                    QueuedTargets(count) => queued_targets += count,
                    ArchiveVerifying(label) => {
                        let line =
                            format!("{:>12} {}", yellow.apply_to("Verifying"), label.to_string(),);
                        pb.println(line);
                        pb.set_length(pb.length() + 1);
                        pb.inc(1)
                    }
                    ArchiveUnpacking(label) => {
                        let line =
                            format!("{:>12} {}", yellow.apply_to("Unpacking"), label.to_string(),);
                        pb.println(line);
                        pb.set_length(pb.length() + 1);
                        pb.inc(1)
                    }
                    ActionRunning { .. } => pb.inc(1),
                    ArchiveDownloading { label, .. } => {
                        let line = format!(
                            "{:>12} {}",
                            yellow.apply_to("Downloading"),
                            label.to_string(),
                        );
                        pb.println(line);
                        pb.set_length(pb.length() + 1);
                        pb.inc(1)
                    }
                    PreparingActions {
                        action_count: ac, ..
                    } => {
                        action_count += ac as u64;
                    }
                    CacheHit(label) => {
                        let line = format!(
                            "{:>12} {}",
                            blue_dim.apply_to("Cache-hit"),
                            label.to_string(),
                        );
                        current_targets.remove(&label);
                        let current_targets_names = current_targets
                            .iter()
                            .map(|l| l.name())
                            .collect::<Vec<String>>();
                        pb.set_message(format!("Pending: {}", current_targets_names.join(", ")));
                        pb.println(line);
                        pb.inc(1);
                        cache_hits += 1;
                    }
                    TargetBuilt(label) => {
                        let line =
                            format!("{:>12} {}", green_bold.apply_to("Built"), label.to_string(),);
                        current_targets.remove(&label);
                        let current_targets_names = current_targets
                            .iter()
                            .map(|l| l.name())
                            .collect::<Vec<String>>();
                        pb.set_message(format!("Pending: {}", current_targets_names.join(", ")));
                        pb.println(line);
                        pb.inc(1);
                        target_count += 1;
                    }
                    ErrorLoadingRule(name, err) => {
                        errored = true;
                        error_count += 1;
                        let line = format!(
                            "{:>12} {} {}",
                            red_bold.apply_to("ERROR"),
                            "error when loading ",
                            name
                        );
                        pb.println(line);
                        pb.println(format!("{}", err));
                    }
                    BadBuildfile(path, err) => {
                        errored = true;
                        error_count += 1;
                        let line = format!(
                            "{:>12} {} {}",
                            red_bold.apply_to("ERROR"),
                            "error when reading ",
                            path.to_str().unwrap()
                        );
                        pb.println(line);
                        pb.println(format!("{}", err));
                    }
                    WorkerError(err) => {
                        errored = true;
                        error_count += 1;
                        let line = format!(
                            "{:>12} {}",
                            red_bold.apply_to("ERROR"),
                            "something went wrong with a worker"
                        );
                        pb.println(line);
                        pb.println(format!("{}", err));
                    }
                    BuildError(label, err) => {
                        errored = true;
                        error_count += 1;
                        let line =
                            format!("{:>12} {}", red_bold.apply_to("ERROR"), label.to_string(),);
                        pb.println(line);
                        pb.println(format!("{}", err));
                    }
                    BuildStarted(t0) => {
                        build_started = t0;
                    }
                    EmptyWorkspace(t1) | BuildCompleted(t1) => {
                        if let EmptyWorkspace(_) = &event {
                            pb.println(format!(
                                "{:>12} {}",
                                blue_dim.apply_to("Prepare"),
                                "Nothing to do in an empty workspace."
                            ));
                        }

                        let line = format!(
                            "{:>12} {} in {}ms ({} targets, {} cached, {} errors)",
                            if errored {
                                red_bold.apply_to("Finished with errors")
                            } else {
                                green_bold.apply_to("Finished")
                            },
                            target.to_string(),
                            t1.saturating_duration_since(build_started).as_millis(),
                            target_count,
                            cache_hits,
                            error_count,
                        );
                        pb.println(line);
                        return;
                    }
                }
            }
        }
    }
}

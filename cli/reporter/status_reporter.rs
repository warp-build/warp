use crate::commands::Flags;
use dashmap::DashSet;
use fxhash::*;
use indicatif::{ProgressBar, ProgressStyle};
use std::sync::Arc;
use tracing::*;
use warp_core::*;

pub struct StatusReporter {
    event_consumer: EventConsumer,
    event_channel: Arc<EventChannel>,
    flags: Flags,
    goal: Goal,
}

impl StatusReporter {
    pub fn new(event_channel: Arc<EventChannel>, flags: Flags, goal: Goal) -> StatusReporter {
        StatusReporter {
            event_consumer: event_channel.consumer(),
            event_channel,
            flags,
            goal,
        }
    }
    pub async fn run(self, targets: &[Label]) {
        let targets = targets.to_vec();
        let handle = std::thread::spawn(move || {
            let green_bold = console::Style::new().green().bold();
            let purple = console::Style::new().magenta().bright();
            let blue_dim = console::Style::new().blue();
            let yellow = console::Style::new().yellow();
            let red_bold = console::Style::new().red().bold();

            let info = console::Style::new().on_blue().bright();

            let style = ProgressStyle::default_bar()
                .template("{prefix:>12.cyan.bold} [{bar:25}] {pos}/{len} {wide_msg}")
                .progress_chars("=> ");

            let pb = ProgressBar::new(100);
            pb.set_style(style);
            pb.set_prefix("Building");

            let current_targets: DashSet<Label> = DashSet::default();

            let cache_hits: DashSet<Label> = DashSet::default();
            let queued_labels: DashSet<String> = DashSet::default();

            let mut error_count = 0;
            let mut errored = false;
            let mut target_count: FxHashSet<Label> = FxHashSet::default();
            let hashed_count: DashSet<LabelId> = DashSet::default();

            let mut build_started = std::time::Instant::now();
            'main_loop: loop {
                self.event_consumer.fetch();
                for event in &self.event_consumer {
                    debug!("{:#?}", event);

                    pb.set_length(queued_labels.len() as u64);
                    use warp_core::Event::*;
                    match event {
                        HandlingTarget { label, .. } => {
                            if self.flags.show_queued_events {
                                let line = format!(
                                    "   {} {}",
                                    info.apply_to("HANDLING"),
                                    label.to_string()
                                );
                                pb.println(line);
                            }
                        }
                        BuildingTarget { label, .. } => {
                            current_targets.insert(label.clone());
                        }

                        QueuedSkipLabel { label } => {
                            if self.flags.show_queued_events {
                                let line = format!(
                                    " {} {}",
                                    info.apply_to("QUEUE SKIP"),
                                    label.to_string()
                                );
                                pb.println(line);
                            }
                        }

                        QueuedLabel { label, .. } => {
                            if self.flags.show_queued_events {
                                let line = format!(
                                    "     {} {}",
                                    info.apply_to("QUEUED"),
                                    label.to_string()
                                );
                                pb.println(line);
                            }
                            queued_labels.insert(label.to_hash_string());
                        }

                        QueueingWorkspace => {
                            let line = format!(
                                "{:>12} {}",
                                blue_dim.apply_to("Prepare"),
                                "Queueing entire workspace...",
                            );
                            pb.println(line);
                        }

                        QueuedTargets(count) => {
                            if self.flags.show_queued_events {
                                let line =
                                    format!("     {} {} targets", info.apply_to("QUEUED"), count,);
                                pb.println(line);
                            }
                        }

                        ResolvingDependency { label, resolver } => {
                            let line = format!(
                                "{:>12} {} using {}",
                                yellow.apply_to("Resolving"),
                                label.to_string(),
                                resolver.to_string(),
                            );
                            pb.println(line);
                            pb.set_length(pb.length() + 1);
                        }

                        ArchiveVerifying(label) => {
                            let line = format!(
                                "{:>12} {}",
                                yellow.apply_to("Verifying"),
                                label.to_string(),
                            );
                            pb.println(line);
                            pb.set_length(pb.length() + 1);
                        }

                        ArchiveUnpacking(label) => {
                            let line = format!(
                                "{:>12} {}",
                                yellow.apply_to("Unpacking"),
                                label.to_string(),
                            );
                            pb.println(line);
                            pb.set_length(pb.length() + 1);
                        }

                        ActionRunning { .. } => (),

                        ArchiveDownloading { label, .. } => {
                            let line = format!(
                                "{:>12} {}",
                                yellow.apply_to("Downloading"),
                                label.to_string(),
                            );
                            pb.println(line);
                            pb.set_length(pb.length() + 1);
                        }

                        PreparingActions { .. } => {}

                        StartedService { label } => {
                            let line = format!(
                                "{:>12} {}",
                                purple.apply_to("Started"),
                                label.to_string(),
                            );
                            pb.println(line);
                        }

                        AnalyzingSource { label } => {
                            let line = format!(
                                "{:>12} {}",
                                purple.apply_to("Analyzing"),
                                label.to_string(),
                            );
                            pb.println(line);
                        }

                        GeneratingSignature { label } => {
                            let line = format!(
                                "{:>12} {}",
                                purple.apply_to("Generating"),
                                label.to_string(),
                            );
                            pb.println(line);
                        }

                        HashedLabel {
                            label,
                            src_hash,
                            ast_hash,
                        } => {
                            let line = format!(
                                "{:>12} {} (src={}, ast={})",
                                purple.apply_to("Hashed"),
                                label,
                                src_hash,
                                ast_hash,
                            );
                            hashed_count.insert(label);
                            pb.println(line);
                        }

                        CacheHit { label, goal } => {
                            current_targets.remove(&label);
                            pb.set_message(format!(
                                " {}",
                                current_targets
                                    .iter()
                                    .map(|l| l.to_string())
                                    .collect::<Vec<String>>()
                                    .join(", ")
                            ));

                            if self.flags.show_cache_hits || goal.is_test() {
                                let line = format!(
                                    "{:>12} {} {}",
                                    if goal.is_test() {
                                        green_bold.apply_to("PASS")
                                    } else {
                                        blue_dim.apply_to("Cache-hit")
                                    },
                                    label.to_string(),
                                    if goal.is_test() { "(CACHED)" } else { "" }
                                );
                                pb.println(line);
                            }

                            pb.inc(1);
                            cache_hits.insert(label);
                        }

                        TargetBuilt { label, goal } => {
                            let line = format!(
                                "{:>12} {}",
                                green_bold.apply_to(if goal.is_test() { "PASS" } else { "Built" }),
                                label.to_string(),
                            );
                            pb.println(line);
                            current_targets.remove(&label);
                            pb.set_message(format!(
                                " {}",
                                current_targets
                                    .iter()
                                    .map(|l| l.to_string())
                                    .collect::<Vec<String>>()
                                    .join(", ")
                            ));
                            pb.inc(1);
                            target_count.insert(label);
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

                        BadBuildfile { buildfile, error } => {
                            errored = true;
                            error_count += 1;
                            let line = format!(
                                "{:>12} {} {}",
                                red_bold.apply_to("ERROR"),
                                "error when reading ",
                                buildfile.to_str().unwrap()
                            );
                            pb.println(line);
                            pb.println(format!("{}", error));
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

                        BuildError { label, error } => {
                            errored = true;
                            error_count += 1;
                            let line = format!(
                                "{:>12} {}",
                                red_bold.apply_to("ERROR"),
                                label.to_string(),
                            );
                            pb.println(line);
                            pb.println(format!("{}", error));
                        }

                        BuildStarted(t0) => {
                            build_started = t0;
                        }

                        EmptyWorkspace(_t1) => {
                            let line = format!(
                                "{:>12} nothing to be done in an empty workspace.",
                                blue_dim.apply_to("Prepare"),
                            );
                            pb.println(line);
                            return;
                        }

                        BuildCompleted(t1) if self.event_consumer.is_empty() => {
                            pb.set_message("");
                            let line = if targets.is_empty() {
                                format!(
                                    "{:>12} in {}ms ({} built, {} cached{})",
                                    green_bold.apply_to("Nothing done"),
                                    t1.saturating_duration_since(build_started).as_millis(),
                                    target_count.len(),
                                    cache_hits.len(),
                                    if error_count > 0 {
                                        format!(", {} errors", error_count)
                                    } else {
                                        "".into()
                                    }
                                )
                            } else if targets.len() == 1 {
                                format!(
                                    "{:>12} {} in {}ms ({} built, {} cached{})",
                                    if errored {
                                        red_bold.apply_to("Finished with errors")
                                    } else if self.goal.is_runnable() {
                                        green_bold.apply_to("Running")
                                    } else {
                                        green_bold.apply_to("Finished")
                                    },
                                    targets[0].to_string(),
                                    t1.saturating_duration_since(build_started).as_millis(),
                                    target_count.len(),
                                    cache_hits.len(),
                                    if error_count > 0 {
                                        format!(", {} errors", error_count)
                                    } else {
                                        "".into()
                                    }
                                )
                            } else {
                                format!(
                                    "{:>12} multiple goals in {}ms ({} targets, {} cached, {} errors): \n{}",
                                    if errored {
                                        red_bold.apply_to("Finished with errors")
                                    } else {
                                        green_bold.apply_to("Finished")
                                    },
                                    t1.saturating_duration_since(build_started).as_millis(),
                                    target_count.len(),
                                    cache_hits.len(),
                                    error_count,
                                    {
                                        let targets = targets.iter().map(|l| format!("{:>12} -> {}", "", l.to_string())).collect::<std::collections::HashSet<String>>();
                                        let mut targets = targets.into_iter().collect::<Vec<String>>();
                                        targets.sort();
                                        targets.join("\n")},
                                )
                            };
                            pb.println("");
                            pb.println(line);
                            break 'main_loop;
                        }

                        BuildCompleted(_) => self.event_channel.send(event),
                    }
                }
            }
        });
        while !handle.is_finished() {
            tokio::time::sleep(std::time::Duration::from_micros(10)).await;
        }
    }
}
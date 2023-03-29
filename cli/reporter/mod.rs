use crate::flags::Flags;
use dashmap::DashSet;
use indicatif::{ProgressBar, ProgressStyle};
use std::sync::Arc;
use tracing::debug;
use warp_core::events::event::*;
use warp_core::events::{EventChannel, EventConsumer};
use warp_core::{CacheStatus, Goal, Target};

trait Reporter {
    fn handle_event(&mut self, event: Event) {
        match event {
            Event::ActionEvent(e) => self.on_action_event(e),
            Event::ArchiveEvent(e) => self.on_archive_event(e),
            Event::Noop => (),
            Event::PackerEvent(e) => self.on_packer_event(e),
            Event::QueueEvent(e) => self.on_queue_event(e),
            Event::TricorderEvent(e) => self.on_tricorder_event(e),
            Event::WorkerEvent(e) => self.on_worker_event(e),
            Event::WorkflowEvent(e) => self.on_workflow_event(e),
        }
    }

    fn on_action_event(&mut self, _event: ActionEvent) {}
    fn on_archive_event(&mut self, _event: ArchiveEvent) {}
    fn on_packer_event(&mut self, _event: PackerEvent) {}
    fn on_queue_event(&mut self, _event: QueueEvent) {}
    fn on_tricorder_event(&mut self, _event: TricorderEvent) {}
    fn on_worker_event(&mut self, _event: WorkerEvent) {}
    fn on_workflow_event(&mut self, _event: WorkflowEvent) {}
}

pub struct StatusReporter {
    should_stop: bool,
    build_started: std::time::Instant,
    build_completed: std::time::Instant,
    event_consumer: EventConsumer,
    event_channel: Arc<EventChannel>,
    flags: Flags,
    goal: Goal,
    current_targets: DashSet<Target>,
    cache_hits: DashSet<Target>,
    queued_labels: DashSet<Target>,
    error_count: u32,
    errored: bool,
    target_count: DashSet<Target>,
    hashed_count: DashSet<Target>,
    pb: ProgressBar,
    targets: Vec<Target>,
}

impl StatusReporter {
    pub fn new(event_channel: Arc<EventChannel>, flags: Flags, goal: Goal) -> StatusReporter {
        let style = ProgressStyle::default_bar()
            .template("{prefix:>12.cyan.bold} [{bar:25}] {pos}/{len} {wide_msg}")
            .progress_chars("=> ");

        let pb = ProgressBar::new(0);
        pb.set_style(style);
        pb.set_prefix("Building");

        StatusReporter {
            targets: vec![],
            build_started: std::time::Instant::now(),
            build_completed: std::time::Instant::now(),
            event_consumer: event_channel.consumer(),
            event_channel,
            flags,
            goal,
            pb,
            error_count: 0,
            errored: false,
            target_count: DashSet::default(),
            hashed_count: DashSet::default(),
            current_targets: DashSet::default(),
            cache_hits: Default::default(),
            queued_labels: Default::default(),
            should_stop: false,
        }
    }
    pub async fn run(mut self, targets: &[Target]) {
        self.targets = targets.to_vec();
        let handle = std::thread::spawn(move || loop {
            self.event_consumer.fetch();
            if let Some(event) = self.event_consumer.pop() {
                debug!("{:#?}", event);
                self.handle_event(event)
            }
            if self.should_stop {
                break;
            }
        });
        while !handle.is_finished() {
            tokio::time::sleep(std::time::Duration::from_micros(10)).await;
        }
    }
}

impl Reporter for StatusReporter {
    fn on_archive_event(&mut self, event: ArchiveEvent) {
        match event {
            ArchiveEvent::CompressionStarted { total_files, .. } => {
                self.pb.set_length(self.pb.length() + (total_files as u64));
            }
            ArchiveEvent::CompressionProgress { .. } => self.pb.inc(1),
            _ => (),
        }
    }

    fn on_packer_event(&mut self, event: PackerEvent) {
        let yellow = console::Style::new().yellow();
        match event {
            PackerEvent::UploadStarted { url: _ } => {
                self.pb.set_length(self.pb.length() + 1);
                self.pb.set_prefix("Uploading");
            }
            PackerEvent::UploadCompleted { url } => {
                let line = format!("{:>12} {}", yellow.apply_to("Uploaded"), url);
                self.pb.println(line);
                self.pb.inc(1);
            }
            PackerEvent::UploadSkipped { url } => {
                let line = format!("{:>12} {} (SKIP)", yellow.apply_to("Uploaded"), url);
                self.pb.println(line);
                self.pb.inc(1);
            }
            PackerEvent::PackagingStarted { target } => {
                self.pb.set_length(self.pb.length() + 1);
                self.pb.set_prefix("Packing");
                self.current_targets.insert(target);
                self.pb.set_message(format!(
                    " {}",
                    self.current_targets
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ));
            }
            PackerEvent::PackagingCompleted { target } => {
                self.current_targets.remove(&target);
                let line = format!("{:>12} {}", yellow.apply_to("Packed"), target.to_string());
                self.pb.println(line);
                self.pb.inc(1);
            }
        }
    }

    fn on_queue_event(&mut self, event: QueueEvent) {
        match event {
            QueueEvent::TaskQueued { target, .. } => {
                if self.target_count.insert(target) {
                    self.pb.set_length(self.pb.length() + 1);
                }
            }
            _ => (),
        }
    }

    fn on_worker_event(&mut self, event: WorkerEvent) {
        let green_bold = console::Style::new().green().bold();
        let blue_dim = console::Style::new().blue();
        let _yellow = console::Style::new().yellow();
        let _red_bold = console::Style::new().red().bold();
        let _info = console::Style::new().on_blue().bright();

        match event {
            WorkerEvent::TargetBuildCompleted {
                cache_status,
                target,
                goal,
                signature,
            } if cache_status == CacheStatus::Cached => {
                self.current_targets.remove(&target);
                self.pb.set_message(format!(
                    " {}",
                    self.current_targets
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ));

                let line = format!(
                    "{:>12} {} {} {}",
                    if goal.is_test() {
                        green_bold.apply_to("PASS")
                    } else {
                        blue_dim.apply_to("Cache-hit")
                    },
                    target.to_string(),
                    if goal.is_test() {
                        signature
                    } else {
                        "".to_string()
                    },
                    if goal.is_test() { "(CACHED)" } else { "" }
                );
                self.pb.println(line);

                self.pb.inc(1);
                self.cache_hits.insert(target);
            }
            WorkerEvent::TargetBuildCompleted {
                target,
                goal,
                signature,
                ..
            } => {
                let line = format!(
                    "{:>12} {} {}",
                    green_bold.apply_to(if goal.is_test() { "PASS" } else { "Built" }),
                    target.to_string(),
                    if goal.is_test() {
                        signature
                    } else {
                        "".to_string()
                    },
                );
                self.pb.println(line);
                self.current_targets.remove(&target);
                self.pb.set_message(format!(
                    " {}",
                    self.current_targets
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ));
                self.pb.inc(1);
                self.target_count.insert(target);
            }
            _ => (),
        }
    }

    fn on_workflow_event(&mut self, event: WorkflowEvent) {
        let green_bold = console::Style::new().green().bold();
        let _purple = console::Style::new().magenta().bright();
        let _blue_dim = console::Style::new().blue();
        let _yellow = console::Style::new().yellow();
        let red_bold = console::Style::new().red().bold();
        let _info = console::Style::new().on_blue().bright();

        match event {
            WorkflowEvent::BuildStarted(t0) => {
                self.build_started = t0;
            }
            WorkflowEvent::BuildCompleted(t1) if self.event_consumer.is_empty() => {
                self.build_completed = t1;
            }
            WorkflowEvent::BuildCompleted(_) => self.event_channel.send(event),

            WorkflowEvent::Shutdown => {
                let t1 = std::time::Instant::now();
                self.pb.set_message("");
                let line = if self.targets.is_empty() {
                    format!(
                        "{:>12} in {}ms ({} built, {} cached{})",
                        green_bold.apply_to("Nothing done"),
                        t1.saturating_duration_since(self.build_started).as_millis(),
                        self.target_count.len(),
                        self.cache_hits.len(),
                        if self.error_count > 0 {
                            format!(", {} errors", self.error_count)
                        } else {
                            "".into()
                        }
                    )
                } else if self.targets.len() == 1 {
                    format!(
                        "{:>12} {} in {}ms ({} built, {} cached{})",
                        if self.errored {
                            red_bold.apply_to("Finished with errors")
                        } else if self.goal.is_runnable() {
                            green_bold.apply_to("Running")
                        } else {
                            green_bold.apply_to("Finished")
                        },
                        self.targets[0].to_string(),
                        t1.saturating_duration_since(self.build_started).as_millis(),
                        self.target_count.len(),
                        self.cache_hits.len(),
                        if self.error_count > 0 {
                            format!(", {} errors", self.error_count)
                        } else {
                            "".into()
                        }
                    )
                } else {
                    format!(
                        "{:>12} multiple goals in {}ms ({} targets, {} cached, {} errors): \n{}",
                        if self.errored {
                            red_bold.apply_to("Finished with errors")
                        } else {
                            green_bold.apply_to("Finished")
                        },
                        t1.saturating_duration_since(self.build_started).as_millis(),
                        self.target_count.len(),
                        self.cache_hits.len(),
                        self.error_count,
                        {
                            let targets = self
                                .targets
                                .iter()
                                .map(|l| format!("{:>12} -> {}", "", l.to_string()))
                                .collect::<std::collections::HashSet<String>>();
                            let mut targets = targets.into_iter().collect::<Vec<String>>();
                            targets.sort();
                            targets.join("\n")
                        },
                    )
                };
                self.pb.println("");
                self.pb.println(line);
                self.should_stop = true;
            }
        }
    }

    fn on_tricorder_event(&mut self, event: TricorderEvent) {
        let purple = console::Style::new().magenta().bright();
        match event {
            TricorderEvent::TricorderServiceStarted { tricorder_url } => {
                let line = format!("{:>12} {}", purple.apply_to("Started"), tricorder_url);
                self.pb.println(line);
                self.pb.inc(1);
            }
            TricorderEvent::TricorderReadyingStarted { tricorder_url } => {
                let line = format!("{:>12} {}", purple.apply_to("Readying"), tricorder_url);
                self.pb.println(line);
                self.pb.inc(1);
            }
            TricorderEvent::TricorderReadyingCompleted { tricorder_url } => {
                let line = format!("{:>12} {}", purple.apply_to("Ready"), tricorder_url);
                self.pb.println(line);
                self.pb.inc(1);
            }
            TricorderEvent::TricorderConnectionEstablished { tricorder_url: _ } => (),
            TricorderEvent::SignatureGenerationStarted { target: _ } => (),
        }
    }
}

use crate::commands::Flags;
use indicatif::ProgressBar;
use std::sync::Arc;
use tracing::*;
use warp_core::*;

pub struct DownloadReporter {
    event_consumer: EventConsumer,
    event_channel: Arc<EventChannel>,
    flags: Flags,
    goal: Goal,
}

impl DownloadReporter {
    pub fn new(event_channel: Arc<EventChannel>, flags: Flags, goal: Goal) -> DownloadReporter {
        DownloadReporter {
            event_consumer: event_channel.consumer(),
            event_channel,
            flags,
            goal,
        }
    }
    pub async fn run(self, targets: &[Label]) {
        let handle = std::thread::spawn(move || {
            let yellow = console::Style::new().yellow();
            let red_bold = console::Style::new().red().bold();

            let pb = ProgressBar::new(100);

            'main_loop: loop {
                self.event_consumer.fetch();
                for event in &self.event_consumer {
                    debug!("{:#?}", event);

                    use warp_core::Event::*;
                    match event {
                        ResolvingDependency { label, resolver } => {
                            let line = format!(
                                "{:>12} {} using {}",
                                yellow.apply_to("Resolving"),
                                label.to_string(),
                                resolver.to_string(),
                            );
                            pb.println(line);
                        }

                        ArchiveVerifying(label) => {
                            let line = format!(
                                "{:>12} {}",
                                yellow.apply_to("Verifying"),
                                label.to_string(),
                            );
                            pb.println(line);
                        }

                        ArchiveUnpacking(label) => {
                            let line = format!(
                                "{:>12} {}",
                                yellow.apply_to("Unpacking"),
                                label.to_string(),
                            );
                            pb.println(line);
                        }

                        ArchiveDownloading { label, .. } => {
                            let line = format!(
                                "{:>12} {}",
                                yellow.apply_to("Downloading"),
                                label.to_string(),
                            );
                            pb.println(line);
                        }

                        ErrorLoadingRule(name, err) => {
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
                            let line = format!(
                                "{:>12} {}",
                                red_bold.apply_to("ERROR"),
                                "something went wrong with a worker"
                            );
                            pb.println(line);
                            pb.println(format!("{}", err));
                        }

                        BuildCompleted(_t1) if self.event_consumer.is_empty() => {
                            break 'main_loop;
                        }

                        BuildCompleted(_) => self.event_channel.send(event),
                        _ => (),
                    }
                }
            }
        });
        while !handle.is_finished() {
            tokio::time::sleep(std::time::Duration::from_micros(10)).await;
        }
    }
}

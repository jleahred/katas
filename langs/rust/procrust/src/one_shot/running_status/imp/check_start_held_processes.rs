use chrono::Local;
use std::process::Command;
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

use super::super::{ProcessStatus, RunningStatus};

pub(crate) fn check_start_held_processes(mut rs: RunningStatus) -> RunningStatus {
    for (id, process) in rs.processes.iter_mut() {
        if let ProcessStatus::PendingHealthStartCheck {
            pid,
            start_health_check,
            retries,
            last_attempt,
        } = process.status.clone()
        {
            if retries > 0 && last_attempt + Duration::from_secs(20) > Local::now() {
                println!(
                    "Skipping start health check for process {}. Last attempt was at {}",
                    id.0, last_attempt
                );
                continue;
            }

            match start_health_check {
                Some(ref cmd) => {
                    println!("cheking start health for process {}", id.0);
                    match run_command_with_timeout("todo: 0") {
                        Ok(()) => {
                            println!("Health check succeeded for process {}", id.0);
                            process.status = ProcessStatus::Running { pid };
                        }
                        Err(err) => {
                            eprintln!(
                                "Health check failed for process {}: {}. Retries: {}",
                                id.0, err, retries
                            );
                            eprintln!("Program process restart {} ", id.0);
                            if retries > 10 {
                                process.status = ProcessStatus::ScheduledStop { pid };
                                process.applied_on = Local::now().naive_local();
                                process.status = ProcessStatus::Stopping {
                                    pid,
                                    retries: 0,
                                    last_attempt: Local::now(),
                                };
                            } else {
                                process.status = ProcessStatus::PendingHealthStartCheck {
                                    pid,
                                    start_health_check,
                                    retries: retries + 1,
                                    last_attempt: Local::now(),
                                };
                            }
                        }
                    }
                }
                None => {
                    println!(
                        "No start health check command provided for process {}",
                        id.0
                    );
                    process.status = ProcessStatus::Running { pid };
                }
            }
        }
    }
    rs
}

fn run_command_with_timeout(command: &str) -> Result<(), String> {
    let (sender, receiver) = mpsc::channel();

    let command = command.to_string();

    thread::spawn(move || {
        let output = Command::new("sh").arg("-c").arg(command).output();

        let _ = sender.send(output);
    });

    let rec = receiver.recv_timeout(Duration::from_secs(2));
    match rec {
        Ok(Ok(output)) => {
            if output.status.success() {
                Ok(())
            } else {
                Err(format!("Command failed with status: {}", output.status))
            }
        }
        Ok(Err(e)) => Err(format!("Command execution failed: {}", e)),
        Err(_) => Err("Command timed out".to_string()),
    }
}

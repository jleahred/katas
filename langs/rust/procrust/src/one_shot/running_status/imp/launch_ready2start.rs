use crate::one_shot::running_status::{ProcessStatus, ProcessWatched, RunningStatus};
use crate::types::config::{self, ProcessId};
use chrono::NaiveDateTime;
use std::{
    io,
    process::{Child, Command},
    thread,
    time::Duration,
};

pub(crate) fn launch_ready2start(mut running_status: RunningStatus) -> RunningStatus {
    let processes: Vec<_> = running_status.processes.values().cloned().collect();
    for process in processes {
        match process.status {
            ProcessStatus::Ready2Start {
                command,
                process_id,
                apply_on,
            } => {
                println!(
                    "Running process {}    apply_on: {}",
                    process.id.0, process.apply_on
                );
                match run_process(&command, &process_id, &apply_on) {
                    Ok(pid) => {
                        println!(
                            "Launched process {} with PID: {}   apply_on: {}",
                            process.id.0, pid, process.apply_on
                        );

                        // update `RunningStatus`
                        running_status.processes.insert(
                            process.id.clone(),
                            ProcessWatched {
                                id: process.id.clone(),
                                apply_on: process.apply_on,
                                procrust_uid: command.0.clone(), // TODO: Mejorar con un UUID único
                                status: ProcessStatus::PendingHealthStartCheck {
                                    pid,
                                    retries: 0,
                                    last_attempt: chrono::Local::now(),
                                },
                                applied_on: chrono::Local::now().naive_utc(),
                            },
                        );
                    }
                    Err(e) => eprintln!("Failed to launch process {}: {}", process.id.0, e),
                }
            }
            _ => continue,
        }
    }

    running_status
}

fn run_process(
    command: &config::Command,
    _process_id: &ProcessId,
    _apply_on: &NaiveDateTime,
) -> Result<u32, io::Error> {
    let child: Child = Command::new("sh")
        .arg("-c")
        .arg(&command.0)
        .env("PROCRUST", &command.0)
        .spawn()?;
    //  todo: convendría desconectar la salida de error y la salida estándar para evitar zombis?

    thread::sleep(Duration::from_secs(1));

    // match child.try_wait()? {
    //     Some(status) => {
    //         if status.success() {
    //             eprintln!(
    //                 "Running process finished successfully: {} (apply_on: {})   command: {}",
    //                 process_id.0, apply_on, command.0
    //             );
    //         } else {
    //             eprintln!(
    //                 "Running process finished with error: {} (apply_on: {})   command: {}",
    //                 process_id.0, apply_on, command.0
    //             );
    //         }
    //     }
    //     None => {}
    // }

    Ok(child.id())
}

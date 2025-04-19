use crate::one_shot::running_status::{ProcessStatus, ProcessWatched, RunningStatus};
use crate::types::config::{ProcessConfig, ProcessId};
use chrono::NaiveDateTime;
use std::{
    fs::{self, File},
    io::{self, Write},
    path::Path,
    process::{Child, Command},
    thread,
    time::Duration,
};

pub(crate) fn launch_missing_processes(
    mut running_status: RunningStatus,
    proccesses_conf_pending2run: &Vec<ProcessConfig>,
) -> RunningStatus {
    for process in proccesses_conf_pending2run {
        match run_process(process) {
            Ok(pid) => {
                println!(
                    "Launched process {} with PID: {}   apply_on: {}",
                    process.id.0, pid, process.apply_on
                );

                // Actualizar el estado del proceso en `RunningStatus`
                running_status.processes.insert(
                    process.id.clone(),
                    ProcessWatched {
                        id: process.id.clone(),
                        pid,
                        apply_on: process.apply_on,
                        procrust_uid: process.command.clone(), // TODO: Mejorar con un UUID único
                        status: ProcessStatus::Running,
                        applied_on: chrono::Local::now().naive_utc(),
                    },
                );
            }
            Err(e) => eprintln!("Failed to launch process {}: {}", process.id.0, e),
        }
    }

    running_status
}

fn run_process(process: &ProcessConfig) -> Result<u32, io::Error> {
    let mut child: Child = Command::new("sh")
        .arg("-c")
        .arg(&process.command)
        .env("PROCRUST", &process.command)
        .spawn()?;
    //  todo: convendría desconectar la salida de error y la salida estándar para evitar zombis?

    thread::sleep(Duration::from_secs(1));

    match child.try_wait()? {
        Some(status) => {
            if status.success() {
                eprintln!(
                    "Running process finished successfully: {} (apply_on: {})",
                    process.id.0, process.apply_on
                );
            } else {
                eprintln!(
                    "Running process finished with error: {} (apply_on: {})",
                    process.id.0, process.apply_on
                );
            }
        }
        None => {}
    }

    Ok(child.id())
}

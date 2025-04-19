use crate::types::process_watched::*;
use std::fs;
use std::io::Write;
use std::path::Path;
use toml;

pub(crate) fn watched_update2stopping(
    path_persist_watched: &str,
    watched_processes: &[ProcessWatched],
) {
    for process in watched_processes {
        let updated_process = match process.status {
            ProcessStatus::Stopping { .. } => {
                continue;
            }
            ProcessStatus::ScheduledStop => {
                continue;
            }
            ProcessStatus::Running => ProcessWatched {
                id: process.id.clone(),
                pid: process.pid,
                apply_on: process.apply_on,
                procrust_uid: process.procrust_uid.clone(),
                status: ProcessStatus::Stopping {
                    retries: 0,
                    last_attempt: chrono::Local::now(),
                },
                applied_on: process.applied_on,
            },
        };

        let file_path = format!("{}/{}.toml", path_persist_watched, process.id);

        if let Ok(toml) = toml::to_string(&updated_process) {
            if let Ok(mut file) = fs::File::create(Path::new(&file_path)) {
                if let Err(e) = file.write_all(toml.as_bytes()) {
                    eprintln!("Failed to update file {}: {}", file_path, e);
                } else {
                    println!("Updated process {} to ScheduleStop", process.id);
                }
            }
        }
    }
}

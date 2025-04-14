use crate::process_watcher::{ProcessStatus, ProcessWatched};
use std::fs;
use std::io::Write;
use std::path::Path;
use toml;

pub fn update_to_stopping(watched_processes: &[ProcessWatched]) {
    for process in watched_processes {
        if let ProcessStatus::Stopping { .. } = process.status {
            // If already in Stopping status, skip
            continue;
        }
        if ProcessStatus::ScheduledStop == process.status {
            // If already in ScheduledStop status, skip
            continue;
        }

        let file_path = format!("/tmp/procrust/{}.toml", process.id);
        let updated_process = ProcessWatched {
            id: process.id.clone(),
            pid: process.pid,
            apply_on: process.apply_on,
            procrust_uid: process.procrust_uid.clone(),
            status: ProcessStatus::ScheduledStop,
            applied_on: process.applied_on,
        };

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

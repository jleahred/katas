use crate::process_checker::is_process_running;
// use crate::process_cmd::get_cmd_by_pid;
use crate::process_cookie;
use crate::process_watcher::ProcessWatched;
use std::fs;

pub fn clean_stale_watched_files(watched_processes: &[ProcessWatched]) {
    for process in watched_processes {
        let file_path = format!("/tmp/procrust/{}.toml", process.id);
        let remove_file: bool = if !is_process_running(process.pid) {
            println!("Removing stale file: {} missing pid", file_path);
            true
        } else {
            match process_cookie::get_process_env_var(process.pid, "SUPRUST") {
                Ok(puid) => match puid {
                    None => {
                        eprintln!(
                            "Removing file: Empty procrust UID for PID {}: {}",
                            process.pid, file_path
                        );
                        true
                    }
                    Some(puid) => {
                        if puid != process.procrust_uid {
                            eprintln!(
                                "Removing file: Different procrust UID for PID {}: {} -> {}",
                                process.pid, puid, process.procrust_uid
                            );
                            true
                        } else {
                            false
                        }
                    }
                },
                Err(e) => {
                    eprintln!(
                        "Removing file: Failed to get command by PID {}: {}",
                        process.pid, e
                    );
                    true
                }
            }
        };

        if remove_file {
            if let Err(e) = fs::remove_file(&file_path) {
                eprintln!("Failed to remove file {}: {}", file_path, e);
            } else {
                println!("Removed stale file: {}", file_path);
            }
        }
    }
}

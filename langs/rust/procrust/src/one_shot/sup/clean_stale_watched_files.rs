use crate::types::process_watched::ProcessWatched;
use std::fs;
use std::io::{self};

pub fn clean_stale_watched_files(path_persist_watched: &str, watched_processes: &[ProcessWatched]) {
    for process in watched_processes {
        let file_path = format!("{}/{}.toml", path_persist_watched, process.id);
        let remove_file: bool = if !is_process_running(process.pid) {
            println!("Removing stale file: {} missing pid", file_path);
            true
        } else {
            match get_process_env_var(process.pid, "PROCRUST") {
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

fn is_process_running(pid: u32) -> bool {
    let path = format!("/proc/{}", pid);
    fs::metadata(path).is_ok()
}

fn get_process_env_var(pid: u32, var_name: &str) -> Result<Option<String>, io::Error> {
    // Build the path to the environ file
    let environ_path = format!("/proc/{}/environ", pid);

    // Read the content of the file
    let content = fs::read(environ_path)?;

    // Environment variables are separated by null characters
    for var in content.split(|&b| b == 0) {
        // Convert to String, ignoring invalid UTF-8 bytes
        let Ok(env_var) = String::from_utf8_lossy(var).into_owned().parse::<String>();
        // Look for the format NAME=VALUE
        if let Some(pos) = env_var.find('=') {
            let (name, value) = env_var.split_at(pos);
            if name == var_name {
                // Remove the '=' character at the start of the value
                return Ok(Some(value[1..].to_string()));
            }
        }
    }

    // Variable not found
    Ok(None)
}

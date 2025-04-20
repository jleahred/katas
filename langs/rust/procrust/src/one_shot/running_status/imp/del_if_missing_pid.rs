use super::super::{ProcessStatus, RunningStatus};
use std::fs;
use std::io::{self};

pub(crate) fn del_if_missing_pid(mut rs: RunningStatus) -> RunningStatus {
    // Iterate over the processes in the running status
    let mut to_remove = Vec::new();

    for (id, process) in rs.processes.iter_mut() {
        let opid = match process.status {
            ProcessStatus::ScheduledStop { pid } => Some(pid),
            ProcessStatus::Stopping { pid, .. } => Some(pid),
            ProcessStatus::Running { pid } => Some(pid),

            ProcessStatus::PendingHealthStartCheck { .. } | ProcessStatus::Ready2Start { .. } => {
                None
            }
        };

        if let Some(pid) = opid {
            // Check if the process is running
            if !is_process_running(pid) {
                // If not running, mark the process for removal
                println!("Removing process {} with PID {}: Not running", id.0, pid);
                to_remove.push(id.clone());
            } else {
                // Check if the process is still being watched but procrust_uid is different
                let remove = match get_process_env_var(pid, "PROCRUST") {
                    Ok(Some(puid)) => {
                        if puid != process.procrust_uid {
                            eprintln!(
                                "Removing watched: Different procrust UID for PID {}: {} -> {}",
                                pid, puid, process.procrust_uid
                            );
                            true
                        } else {
                            // println!(
                            //     "Keeping process {} with PID {}: Still running",
                            //     id.0, process.pid
                            // );
                            false
                        }
                    }
                    Ok(None) => {
                        eprintln!("Removing watched: missing procrust_uid by PID {}", pid);
                        true
                    }
                    Err(e) => {
                        eprintln!(
                            "Removing watched: Failed to procrust_uid by PID {}: {}",
                            pid, e
                        );
                        true
                    }
                };
                if remove {
                    to_remove.push(id.clone());
                }
            }
        }
    }

    for id in to_remove {
        rs.processes.remove(&id);
    }
    rs
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

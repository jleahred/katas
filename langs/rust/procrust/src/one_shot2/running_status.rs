pub(crate) mod imp;

use crate::types::config::{ProcessConfig, ProcessId};
use chrono::NaiveDateTime;
use nix::libc::remove;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::io::{self};

pub(crate) use imp::load_running_status::load_running_status;

#[derive(Deserialize, Serialize, Debug)]
pub(crate) struct RunningStatus {
    pub(crate) uid: String,
    #[serde(rename = "file_format")]
    pub(crate) _file_format: String,
    pub(crate) processes: HashMap<ProcessId, ProcessWatched>,
}

impl RunningStatus {
    pub(crate) fn save(self, file_path: &str) {
        imp::save(&self, file_path);
    }
    pub(crate) fn send_kill_on_stopping_processes(self) -> Self {
        imp::send_kill_on_stopping_processes(self)
    }

    pub(crate) fn mark_stopping(self, conf_proc_active: &[ProcessConfig]) -> Self {
        mark_stopping(self, conf_proc_active)
    }

    pub(crate) fn del_if_missing_pid(self) -> Self {
        del_if_missing_pid(self)
    }

    pub(crate) fn launch_missing_processes(mut self, proc_conf: &Vec<ProcessConfig>) -> Self {
        imp::launch_missing_processes(self, proc_conf)
    }

    pub(crate) fn get_running_ids(&self) -> Vec<ProcessId> {
        imp::get_running_ids(self)
    }

    pub(crate) fn debug(self) -> Self {
        dbg!(&self);
        self
    }

    pub(crate) fn inspect<F>(self, func: F) -> Self
    where
        F: Fn(&Self),
    {
        func(&self);
        self
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct ProcessWatched {
    pub(crate) id: ProcessId,
    pub(crate) pid: u32,
    pub(crate) procrust_uid: String,
    pub(crate) apply_on: NaiveDateTime,
    pub(crate) status: ProcessStatus,
    pub(crate) applied_on: NaiveDateTime,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub(crate) enum ProcessStatus {
    Running,
    Stopping {
        retries: u32,
        last_attempt: chrono::DateTime<chrono::Local>,
    },
    ScheduledStop,
}

pub(crate) fn mark_stopping(
    mut rs: RunningStatus,
    conf_proc_active: &[ProcessConfig],
) -> RunningStatus {
    let active_ids: Vec<ProcessId> = conf_proc_active
        .iter()
        .map(|conf| conf.id.clone())
        .collect();

    for (id, process) in rs.processes.iter_mut() {
        if !active_ids.contains(id) && process.status == ProcessStatus::Running {
            println!(
                "Marking process {} with PID {} as stopping",
                id.0, process.pid
            );
            process.status = ProcessStatus::Stopping {
                retries: 0,
                last_attempt: chrono::Local::now(),
            };
        }
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

pub(crate) fn del_if_missing_pid(mut rs: RunningStatus) -> RunningStatus {
    // Iterate over the processes in the running status
    let mut to_remove = Vec::new();

    for (id, process) in rs.processes.iter_mut() {
        // Check if the process is running
        if !is_process_running(process.pid) {
            // If not running, mark the process for removal
            println!(
                "Removing process {} with PID {}: Not running",
                id.0, process.pid
            );
            to_remove.push(id.clone());
        } else {
            // Check if the process is still being watched but procrust_uid is different
            let remove = match get_process_env_var(process.pid, "PROCRUST") {
                Ok(Some(puid)) => {
                    if puid != process.procrust_uid {
                        eprintln!(
                            "Removing watched: Different procrust UID for PID {}: {} -> {}",
                            process.pid, puid, process.procrust_uid
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
                    eprintln!(
                        "Removing watched: missing procrust_uid by PID {}",
                        process.pid
                    );
                    true
                }
                Err(e) => {
                    eprintln!(
                        "Removing watched: Failed to procrust_uid by PID {}: {}",
                        process.pid, e
                    );
                    true
                }
            };
            if remove {
                to_remove.push(id.clone());
            }
        }
    }

    for id in to_remove {
        rs.processes.remove(&id);
    }
    rs
}

mod imp;

use crate::read_config_file::read_config_file;
use crate::types::config::Config;
use crate::types::config::RunningStatus;
use std::fs;
use std::fs::{self};
use std::path::Path;

pub fn one_shot2() {
    println!("\n--------------------------------------------------------------------------------");
    println!("Checking... {}\n", chrono::Local::now());

    let path_persist_running_status = format!("/tmp/procrust/{}/", config.uid);
    let running_status = load_running_status(path_persist_running_status);
}

pub fn load_running_status<P: AsRef<Path>>(file_path: P) -> RunningStatus {
    let content = fs::read_to_string(file_path.as_ref()).unwrap_or_else(|err| {
        panic!(
            "Failed to read file {}: {}",
            file_path.as_ref().display(),
            err
        )
    });
    toml::from_str(&content).unwrap_or_else(|err| {
        panic!(
            "Failed to parse TOML from file {}: {}",
            file_path.as_ref().display(),
            err
        )
    })
}

use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Deserialize, Debug)]
pub struct RunningStatus {
    pub uid: String,
    #[serde(rename = "file_format")]
    pub _file_format: String,
    pub processes: HashMap<ProcessId, ProcessWatched>,
}

#[derive(Deserialize, PartialEq, Eq, Hash, Debug)]
pub struct ProcessId(pub String);

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ProcessWatched {
    pub id: String,
    pub pid: u32,
    pub procrust_uid: String,
    pub apply_on: NaiveDateTime,
    pub status: ProcessStatus,
    pub applied_on: NaiveDateTime,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub enum ProcessStatus {
    Running,
    Stopping {
        retries: u32,
        last_attempt: chrono::DateTime<chrono::Local>,
    },
    ScheduledStop,
}

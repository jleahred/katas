pub(crate) mod imp;

use crate::types::config::ProcessConfig;
use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

pub(crate) use imp::load_running_status::load_running_status;

#[derive(Deserialize, Serialize, Debug)]
pub struct RunningStatus {
    pub uid: String,
    #[serde(rename = "file_format")]
    pub _file_format: String,
    pub processes: HashMap<ProcessId, ProcessWatched>,
}

impl RunningStatus {
    pub fn save(self, file_path: &str) {
        imp::save(&self, file_path);
    }
    pub fn send_kill_on_stopping_processes(self) -> Self {
        imp::send_kill_on_stopping_processes(self)
    }

    pub fn mark_running_as_stop_by_config(mut self) -> Self {
        // todo
        self
    }

    pub fn del_if_missing_pid(mut self) -> Self {
        // todo
        self
    }
    pub fn launch_missing_processes(mut self, proc_conf: &Vec<ProcessConfig>) -> Self {
        imp::launch_missing_processes(self, proc_conf)
    }
}

#[derive(Deserialize, Serialize, PartialEq, Eq, Hash, Clone, Debug)]
pub struct ProcessId(pub String);

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ProcessWatched {
    pub id: ProcessId,
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

pub(crate) mod imp;

use crate::types::config::ProcessConfig;
use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

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

    pub(crate) fn mark_running_as_stop_by_config(mut self) -> Self {
        // todo
        self
    }

    pub(crate) fn del_if_missing_pid(mut self) -> Self {
        // todo
        self
    }
    pub(crate) fn launch_missing_processes(mut self, proc_conf: &Vec<ProcessConfig>) -> Self {
        imp::launch_missing_processes(self, proc_conf)
    }
}

#[derive(Deserialize, Serialize, PartialEq, Eq, Hash, Clone, Debug)]
pub(crate) struct ProcessId(pub(crate) String);

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

pub(crate) mod imp;

use crate::types::config::{ProcessConfig, ProcessId};
use chrono::NaiveDateTime;
pub(crate) use imp::load_running_status::load_running_status;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

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
        imp::mark_stopping(self, conf_proc_active)
    }

    pub(crate) fn del_if_missing_pid(self) -> Self {
        imp::del_if_missing_pid(self)
    }

    pub(crate) fn launch_missing_processes(self, proc_conf: &Vec<ProcessConfig>) -> Self {
        imp::launch_missing_processes(self, proc_conf)
    }

    pub(crate) fn get_running_ids(&self) -> Vec<ProcessId> {
        imp::get_running_ids(self)
    }

    // pub(crate) fn debug(self) -> Self {
    //     dbg!(&self);
    //     self
    // }

    // pub(crate) fn inspect<F>(self, func: F) -> Self
    // where
    //     F: Fn(&Self),
    // {
    //     func(&self);
    //     self
    // }
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

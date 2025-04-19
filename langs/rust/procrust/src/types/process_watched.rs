use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct ProcessWatched {
    pub(crate) id: String,
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

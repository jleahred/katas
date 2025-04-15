use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};

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

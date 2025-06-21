use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Task {
    pub name: String,
    pub scheduled: chrono::NaiveDateTime,
    pub executed: Option<chrono::NaiveDateTime>,
    #[serde(default = "default_status", skip_serializing_if = "is_pending")]
    pub status: Status,
    pub commands: String,
    pub daily: Option<bool>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
pub enum Status {
    Pending,
    Completed,
    Failed,
}

fn default_status() -> Status {
    Status::Pending
}

fn is_pending(status: &Status) -> bool {
    *status == Status::Pending
}

use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ProcessConfig {
    pub id: String,
    pub command: String,
    pub apply_on: Option<NaiveDateTime>,
    // pub apply_on: chrono::DateTime<chrono::Local>,
}

mod imp;

use chrono::{NaiveDateTime, NaiveTime};
use serde::{Deserialize, Serialize};

#[cfg(test)]
mod tests;

#[derive(Deserialize)]
pub struct Config {
    pub uid: String,
    #[serde(rename = "file_format")]
    pub _file_format: String,
    pub process: Option<Vec<ProcessConfig>>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ProcessConfig {
    pub id: String,
    pub command: String,
    pub apply_on: NaiveDateTime,

    #[serde(default)]
    pub week_days: Option<DaySelection>,

    #[serde(default)]
    pub start_time: Option<NaiveTime>,

    #[serde(default)]
    pub stop_time: Option<NaiveTime>,

    #[serde(default, rename = "type")]
    pub process_type: Option<ProcessType>,

    #[serde(default)]
    pub depends_on: Option<String>,
}

#[derive(Debug, Clone)]
pub struct ProcessWatched {
    pub id: String,
    pub status: String, // Example: "running", "stopped", etc.
}

pub struct ActiveByDateProcConf(pub Vec<ProcessConfig>);

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum ProcessType {
    Normal,
    Fake,
}

impl ProcessConfig {
    pub fn check_config(&self) -> Result<(), String> {
        if self.command.is_empty() {
            return Err("Command cannot be empty".to_string());
        }
        imp::is_valid_start_stop(self)
    }
}

//  ----------------

#[derive(Debug, Clone, Serialize)]
pub enum DaySelection {
    Days(Vec<chrono::Weekday>),
    #[serde(rename = "mon-fri")]
    Mon2Fri,
    #[serde(rename = "all")]
    All,
}

impl DaySelection {
    pub fn matches(&self, weekday: chrono::Weekday) -> bool {
        imp::matches(self, weekday)
    }
}

impl<'de> Deserialize<'de> for DaySelection {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        imp::deserialize_day_selection(deserializer)
    }
}

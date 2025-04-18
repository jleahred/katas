mod imp;

use chrono::{NaiveDateTime, NaiveTime};
use serde::{Deserialize, Serialize};

#[cfg(test)]
mod tests;

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
pub struct Config {
    pub uid: String,
    #[serde(rename = "file_format")]
    pub _file_format: String,
    pub process: Vec<ProcessConfig>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(deny_unknown_fields)]
pub struct ProcessConfig {
    pub id: String,
    pub command: String,
    pub apply_on: NaiveDateTime,

    #[serde(default)]
    pub schedule: Option<Schedule>,

    #[serde(default, rename = "type")]
    pub process_type: ProcessType,

    #[serde(default)]
    pub depends_on: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(deny_unknown_fields)]
pub struct Schedule {
    #[serde(default)]
    pub week_days: DaySelection,

    pub start_time: NaiveTime,
    pub stop_time: NaiveTime,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(deny_unknown_fields)]
#[serde(rename_all = "lowercase")]
pub enum ProcessType {
    Normal,
    Fake,
}

impl Default for ProcessType {
    fn default() -> Self {
        ProcessType::Normal
    }
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

impl Default for DaySelection {
    fn default() -> Self {
        DaySelection::All
    }
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

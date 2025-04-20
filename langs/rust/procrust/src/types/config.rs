mod imp;

use chrono::{Datelike, Local, NaiveDateTime, NaiveTime};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[cfg(test)]
mod tests;

#[derive(Deserialize, Serialize, PartialEq, Eq, Hash, Clone, Debug)]
pub(crate) struct ProcessId(pub(crate) String);

#[derive(Deserialize, Serialize, PartialEq, Eq, Hash, Clone, Debug)]
pub(crate) struct ConfigUid(pub(crate) String);

#[derive(Deserialize, Serialize, PartialEq, Eq, Hash, Clone, Debug)]
pub(crate) struct Command(pub(crate) String);

#[derive(Deserialize, Serialize, PartialEq, Eq, Hash, Clone, Debug)]
pub(crate) struct CommandCheckHealth(pub(crate) String);

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
pub(crate) struct Config {
    pub(crate) uid: ConfigUid,
    #[serde(rename = "file_format")]
    pub(crate) _file_format: String,
    pub(crate) process: Vec<ProcessConfig>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(deny_unknown_fields)]
pub(crate) struct ProcessConfig {
    pub(crate) id: ProcessId,
    pub(crate) command: Command,
    pub(crate) start_health_check: Option<CommandCheckHealth>,
    pub(crate) apply_on: NaiveDateTime,

    #[serde(default)]
    pub(crate) schedule: Option<Schedule>,

    #[serde(default, rename = "type")]
    pub(crate) process_type: ProcessType,

    #[serde(default)]
    pub(crate) depends_on: Vec<ProcessId>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(deny_unknown_fields)]
pub(crate) struct Schedule {
    #[serde(default)]
    pub(crate) week_days: DaySelection,

    pub(crate) start_time: NaiveTime,
    pub(crate) stop_time: NaiveTime,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(deny_unknown_fields)]
#[serde(rename_all = "lowercase")]
pub(crate) enum ProcessType {
    Normal,
    Fake,
}

impl Config {
    pub(crate) fn check_config(&self) -> Result<(), String> {
        if self.uid.0.is_empty() {
            return Err("UID cannot be empty".to_string());
        }
        if self.process.is_empty() {
            return Err("Process list cannot be empty".to_string());
        }
        for process in &self.process {
            process.check_config()?;
        }
        Ok(())
    }

    pub(crate) fn get_active_procs_by_config(&self) -> Vec<ProcessConfig> {
        imp::get_active_procs_by_config(&self)
    }
}

impl Default for ProcessType {
    fn default() -> Self {
        ProcessType::Normal
    }
}

impl ProcessConfig {
    pub(crate) fn check_config(&self) -> Result<(), String> {
        if self.command.0.is_empty() {
            return Err("Command cannot be empty".to_string());
        }
        imp::is_valid_start_stop(self)
    }
}

//  ----------------

#[derive(Debug, Clone, Serialize)]
pub(crate) enum DaySelection {
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
    pub(crate) fn matches(&self, weekday: chrono::Weekday) -> bool {
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

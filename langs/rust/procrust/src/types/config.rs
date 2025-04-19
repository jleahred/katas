mod imp;

use chrono::{Datelike, Local, NaiveDateTime, NaiveTime, Timelike};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[cfg(test)]
mod tests;

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
pub(crate) struct Config {
    pub(crate) uid: String,
    #[serde(rename = "file_format")]
    pub(crate) _file_format: String,
    pub(crate) process: Vec<ProcessConfig>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(deny_unknown_fields)]
pub(crate) struct ProcessConfig {
    pub(crate) id: String,
    pub(crate) command: String,
    pub(crate) apply_on: NaiveDateTime,

    #[serde(default)]
    pub(crate) schedule: Option<Schedule>,

    #[serde(default, rename = "type")]
    pub(crate) process_type: ProcessType,

    #[serde(default)]
    pub(crate) depends_on: Vec<String>,
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
        if self.uid.is_empty() {
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
        let now = Local::now().naive_local();
        let mut process_map: HashMap<String, ProcessConfig> = HashMap::new();

        for process in &self.process {
            // Ignorar procesos futuros
            if process.apply_on > now {
                continue;
            }

            // Verificar si el proceso tiene un schedule y si aplica
            if let Some(schedule) = &process.schedule {
                let weekday = now.weekday();
                let time = now.time();

                if !schedule.week_days.matches(weekday) {
                    continue;
                }

                if time < schedule.start_time || time >= schedule.stop_time {
                    continue;
                }
            }

            // Mantener el proceso más reciente según apply_on
            let entry = process_map
                .entry(process.id.clone())
                .or_insert_with(|| process.clone());

            if entry.apply_on < process.apply_on {
                *entry = process.clone();
            }
        }

        process_map.into_values().collect()
    }
}

impl Default for ProcessType {
    fn default() -> Self {
        ProcessType::Normal
    }
}

impl ProcessConfig {
    pub(crate) fn check_config(&self) -> Result<(), String> {
        if self.command.is_empty() {
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

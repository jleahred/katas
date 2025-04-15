use chrono::Local;
use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Deserialize)]
pub struct Config {
    pub uid: String,
    pub file_format: String,
    pub process: Option<Vec<ProcessConfig>>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ProcessConfig {
    pub id: String,
    pub command: String,
    pub apply_on: NaiveDateTime,
    // pub apply_on: chrono::DateTime<chrono::Local>,
}

pub struct ActiveProcessesConfigured(pub Vec<ProcessConfig>);

impl Config {
    pub fn active_processes(self: &Self) -> ActiveProcessesConfigured {
        let now = Local::now().naive_local();
        let mut process_map: HashMap<String, ProcessConfig> = HashMap::new();

        if let Some(processes) = &self.process {
            for process in processes {
                if process.apply_on < now {
                    let entry = process_map
                        .entry(process.id.clone())
                        .or_insert(process.clone());
                    if entry.apply_on < process.apply_on {
                        *entry = process.clone();
                    }
                }
            }
        }

        ActiveProcessesConfigured(process_map.into_values().collect())
    }
}

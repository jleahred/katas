use crate::process_config::ProcessConfig;
use chrono::{Local, NaiveDateTime};
use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use toml;

#[derive(Deserialize)]
struct Config {
    uid: String,
    process: Option<Vec<ProcessConfig>>,
}

pub fn read_processes(file_path: &str) -> Vec<ProcessConfig> {
    let content = fs::read_to_string(file_path).unwrap_or_else(|err| {
        panic!("Failed to read the TOML file at '{}': {}", file_path, err);
    });

    let config: Config = toml::from_str(&content).unwrap_or_else(|err| {
        panic!("Failed to parse the TOML file at '{}': {}", file_path, err);
    });

    let now = Local::now().naive_local();
    let mut process_map: HashMap<String, ProcessConfig> = HashMap::new();

    if let Some(processes) = config.process {
        for process in processes {
            if let Some(apply_on) = process.apply_on {
                if apply_on < now {
                    let entry = process_map
                        .entry(process.id.clone())
                        .or_insert(process.clone());
                    if entry.apply_on.unwrap_or(NaiveDateTime::MIN) < apply_on {
                        *entry = process;
                    }
                }
            }
        }
    } else {
        eprintln!(
            "Warning: No processes found in the TOML file at '{}'",
            file_path
        );
    }

    process_map.into_values().collect()
}

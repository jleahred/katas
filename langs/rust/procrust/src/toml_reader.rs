use crate::process_config::ProcessConfig;
use serde::Deserialize;
use std::fs;
use toml;

#[derive(Deserialize)]
struct Config {
    process: Option<Vec<ProcessConfig>>,
}

pub fn read_processes(file_path: &str) -> Vec<ProcessConfig> {
    let content = fs::read_to_string(file_path).expect("Failed to read the TOML file");
    let config: Config = toml::from_str(&content).expect("Failed to parse the TOML file");
    config.process.unwrap_or(vec![])
}

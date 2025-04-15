use crate::types::config::Config;
use std::fs;
use toml;

pub fn read_config_file(file_path: &str) -> Config {
    let content = fs::read_to_string(file_path).unwrap_or_else(|err| {
        panic!("Failed to read the TOML file at '{}': {}", file_path, err);
    });

    toml::from_str(&content).unwrap_or_else(|err| {
        panic!("Failed to parse the TOML file at '{}': {}", file_path, err);
    })
}

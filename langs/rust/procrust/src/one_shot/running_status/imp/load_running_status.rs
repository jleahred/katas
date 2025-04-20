use crate::one_shot::running_status::RunningStatus;
use crate::types::config::ConfigUid;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

pub(crate) fn load_running_status(file_path: &str, file_uid: &ConfigUid) -> RunningStatus {
    let full_path = format!("{}/{}.toml", file_path, file_uid.0); // Construir la ruta completa

    if Path::new(&full_path).exists() {
        let content = fs::read_to_string(&full_path)
            .unwrap_or_else(|err| panic!("Failed to read file {}: {}", full_path, err));
        toml::from_str(&content)
            .unwrap_or_else(|err| panic!("Failed to parse TOML from file {}: {}", full_path, err))
    } else {
        println!(
            "File {} does not exist. Returning default RunningStatus.",
            full_path
        );
        RunningStatus {
            file_uid: file_uid.clone(),
            _file_format: String::from("0"),
            processes: HashMap::new(),
        }
    }
}

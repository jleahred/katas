use crate::types::process_watched::ProcessWatched;
use std::fs;
use std::path::Path;

pub fn read_all_process_watcheds(dir_path: &str) -> Vec<ProcessWatched> {
    if let Ok(entries) = fs::read_dir(dir_path) {
        entries
            .filter_map(|entry| entry.ok())
            .filter_map(|entry| process_file(entry.path()))
            .collect()
    } else {
        eprintln!("Failed to read directory: {}", dir_path);
        Vec::new()
    }
}

fn process_file(path: std::path::PathBuf) -> Option<ProcessWatched> {
    if path.is_file() && path.extension().and_then(|ext| ext.to_str()) == Some("toml") {
        match fs::read_to_string(&path) {
            Ok(content) => parse_toml(&path, &content),
            Err(_) => {
                eprintln!("Failed to read TOML file: {}", path.display());
                None
            }
        }
    } else {
        None
    }
}

fn parse_toml(path: &Path, content: &str) -> Option<ProcessWatched> {
    match toml::from_str::<ProcessWatched>(content) {
        Ok(watcher) => Some(watcher),
        Err(err) => {
            eprintln!("Failed to parse TOML file '{}': {}", path.display(), err);
            None
        }
    }
}

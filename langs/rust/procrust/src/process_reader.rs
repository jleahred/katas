use crate::process_watcher;
use crate::process_watcher::ProcessWatched;
use std::fs;
use std::io;
use std::path::Path;

pub fn read_all_process_watchers(dir_path: &str) -> Vec<ProcessWatched> {
    let mut watchers = Vec::new();

    if let Ok(entries) = fs::read_dir(dir_path) {
        for entry in entries {
            if let Ok(entry) = entry {
                let path = entry.path();
                if path.is_file() && path.extension().and_then(|ext| ext.to_str()) == Some("toml") {
                    if let Ok(content) = fs::read_to_string(&path) {
                        if let Ok(mut watcher) = toml::from_str::<ProcessWatched>(&content) {
                            watcher.id = path
                                .file_stem()
                                .and_then(|stem| stem.to_str())
                                .unwrap_or("")
                                .to_string();
                            watchers.push(watcher);
                        } else {
                            eprint!("Failed to parse TOML file: {}", path.display());
                        }
                    }
                }
            }
        }
    }

    watchers
}

use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use std::fs::{self, File};
use std::io::Write;
use std::path::Path;
use toml;

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub enum ProcessStatus {
    Running,
    Stopping {
        retries: u32,
        last_attempt: chrono::DateTime<chrono::Local>,
    },
    ScheduledStop,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ProcessWatched {
    pub id: String,
    pub pid: u32,
    pub procrust_uid: String,
    pub status: ProcessStatus,
    pub applied_on: NaiveDateTime,
}

pub fn write_process_watched(process_id: &str, pid: u32, cmd: &str) -> std::io::Result<()> {
    let dir_path = "/tmp/procrust/";
    fs::create_dir_all(dir_path)?;

    let file_path = format!("{}{}.toml", dir_path, process_id);
    let process_watched = ProcessWatched {
        id: process_id.to_string(),
        pid,
        procrust_uid: cmd.to_string(), // todo:0 mejor un uuid
        status: ProcessStatus::Running,
        applied_on: chrono::Local::now().naive_utc(),
    };

    // Serialize the structure to TOML
    let toml =
        toml::to_string(&process_watched).expect("Failed to serialize ProcessWatched to TOML");
    let mut file = File::create(Path::new(&file_path))?;
    file.write_all(toml.as_bytes())?;

    Ok(())
}

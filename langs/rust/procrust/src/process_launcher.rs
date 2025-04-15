use crate::run_process::run_process;
use crate::types::process_config::ProcessConfig;
use crate::types::process_watcher::{ProcessStatus, ProcessWatched};
use chrono::NaiveDateTime;
use std::fs::{self, File};
use std::io::Write;
use std::path::Path;
use toml;

pub fn launch_missing_processes(missing_processes: &[ProcessConfig]) {
    for process in missing_processes {
        let file_path = format!("/tmp/procrust/{}.toml", process.id);

        if Path::new(&file_path).exists() {
            println!(
                "Launch canceled for process {} (apply_on: {}), waiting for process to stop.",
                process.id, process.apply_on
            );
            continue;
        }

        match run_process(process) {
            Ok(pid) => {
                println!(
                    "Launched process {} with PID: {}   apply_on: {}",
                    process.id, pid, process.apply_on
                );
                if let Err(e) = write_process_watched(
                    &process.id,
                    pid,
                    &process.command,
                    process.apply_on.clone(),
                ) {
                    eprintln!("Failed to write process file for {}: {}", process.id, e);
                }
            }
            Err(e) => eprintln!("Failed to launch process {}: {}", process.id, e),
        }
    }
}

fn write_process_watched(
    process_id: &str,
    pid: u32,
    cmd: &str,
    apply_on: NaiveDateTime,
) -> std::io::Result<()> {
    let dir_path = "/tmp/procrust/";
    fs::create_dir_all(dir_path)?;

    let file_name = format!("{}.toml", process_id);

    let file_path = format!("{}{}", dir_path, file_name);
    let process_watched = ProcessWatched {
        id: process_id.to_string(),
        pid,
        apply_on,
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

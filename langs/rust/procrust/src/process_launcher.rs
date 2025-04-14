use crate::process_config::ProcessConfig;
use crate::process_watcher::write_process_watched;
use crate::run_process::run_process;
use std::path::Path;

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

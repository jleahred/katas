use crate::process_config::ProcessConfig;
use crate::process_watcher::write_process_watched;
use crate::run_process::run_process;

pub fn launch_missing_processes(missing_processes: &[ProcessConfig]) {
    for process in missing_processes {
        match run_process(process) {
            Ok(pid) => {
                println!("Launched process {} with PID: {}", process.id, pid);
                if let Err(e) = write_process_watched(&process.id, pid, &process.command) {
                    eprintln!("Failed to write process file for {}: {}", process.id, e);
                }
            }
            Err(e) => eprintln!("Failed to launch process {}: {}", process.id, e),
        }
    }
}

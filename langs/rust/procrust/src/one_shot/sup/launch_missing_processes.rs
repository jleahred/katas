use crate::launch_process::launch_process;
use crate::types::config::ProcessConfig;
use crate::types::process_watched::{ProcessStatus, ProcessWatched};
use chrono::NaiveDateTime;
use std::fs::{self, File};
use std::io;
use std::io::Write;
use std::path::Path;
use std::process::{Child, Command};
use std::thread;
use std::time::Duration;
use toml;

// pub fn launch_missing_processes(path_persist_watched: &str, missing_processes: &[ProcessConfig]) {
//     for process in missing_processes {
//         let file_path = format!("{}/{}.toml", path_persist_watched, process.id);

//         if Path::new(&file_path).exists() {
//             println!(
//                 "Launch canceled for process {} (apply_on: {}), waiting for process to stop.",
//                 process.id, process.apply_on
//             );
//             continue;
//         }
//           match run_process(process) {s
//             Ok(pid) => {
//                 println!(
//                     "Launched process {} with PID: {}   apply_on: {}",
//                     process.id, pid, process.apply_on
//                 );
//                 if let Err(e) = write_process_watched(
//                     &path_persist_watched,
//                     &process.id,
//                     pid,
//                     &process.command,
//                     process.apply_on.clone(),
//                 ) {
//                     eprintln!("Failed to write process file for {}: {}", process.id, e);
//                 }
//             }
//             Err(e) => eprintln!("Failed to launch process {}: {}", process.id, e),
//         }
//     }
// }

// fn write_process_watched(
//     path_persist_watched: &str,
//     process_id: &str,
//     pid: u32,
//     cmd: &str,
//     apply_on: NaiveDateTime,
// ) -> std::io::Result<()> {
//     fs::create_dir_all(path_persist_watched)?;

//     let file_name = format!("{}.toml", process_id);

//     let file_path = format!("{}/{}", path_persist_watched, file_name);
//     let process_watched = ProcessWatched {
//         id: process_id.to_string(),
//         pid,
//         apply_on,
//         procrust_uid: cmd.to_string(), // todo:0 mejor un uuid
//         status: ProcessStatus::Running,
//         applied_on: chrono::Local::now().naive_utc(),
//     };

//     // Serialize the structure to TOML
//     let toml =
//         toml::to_string(&process_watched).expect("Failed to serialize ProcessWatched to TOML");
//     let mut file = File::create(Path::new(&file_path))?;
//     file.write_all(toml.as_bytes())?;

//     Ok(())
// }

// fn run_process(process: &ProcessConfig) -> Result<u32, io::Error> {
//     let mut child: Child = Command::new("sh")
//         .arg("-c")
//         .arg(&process.command)
//         .env("PROCRUST", &process.command)
//         .spawn()?;

//     thread::sleep(Duration::from_secs(2));

//     match child.try_wait()? {
//         Some(status) => {
//             if status.success() {
//                 eprintln!(
//                     "Running process, finished OK  ??  {} (apply_on: {})",
//                     process.id, process.apply_on
//                 );
//             } else {
//                 eprintln!(
//                     "Running process, finished with error  :-(  {} (apply_on: {})",
//                     process.id, process.apply_on
//                 );
//             }
//         }
//         None => {}
//     }

//     Ok(child.id())
// }

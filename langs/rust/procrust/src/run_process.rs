use crate::types::process_config::ProcessConfig;
use std::io;
use std::process::{Child, Command};
use std::thread;
use std::time::Duration;

pub fn run_process(process: &ProcessConfig) -> Result<u32, io::Error> {
    let mut child: Child = Command::new("sh")
        .arg("-c")
        .arg(&process.command)
        .env("PROCRUST", &process.command)
        .spawn()?;

    thread::sleep(Duration::from_secs(2));

    match child.try_wait()? {
        Some(status) => {
            if status.success() {
                eprintln!(
                    "Running process, finished OK  ??  {} (apply_on: {})",
                    process.id, process.apply_on
                );
            } else {
                eprintln!(
                    "Running process, finished with error  :-(  {} (apply_on: {})",
                    process.id, process.apply_on
                );
            }
        }
        None => {}
    }

    Ok(child.id())
}

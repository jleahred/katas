use crate::process_config::ProcessConfig;
use std::io;
use std::process::{Child, Command};

pub fn run_process(process: &ProcessConfig) -> Result<u32, io::Error> {
    let child: Child = Command::new("sh")
        .arg("-c")
        .arg(&process.command)
        .env("SUPRUST", &process.command)
        .spawn()?;

    Ok(child.id())
}

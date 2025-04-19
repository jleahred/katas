use crate::types::{ProcessStatus, ProcessWatched};
use std::fs;
use std::path::Path;

pub(crate) fn send_kill_if_stopping(watched_dir: &str) {
    let entries = match fs::read_dir(watched_dir) {
        Ok(entries) => entries,
        Err(e) => {
            eprintln!("Failed to read watched directory: {}", e);
            return;
        }
    };

    for entry in entries {
        if let Ok(entry) = entry {
            let path = entry.path();
            if path.is_file() {
                if let Err(e) = process_file(&path) {
                    eprintln!("Error processing file {:?}: {}", path, e);
                }
            }
        }
    }
}

fn process_file(path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let content = fs::read_to_string(path)?;
    let mut watched: ProcessWatched = toml::from_str(&content)?;

    dbg!(&watched.pid);
    match watched.status {
        ProcessStatus::Stopping {
            ref mut retries,
            ref mut last_attempt,
        } => {
            *retries += 1;
            *last_attempt = chrono::Local::now();
            if *retries > 5 {
                kill_process(watched.pid, true)?;
            } else {
                kill_process(watched.pid, false)?;
            }
            let updated_content = toml::to_string(&watched)?;
            fs::write(path, updated_content)?;
        }
        ProcessStatus::ScheduledStop => {
            kill_process(watched.pid, false)?;
            let updated_content = toml::to_string(&ProcessWatched {
                id: watched.id,
                pid: watched.pid,
                apply_on: watched.apply_on,
                procrust_uid: watched.procrust_uid,
                status: ProcessStatus::Stopping {
                    retries: 0,
                    last_attempt: chrono::Local::now(),
                },
                applied_on: watched.applied_on,
            })?;
            fs::write(path, updated_content)?;
        }
        _ => {}
    }

    Ok(())
}

fn kill_process(pid: u32, force: bool) -> Result<(), Box<dyn std::error::Error>> {
    use nix::sys::signal::{kill, Signal};
    use nix::unistd::Pid;

    let signal = if force {
        Signal::SIGKILL
    } else {
        Signal::SIGTERM
    };
    let status = kill(Pid::from_raw(pid as i32), signal);

    if status.is_ok() {
        println!("Successfully sent signal {} to PID {}", signal, pid);
    } else {
        eprintln!("Failed to send signal {} to PID {}", signal, pid);
    }

    Ok(())
}

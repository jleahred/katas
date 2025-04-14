use crate::process_watcher::ProcessWatched;
use std::fs;
use std::path::Path;

pub fn kill_stale_processes(watched_dir: &str) {
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

    match watched.status {
        crate::process_watcher::ProcessStatus::Stopping {
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
            dbg!(&updated_content);
            fs::write(path, updated_content)?;
        }
        crate::process_watcher::ProcessStatus::ScheduledStop => {
            kill_process(watched.pid, false)?;
            let updated_content = toml::to_string(&ProcessWatched {
                id: watched.id,
                pid: watched.pid,
                procrust_uid: watched.procrust_uid,
                status: crate::process_watcher::ProcessStatus::Stopping {
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

    // fn kill_pid(pid: u32) {
    //     let _ = kill(Pid::from_raw(pid as i32), Signal::SIGTERM);
    // }

    // let signal = if force { "-9" } else { "-15" };
    // let status = Command::new("kill")
    //     .arg(signal)
    //     .arg(pid.to_string())
    //     .status()?;

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

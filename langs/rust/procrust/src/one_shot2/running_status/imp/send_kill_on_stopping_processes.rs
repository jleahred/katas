use super::super::RunningStatus;
use crate::one_shot2::running_status::{ProcessStatus, ProcessWatched};

pub(crate) fn send_kill_on_stopping_processes(mut st: RunningStatus) -> RunningStatus {
    for proc_watched in st.processes.values_mut() {
        match send_kill_if_so(proc_watched) {
            Ok(updated_proc) => {
                *proc_watched = updated_proc;
            }
            Err(err) => {
                eprintln!(
                    "Failed to send kill signal for process '{}': {}",
                    proc_watched.id.0, err
                );
            }
        }
    }
    st
}

pub(crate) fn send_kill_if_so(
    proc_watched: &ProcessWatched,
) -> Result<ProcessWatched, Box<dyn std::error::Error>> {
    match proc_watched.status {
        ProcessStatus::Stopping { retries, .. } => {
            if retries < 5 {
                kill_process(proc_watched.pid, false)?;
            } else {
                kill_process(proc_watched.pid, true)?;
            };

            let result = ProcessWatched {
                id: proc_watched.id.clone(),
                pid: proc_watched.pid,
                apply_on: proc_watched.apply_on,
                procrust_uid: proc_watched.procrust_uid.clone(),
                status: ProcessStatus::Stopping {
                    retries: retries + 1,
                    last_attempt: chrono::Local::now(),
                },
                applied_on: proc_watched.applied_on,
            };
            Ok(result)
        }
        ProcessStatus::ScheduledStop => Ok(ProcessWatched {
            id: proc_watched.id.clone(),
            pid: proc_watched.pid,
            apply_on: proc_watched.apply_on,
            procrust_uid: proc_watched.procrust_uid.clone(),
            status: ProcessStatus::Stopping {
                retries: 0,
                last_attempt: chrono::Local::now(),
            },
            applied_on: proc_watched.applied_on,
        }),
        ProcessStatus::Running => Ok(proc_watched.clone()),
    }
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

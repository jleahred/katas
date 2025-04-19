use super::super::{ProcessStatus, RunningStatus};
use crate::types::config::{ProcessConfig, ProcessId};

pub(crate) fn mark_stopping(
    mut rs: RunningStatus,
    conf_proc_active: &[ProcessConfig],
) -> RunningStatus {
    let active_ids: Vec<ProcessId> = conf_proc_active
        .iter()
        .map(|conf| conf.id.clone())
        .collect();

    for (id, process) in rs.processes.iter_mut() {
        if !active_ids.contains(id) && process.status == ProcessStatus::Running {
            println!(
                "Marking process {} with PID {} as stopping",
                id.0, process.pid
            );
            process.status = ProcessStatus::Stopping {
                retries: 0,
                last_attempt: chrono::Local::now(),
            };
        }
    }

    rs
}

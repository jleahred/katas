use crate::one_shot::running_status::{ProcessStatus, ProcessWatched, RunningStatus};
use crate::types::config::ProcessConfig;

pub(crate) fn ready2start_from_missing_watched(
    mut running_status: RunningStatus,
    proccesses_conf_pending2run: &Vec<ProcessConfig>,
) -> RunningStatus {
    for process in proccesses_conf_pending2run {
        running_status.processes.insert(
            process.id.clone(),
            ProcessWatched {
                id: process.id.clone(),
                apply_on: process.apply_on,
                procrust_uid: process.command.0.clone(), // TODO: Mejorar con un UUID único
                status: ProcessStatus::Ready2Start {
                    command: process.command.clone(),
                    process_id: process.id.clone(),
                    start_health_check: process.start_health_check.clone(),
                    apply_on: process.apply_on,
                },
                applied_on: chrono::Local::now().naive_utc(),
            },
        );
        println!(
            "Process {} is ready to start. Command: {}",
            process.id.0, process.command.0
        );
    }

    running_status
}

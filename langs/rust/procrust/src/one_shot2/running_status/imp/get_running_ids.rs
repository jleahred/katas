use crate::one_shot2::running_status::ProcessStatus;
use crate::one_shot2::running_status::RunningStatus;
use crate::types::config::ProcessId;

pub(crate) fn get_running_ids(runnstatus: &RunningStatus) -> Vec<ProcessId> {
    runnstatus
        .processes
        .iter()
        .filter_map(|(id, process)| {
            if matches!(process.status, ProcessStatus::Running) {
                Some(id.clone())
            } else {
                None
            }
        })
        .collect()
}

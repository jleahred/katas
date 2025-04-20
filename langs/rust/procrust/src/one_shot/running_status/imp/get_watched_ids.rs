use crate::one_shot::running_status::ProcessStatus;
use crate::one_shot::running_status::RunningStatus;
use crate::types::config::ProcessId;

pub(crate) fn get_watched_ids(runnstatus: &RunningStatus) -> Vec<ProcessId> {
    runnstatus
        .processes
        .iter()
        .filter_map(|(id, process)| {
            let opid = match process.status {
                ProcessStatus::ScheduledStop { pid } => Some(pid),
                ProcessStatus::Stopping { pid, .. } => Some(pid),
                ProcessStatus::Running { pid } => Some(pid),

                ProcessStatus::PendingHealthStartCheck { .. }
                | ProcessStatus::Ready2Start { .. } => None,
            };

            if opid.is_none() {
                Some(id.clone())
            } else {
                None
            }
        })
        .collect()
}

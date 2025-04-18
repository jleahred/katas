use crate::types::ProcessWatched;

pub struct AllProcWatched(pub Vec<ProcessWatched>);

pub struct WatchedRunningProcesses(pub(super) Vec<crate::types::ProcessWatched>);
pub fn get_running_processes(processes: &AllProcWatched) -> WatchedRunningProcesses {
    let result = processes
        .0
        .iter()
        .filter(|process| match process.status {
            crate::types::ProcessStatus::Running => true,
            crate::types::ProcessStatus::Stopping { .. } => false,
            crate::types::ProcessStatus::ScheduledStop => false,
        })
        .cloned()
        .collect();
    WatchedRunningProcesses(result)
}

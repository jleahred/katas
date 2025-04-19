use crate::types::config::{ProcessConfig, ProcessId};

pub(super) fn filter_active_procs_by_config_with_running(
    active_procs_by_config: &[ProcessConfig],
    running_processes: &[ProcessId],
) -> Vec<ProcessConfig> {
    active_procs_by_config
        .iter()
        .filter(|proc_config| {
            proc_config.depends_on.is_empty()
                || proc_config
                    .depends_on
                    .iter()
                    .all(|dep| running_processes.contains(&ProcessId(dep.clone())))
        })
        .cloned()
        .collect()
}

pub(super) fn get_pending2run_processes(
    active_procs_cfg_all_depends_running: &[ProcessConfig],
    running_processes: &[ProcessId],
) -> Vec<ProcessConfig> {
    active_procs_cfg_all_depends_running
        .iter()
        .filter(|proc_config| !running_processes.contains(&proc_config.id))
        .cloned()
        .collect()
}

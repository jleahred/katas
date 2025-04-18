//   devuelve cfg_proc eliminando los que tengan dependencias que no estén en ejercución

use super::ActiveByDateProcConf;
use super::WatchedRunningProcesses;
use crate::types::config::ProcessConfig;

pub struct CfgProcsFilterByDependency(pub Vec<ProcessConfig>);

pub fn cfg_proc_filter_by_dependency(
    cfg_procs: &ActiveByDateProcConf,
    running: &WatchedRunningProcesses,
) -> CfgProcsFilterByDependency {
    let running_ids: Vec<&String> = running.0.iter().map(|p| &p.id).collect();

    let result = cfg_procs
        .0
        .iter()
        .filter(|proc| {
            if let Some(depends_on) = &proc.depends_on {
                depends_on.iter().all(|dep| running_ids.contains(&dep))
            } else {
                true // Keep processes without dependencies
            }
        })
        .cloned()
        .collect();
    CfgProcsFilterByDependency(result)
}

use crate::process_config::ProcessConfig;
use crate::process_watcher::ProcessWatched;

pub fn diff_processes(
    watched: &[ProcessWatched],
    config: &[ProcessConfig],
) -> (Vec<ProcessWatched>, Vec<ProcessConfig>) {
    let only_in_watched: Vec<ProcessWatched> = watched
        .iter()
        .filter(|w| !config.iter().any(|c| c.id == w.id))
        .cloned()
        .collect();

    let only_in_config: Vec<ProcessConfig> = config
        .iter()
        .filter(|c| !watched.iter().any(|w| w.id == c.id))
        .cloned()
        .collect();

    (only_in_watched, only_in_config)
}

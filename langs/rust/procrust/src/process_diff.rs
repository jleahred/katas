use crate::process_config::ProcessConfig;
use crate::process_watcher::ProcessWatched;

pub fn diff_processes(
    watched: &[ProcessWatched],
    config: &[ProcessConfig],
) -> (Vec<ProcessWatched>, Vec<ProcessConfig>) {
    let only_in_watched: Vec<ProcessWatched> = watched
        .iter()
        .filter(|w| {
            !config
                .iter()
                .any(|c| c.id == w.id && c.apply_on == w.apply_on)
        })
        .cloned()
        .collect();

    let only_in_config: Vec<ProcessConfig> = config
        .iter()
        .filter(|c| {
            !watched
                .iter()
                .any(|w| w.id == c.id && w.apply_on == c.apply_on)
        })
        .cloned()
        .collect();

    // Debug output (optional)
    for process in &only_in_watched {
        eprintln!(
            "Only in watched: {} (apply_on: {})",
            process.id, process.apply_on
        );
    }
    for process in &only_in_config {
        eprintln!(
            "Only in config: {} (apply_on: {})",
            process.id, process.apply_on
        );
    }

    (only_in_watched, only_in_config)
}

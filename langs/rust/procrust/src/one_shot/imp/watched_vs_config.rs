use crate::types::config::ProcessConfig;
use crate::types::ProcessWatched;

pub(crate) struct OnlyInWatched(pub(crate) Vec<ProcessWatched>);
pub(crate) struct OnlyInConfig(pub(crate) Vec<ProcessConfig>);

pub(crate) fn watched_vs_config(
    watched: &[ProcessWatched],
    config: &[ProcessConfig],
) -> (OnlyInWatched, OnlyInConfig) {
    // Find processes that are only in the watched list but not in the config list
    let only_in_watched: Vec<ProcessWatched> = watched
        .iter()
        .filter(|w| {
            !config
                .iter()
                .any(|c| c.id == w.id && c.apply_on == w.apply_on)
        })
        .cloned()
        .collect();

    // Find processes that are only in the config list but not in the watched list
    let only_in_config: Vec<ProcessConfig> = config
        .iter()
        .filter(|c| {
            !watched
                .iter()
                .any(|w| w.id == c.id && w.apply_on == c.apply_on)
        })
        .cloned()
        .collect();

    // Debug output: Log processes that are only in the watched list
    for process in &only_in_watched {
        eprintln!(
            "Only in watched: {} (apply_on: {})",
            process.id, process.apply_on
        );
    }
    // Debug output: Log processes that are only in the config list
    for process in &only_in_config {
        eprintln!(
            "Only in config: {} (apply_on: {})",
            process.id, process.apply_on
        );
    }

    (OnlyInWatched(only_in_watched), OnlyInConfig(only_in_config))
}

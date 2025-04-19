use crate::types::config::ProcessConfig;
use crate::types::ProcessWatched;

pub(crate) fn print_diff(only_in_watched: &[ProcessWatched], only_in_config: &[ProcessConfig]) {
    if !only_in_watched.is_empty() {
        println!("\nProcesses only in watched:");
    }
    for process in only_in_watched {
        println!("- {} (apply_on: {})", process.id, process.apply_on);
    }

    if !only_in_config.is_empty() {
        println!("\nProcesses only in config:");
    }
    for process in only_in_config {
        println!("- {} (apply_on: {})", process.id, process.apply_on);
    }

    println!();
}

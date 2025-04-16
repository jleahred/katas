mod sup;

use crate::read_config_file::read_config_file;
use crate::types::config::{Config, ProcessConfig};
use crate::types::process_watched::ProcessWatched;

pub fn one_shot() {
    println!("\n--------------------------------------------------------------------------------");
    println!("Checking... {}\n", chrono::Local::now());

    let config: Config = read_config_file("processes.toml");
    let path_persist_watched = format!("/tmp/procrust/{}/", config.uid);

    let active_proc_in_config = config.active_processes();
    let proc_watcheds = sup::read_all_process_watcheds(&path_persist_watched);

    let (only_in_watched, only_in_config) =
        sup::watched_vs_config(&proc_watcheds, &active_proc_in_config.0);

    print_diff(&only_in_watched.0, &only_in_config.0);

    sup::launch_missing_processes(&path_persist_watched, &only_in_config.0);
    handle_stale_processes(&path_persist_watched, &only_in_watched.0, &proc_watcheds);
}

fn print_diff(only_in_watched: &[ProcessWatched], only_in_config: &[ProcessConfig]) {
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

fn handle_stale_processes(
    path_persist_watched: &str,
    only_in_watched: &[ProcessWatched],
    proc_watcheds: &[ProcessWatched],
) {
    sup::watched_update2stopping(path_persist_watched, only_in_watched);
    sup::clean_stale_watched_files(path_persist_watched, proc_watcheds);
    sup::kill_stale_processes(path_persist_watched);
}

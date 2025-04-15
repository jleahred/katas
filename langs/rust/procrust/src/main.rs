mod process_cleaner;
mod process_diff;
mod process_killer;
mod process_launcher;
mod process_reader;
mod process_updater;
mod run_process;
mod toml_reader;
mod types;

use crate::types::process_watcher::ProcessWatched;
use process_cleaner::clean_stale_watched_files;
use std::time::Duration;
use uuid::Uuid;

use process_diff::diff_processes;
use process_killer::kill_stale_processes;
use process_launcher::launch_missing_processes;
use process_reader::read_all_process_watchers;
use process_updater::update_to_stopping;
use toml_reader::read_processes;
use types::process_config::ProcessConfig;

use std::env;
use std::process::Command;
use std::thread::sleep;

fn main() {
    if env::args().any(|arg| arg == "--one-shot") {
        process_oneshot();
        return;
    } else if env::args().any(|arg| arg == "--uid") {
        println!("{}", Uuid::new_v4().to_string());
        return;
    }

    loop {
        let current_exe = env::current_exe().expect("CRITIC: Can't get current executable path");

        // Create a new process
        let mut child = Command::new(current_exe)
            .args(["--one-shot"])
            .spawn()
            .expect("CRITIC: Can't spawn child process");

        // println!("New process created with PID: {}", child.id());
        if child.wait().is_err() {
            eprintln!("Error waiting for child process");
        } else {
            println!("Shot finished");
        }
        sleep(Duration::from_secs(2));
    }
}

fn process_oneshot() {
    println!("\n--------------------------------------------------------------------------------");
    println!("Checking... {}\n", chrono::Local::now());

    let proc_config = read_processes("processes.toml");
    let proc_watcheds = read_all_process_watchers("/tmp/procrust/");

    let (only_in_watched, only_in_config) = diff_processes(&proc_watcheds, &proc_config);

    print_diff(&only_in_watched, &only_in_config);

    launch_missing_processes(&only_in_config);
    handle_stale_processes(&only_in_watched, &proc_watcheds);
}

fn print_diff(only_in_watched: &[ProcessWatched], only_in_config: &[ProcessConfig]) {
    println!("\nProcesses only in watched:");
    for process in only_in_watched {
        println!("- {} (apply_on: {})", process.id, process.apply_on);
    }

    println!("\nProcesses only in config:");
    for process in only_in_config {
        println!("- {} (apply_on: {})", process.id, process.apply_on);
    }

    println!();
}

fn handle_stale_processes(only_in_watched: &[ProcessWatched], proc_watcheds: &[ProcessWatched]) {
    update_to_stopping(only_in_watched);
    clean_stale_watched_files(proc_watcheds);
    kill_stale_processes("/tmp/procrust/");
}

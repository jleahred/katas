mod process_checker;
mod process_cleaner;
mod process_cmd;
mod process_config;
mod process_cookie;
mod process_diff;
mod process_killer;
mod process_launcher;
mod process_reader;
mod process_updater;
mod process_watcher;
mod run_process;
mod toml_reader;

// use std::thread;
use std::time::Duration;
use uuid::Uuid;
// use process_checker::is_process_running;
use crate::process_watcher::ProcessWatched;
use process_cleaner::clean_stale_watched_files;

use process_config::ProcessConfig;
use process_diff::diff_processes;
use process_killer::kill_stale_processes; // Import the new function
use process_launcher::launch_missing_processes;
use process_reader::read_all_process_watchers;
use process_updater::update_to_stopping;
// use process_watcher::write_process_watched;
// use run_process::run_process;
use toml_reader::read_processes;

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
        // Obtener la ruta del ejecutable actual
        let current_exe = env::current_exe().expect("No se pudo obtener la ruta del ejecutable");

        // Obtener los argumentos actuales
        // let args: Vec<String> = env::args().skip(1).collect();

        // Crear un nuevo proceso con el mismo ejecutable y argumentos
        let mut child = Command::new(current_exe)
            .args(["--one-shot"])
            .spawn()
            .expect("No se pudo crear el proceso hijo");

        // Opcional: imprimir el PID del nuevo proceso
        println!("Nuevo proceso creado con PID: {}", child.id());
        if child.wait().is_err() {
            eprintln!("Error al esperar el proceso hijo");
        } else {
            println!("Proceso hijo terminado");
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
    kill_stale_processes("/tmp/procrust/"); // todo:0 add path to watched_processes_path?
}

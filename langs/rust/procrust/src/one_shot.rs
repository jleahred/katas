mod imp;

use crate::read_config_file::read_config_file;
use crate::types::config::Config;
use std::fs::{self};

pub fn one_shot() {
    println!("\n--------------------------------------------------------------------------------");
    println!("Checking... {}\n", chrono::Local::now());

    let config: Config = read_config_file("processes.toml");
    let path_persist_watched = format!("/tmp/procrust/{}/", config.uid);
    fs::create_dir_all(&path_persist_watched).expect("Failed to create directory on /tmp/procrust");

    imp::send_kill_if_stopping(&path_persist_watched);

    let all_proc_watcheds = imp::read_all_process_watcheds(&path_persist_watched);
    let watched_running_processes = imp::get_running_processes(&all_proc_watcheds);
    let conf_proc_act_by_date = imp::get_act_proc_conf_by_date(&config);

    let (only_in_watched, only_in_config) =
        imp::watched_vs_config(&all_proc_watcheds.0, &conf_proc_act_by_date.0);

    imp::print_diff(&only_in_watched.0, &only_in_config.0);

    imp::watched_update2stopping(&path_persist_watched, &only_in_watched.0);

    imp::del_watched_files_if_missing_pid(&path_persist_watched, &all_proc_watcheds);
    imp::launch_missing_processes(&path_persist_watched, &config.uid, &only_in_config.0);
}

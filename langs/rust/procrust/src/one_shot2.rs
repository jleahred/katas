mod running_status;
use crate::read_config_file::read_config_file_or_panic;
use crate::types::config::{Config, ProcessConfig};

const RUNNING_STATUS_FOLDER: &str = "/tmp/procrust";

pub fn one_shot2() {
    println!("\n--------------------------------------------------------------------------------");
    println!("Checking... {}\n", chrono::Local::now());

    let config: Config = read_config_file_or_panic("processes.toml");
    let running_status = running_status::load_running_status(RUNNING_STATUS_FOLDER, &config.uid);
    let active_procs_by_config: Vec<ProcessConfig> = config.get_active_procs_by_config();

    running_status
        .send_kill_on_stopping_processes()
        .mark_running_as_stop_by_config()
        .del_if_missing_pid()
        .launch_missing_processes(&config.process)
        .save(RUNNING_STATUS_FOLDER);
}

mod imp;
mod running_status;

use crate::read_config_file::read_config_file_or_panic;
use crate::types::config::Config;

const RUNNING_STATUS_FOLDER: &str = "/tmp/procrust";

pub(crate) fn one_shot2() {
    println!("\n--------------------------------------------------------------------------------");
    println!("Checking... {}\n", chrono::Local::now());

    let config: Config = read_config_file_or_panic("processes.toml");
    let running_status = running_status::load_running_status(RUNNING_STATUS_FOLDER, &config.uid);
    let active_procs_by_config = config.get_active_procs_by_config();
    let running_ids = running_status.get_running_ids();
    let whatched_ids = running_status.get_watched_ids();
    let active_procs_cfg_all_depends_running =
        imp::filter_active_procs_by_config_with_running(&active_procs_by_config, &running_ids);
    let pending2watch =
        imp::get_pending2run_processes(&active_procs_cfg_all_depends_running, &whatched_ids);
    dbg!(&whatched_ids);
    //  --------------------

    running_status
        .del_if_missing_pid()
        .send_kill_on_stopping_processes()
        .mark_stopping(&active_procs_cfg_all_depends_running)
        .check_start_held_processes()
        .ready2start_from_missing_watched(&pending2watch)
        .launch_ready2start()
        .save(RUNNING_STATUS_FOLDER);
}

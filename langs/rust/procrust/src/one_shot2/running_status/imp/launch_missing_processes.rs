use crate::one_shot2::running_status::RunningStatus;
use crate::types::config::ProcessConfig;

pub(crate) fn launch_missing_processes(
    runnstatus: RunningStatus,
    proccesses_conf: &Vec<ProcessConfig>,
) -> RunningStatus {
    //  todo

    // get active proceesses by config
    // if not in running, launch it

    runnstatus
}

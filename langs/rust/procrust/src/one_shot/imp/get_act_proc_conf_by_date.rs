use crate::types::config::{Config, ProcessConfig, ProcessType};
use chrono::{Datelike, Local, NaiveDateTime};
use std::collections::HashMap;

pub(crate) struct ActiveByDateProcConf(pub(crate) Vec<ProcessConfig>);

pub(crate) fn get_act_proc_conf_by_date(config: &Config) -> ActiveByDateProcConf {
    let now = Local::now().naive_local();
    let mut process_map: HashMap<String, ProcessConfig> = HashMap::new();

    for process in &config.process {
        // Check if the process is valid
        if let Err(err) = process.check_config() {
            eprintln!(
                "Skipping process '{}' due to invalid configuration: {}",
                process.id, err
            );
            continue;
        }

        // Check if the process should apply
        if !should_apply(process, now) {
            eprintln!(
                "Skipping process '{}' as it does not meet the apply conditions.",
                process.id
            );
            continue;
        }

        let entry = process_map
            .entry(process.id.clone())
            .or_insert(process.clone());
        if entry.apply_on < process.apply_on {
            *entry = process.clone();
        }
    }

    ActiveByDateProcConf(process_map.into_values().collect())
}

fn should_apply(proc_conf: &ProcessConfig, datetime: NaiveDateTime) -> bool {
    // Check if the given datetime is after `apply_on`
    if datetime < proc_conf.apply_on {
        return false;
    }

    // Check if the process type is `Normal` or not set
    match &proc_conf.process_type {
        ProcessType::Normal => {}
        ProcessType::Fake => return false,
    }

    // Check if the schedule is defined
    if let Some(schedule) = &proc_conf.schedule {
        // Check if the day of the week matches
        let weekday = datetime.weekday();
        if !&schedule.week_days.matches(weekday) {
            return false;
        }

        // Check if the time is within the range of `start_time` and `stop_time`
        let time = datetime.time();
        if time < schedule.start_time || time >= schedule.stop_time {
            return false;
        }
    }

    true
}

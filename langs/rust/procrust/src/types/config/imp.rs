use super::*;

use chrono::Weekday;
use serde::de::{Deserializer, SeqAccess, Visitor};
use serde::Deserialize;
use std::fmt;

pub(super) fn is_valid_start_stop(proc_conf: &ProcessConfig) -> Result<(), String> {
    if let Some(schedule) = &proc_conf.schedule {
        if schedule.start_time < schedule.stop_time {
            Ok(())
        } else {
            Err(format!(
                "Invalid time range: start_time ({}) is not before stop_time ({})",
                schedule.start_time, schedule.stop_time
            ))
        }
    } else {
        Ok(()) // If no schedule is defined, consider it valid
    }
}

pub(super) fn deserialize_day_selection<'de, D>(deserializer: D) -> Result<DaySelection, D::Error>
where
    D: Deserializer<'de>,
{
    struct DaySelectionVisitor;

    impl<'de> Visitor<'de> for DaySelectionVisitor {
        type Value = DaySelection;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a list of weekdays, 'mon-fri' or 'all'")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            match v {
                "mon-fri" => Ok(DaySelection::Mon2Fri),
                "all" => Ok(DaySelection::All),
                _ => Err(E::custom(format!("unexpected string: {}", v))),
            }
        }

        fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
        where
            A: SeqAccess<'de>,
        {
            let days: Vec<Weekday> =
                Deserialize::deserialize(serde::de::value::SeqAccessDeserializer::new(seq))?;
            Ok(DaySelection::Days(days))
        }
    }

    deserializer.deserialize_any(DaySelectionVisitor)
}

pub(super) fn matches(ds: &DaySelection, weekday: chrono::Weekday) -> bool {
    match ds {
        DaySelection::Days(days) => days.contains(&weekday),
        DaySelection::Mon2Fri => {
            weekday.num_days_from_monday() >= chrono::Weekday::Mon.num_days_from_monday()
                && weekday.num_days_from_monday() <= chrono::Weekday::Fri.num_days_from_monday()
        }
        DaySelection::All => true,
    }
}

pub(crate) fn get_active_procs_by_config(config: &Config) -> Vec<ProcessConfig> {
    let now = Local::now().naive_local();
    let mut process_map: HashMap<ProcessId, ProcessConfig> = HashMap::new();

    for process in &config.process {
        if process.apply_on > now {
            continue;
        }

        if let Some(schedule) = &process.schedule {
            let weekday = now.weekday();
            let time = now.time();

            if !schedule.week_days.matches(weekday) {
                continue;
            }

            if time < schedule.start_time || time >= schedule.stop_time {
                continue;
            }
        }

        match process.process_type {
            ProcessType::Normal => {}
            ProcessType::Fake => {
                println!("[{}] Process type is fake, skipping...", process.id.0);
                continue;
            }
        }

        // keep more recent process config
        let entry = process_map
            .entry(process.id.clone())
            .or_insert_with(|| process.clone());

        if entry.apply_on < process.apply_on {
            *entry = process.clone();
        }
    }

    process_map.values().cloned().collect()
}

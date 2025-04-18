use super::*;

use chrono::Weekday;
use serde::de::{Deserializer, SeqAccess, Visitor};
use serde::Deserialize;
use std::fmt;

pub(super) fn is_valid_start_stop(proc_conf: &ProcessConfig) -> Result<(), String> {
    if let Some(schedule) = &proc_conf.schedule {
        match (schedule.start_time, schedule.stop_time) {
            (Some(start), Some(stop)) => {
                if start < stop {
                    Ok(())
                } else {
                    Err(format!(
                        "Invalid time range: start_time ({}) is not before stop_time ({})",
                        start, stop
                    ))
                }
            }
            (None, Some(_)) => Err("start_time is missing while stop_time is set".to_string()),
            _ => Ok(()),
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
            formatter.write_str("a list of weekdays or the string 'mon-fri'")
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

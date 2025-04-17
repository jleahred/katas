use chrono::Datelike;
use chrono::Local;
use chrono::NaiveDateTime;
use chrono::NaiveTime;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Deserialize)]
pub struct Config {
    pub uid: String,
    #[serde(rename = "file_format")]
    pub _file_format: String,
    pub process: Option<Vec<ProcessConfig>>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ProcessConfig {
    pub id: String,
    pub command: String,
    pub apply_on: NaiveDateTime,

    #[serde(default)]
    week_days: Option<DaySelection>,

    #[serde(default)]
    start_time: Option<NaiveTime>,

    #[serde(default)]
    stop_time: Option<NaiveTime>,

    #[serde(default, rename = "type")]
    pub process_type: Option<ProcessType>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum ProcessType {
    Normal,
    Fake,
}

impl ProcessConfig {
    pub fn is_valid_start_stop(&self) -> Result<(), String> {
        match (self.start_time, self.stop_time) {
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
    }

    pub fn should_apply(&self, datetime: NaiveDateTime) -> bool {
        // Check if the given datetime is after `apply_on`
        if datetime < self.apply_on {
            return false;
        }

        // Check if the process type is `Normal` or not set
        if let Some(process_type) = &self.process_type {
            match process_type {
                ProcessType::Normal => {}
                ProcessType::Fake => return false,
            }
        }

        // Check if the day of the week matches
        if let Some(week_days) = &self.week_days {
            let weekday = datetime.weekday();
            if !week_days.matches(weekday) {
                return false;
            }
        }

        // Check if the time is within the range of `start_time` and `stop_time`
        if let (Some(start_time), Some(stop_time)) = (self.start_time, self.stop_time) {
            let time = datetime.time();
            if time < start_time || time >= stop_time {
                return false;
            }
        }

        true
    }
}

//  ----------------

#[derive(Debug, Clone, Serialize)]
enum DaySelection {
    Days(Vec<chrono::Weekday>),
    #[serde(rename = "mon-fri")]
    Mon2Fri,
}

impl DaySelection {
    pub fn matches(&self, weekday: chrono::Weekday) -> bool {
        match self {
            DaySelection::Days(days) => days.contains(&weekday),
            DaySelection::Mon2Fri => {
                weekday.num_days_from_monday() >= chrono::Weekday::Mon.num_days_from_monday()
                    && weekday.num_days_from_monday() <= chrono::Weekday::Fri.num_days_from_monday()
            }
        }
    }
}

// Add a test to verify deserialization
#[cfg(test)]
mod tests {
    use super::*;
    use toml;

    #[test]
    fn test_toml_parsing() {
        let toml_data = r#"
            uid = "550e8400-e29b-41d4-a716-446655440000"
            file_format = "0"

            [[process]]
            id = "example_process"
            command = "echo 'Hello, World!' && sleep 10"
            apply_on = "2023-10-01T12:00:00"

            [[process]]
            id = "PRUEBA_C"
            command = "echo $(date)  '  bb'"
            apply_on = "2029-11-01T12:20:00"

            [[process]]
            id = "example_process"
            command = "echo 'Hello, World!' && sleep 10"
            apply_on = "2023-10-01T12:00:00"
            # week_days = ["mon", "wed", "thu", "fri"]     # optional  
            week_days = "mon-fri"   # optional  
            start_time = "09:00:00" # optional
            stop_time = "23:00:00"  # optional
            type = "normal"         # optional  normal/fake

            [[process]]
            id = "example_process"
            command = "echo 'Hello, World!' && sleep 10"
            apply_on = "2023-10-01T12:00:00"
            # week_days = ["mon", "wed", "thu", "fri"]     # optional  
            week_days = "mon-fri"   # optional  
            start_time = "09:00:00" # optional
            stop_time = "23:00:00"  # optional
            type = "normal"         # optional  normal/fake

        "#;

        // Parse the TOML data
        let parsed: Result<Config, _> = toml::from_str(toml_data);

        // Assert that parsing was successful, providing error details if it fails
        assert!(
            parsed.is_ok(),
            "Failed to parse TOML data:\nError: {}\nRaw Data: {:?}",
            parsed
                .as_ref()
                .err()
                .map(|e| e.to_string())
                .unwrap_or_default(),
            toml_data
        );

        // Verify the parsed data
        let config = parsed.unwrap();
        assert_eq!(config.process.as_ref().unwrap().len(), 4);
        assert_eq!(config.process.as_ref().unwrap()[0].id, "example_process");
        assert_eq!(config.process.as_ref().unwrap()[1].id, "PRUEBA_C");
    }
}

pub struct ActiveByDateProcConf(pub Vec<ProcessConfig>);

impl Config {
    pub fn act_proc_conf_by_date(self: &Self) -> ActiveByDateProcConf {
        let now = Local::now().naive_local();
        let mut process_map: HashMap<String, ProcessConfig> = HashMap::new();

        if let Some(processes) = &self.process {
            for process in processes {
                // Check if the process is valid
                if let Err(err) = process.is_valid_start_stop() {
                    eprintln!(
                        "Skipping process '{}' due to invalid configuration: {}",
                        process.id, err
                    );
                    continue;
                }

                // Check if the process should apply
                if !process.should_apply(now) {
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
        }

        ActiveByDateProcConf(process_map.into_values().collect())
    }
}

impl<'de> Deserialize<'de> for DaySelection {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct DaySelectionVisitor;

        impl<'de> serde::de::Visitor<'de> for DaySelectionVisitor {
            type Value = DaySelection;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a list of weekdays or the string 'mon-fri'")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                if v == "mon-fri" {
                    Ok(DaySelection::Mon2Fri)
                } else {
                    Err(E::custom(format!("unexpected string: {}", v)))
                }
            }

            fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                let days: Vec<chrono::Weekday> =
                    Deserialize::deserialize(serde::de::value::SeqAccessDeserializer::new(seq))?;
                Ok(DaySelection::Days(days))
            }
        }

        deserializer.deserialize_any(DaySelectionVisitor)
    }
}

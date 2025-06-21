use crate::config::Task;
use serde::Deserialize;

#[derive(Deserialize)]
struct TaskFile {
    tasks: Vec<Task>,
}

pub(crate) fn read_file(file_path: &str) -> Result<Vec<Task>, std::io::Error> {
    use std::fs::File;
    use std::io::Read;

    let mut file = File::open(file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let task_file: TaskFile = toml::from_str(&contents)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;

    Ok(task_file.tasks)
}

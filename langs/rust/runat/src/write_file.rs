use crate::config::Task;
use serde::Serialize;
use std::fs::File;
use std::io::Write;

#[derive(Serialize)]
struct TaskFile {
    tasks: Vec<Task>,
}

pub(crate) fn write_file(file_path: &str, tasks: &[Task]) -> Result<(), std::io::Error> {
    let task_file = TaskFile {
        tasks: tasks.to_vec(),
    };

    // Serializar a formato TOML
    let toml_string = toml::to_string(&task_file)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;

    // Escribir al archivo
    let mut file = File::create(file_path)?;
    file.write_all(toml_string.as_bytes())?;

    Ok(())
}

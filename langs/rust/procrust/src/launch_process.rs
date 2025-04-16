use crate::types::process_watched::{ProcessStatus, ProcessWatched};
use chrono::NaiveDateTime;
use std::fs::{self, File};
use std::io::Write;
use std::io::{BufRead, BufReader};
use std::path::Path;
use std::process::{Child, Command, Stdio};
use std::thread;

pub fn launch_process(command: &str, prefix: &str) -> std::io::Result<i32> {
    let mut child: Child = Command::new("sh")
        .arg("-c")
        .arg(&command)
        .env("PROCRUST", &command)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;
    // let mut child = Command::new(command)
    //     .args(args)
    //     .stdout(Stdio::piped())
    //     .stderr(Stdio::piped())
    //     .spawn()?;

    let stdout = child.stdout.take().expect("Failed to capture stdout");
    let stderr = child.stderr.take().expect("Failed to capture stderr");

    let prefix_out = prefix.to_string();
    let prefix_err = prefix.to_string();

    // Hilo para stdout
    let handle_out = thread::spawn(move || {
        let reader = BufReader::new(stdout);
        for line in reader.lines().flatten() {
            println!("[{}] {}", prefix_out, line);
        }
    });

    // Hilo para stderr
    let handle_err = thread::spawn(move || {
        let reader = BufReader::new(stderr);
        for line in reader.lines().flatten() {
            eprintln!("[{}][ERROR] {}", prefix_err, line);
        }
    });

    // Esperar a que termine el proceso
    let status = child.wait()?;

    // Esperar a que terminen los hilos de salida
    handle_out.join().unwrap();
    handle_err.join().unwrap();

    match status.code() {
        Some(result) => println!("finished with code {}", result),
        None => println!("not result?"),
    }

    Ok(status.code().unwrap_or(-1))
}

fn write_process_watched(
    path_persist_watched: &str,
    process_id: &str,
    pid: u32,
    cmd: &str,
    apply_on: NaiveDateTime,
) -> std::io::Result<()> {
    fs::create_dir_all(path_persist_watched)?;

    let file_name = format!("{}.toml", process_id);

    let file_path = format!("{}/{}", path_persist_watched, file_name);
    let process_watched = ProcessWatched {
        id: process_id.to_string(),
        pid,
        apply_on,
        procrust_uid: cmd.to_string(), // todo:0 mejor un uuid
        status: ProcessStatus::Running,
        applied_on: chrono::Local::now().naive_utc(),
    };

    // Serialize the structure to TOML
    let toml =
        toml::to_string(&process_watched).expect("Failed to serialize ProcessWatched to TOML");
    let mut file = File::create(Path::new(&file_path))?;
    file.write_all(toml.as_bytes())?;

    Ok(())
}

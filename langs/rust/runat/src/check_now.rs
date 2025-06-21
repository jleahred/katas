use std::process::Command;
use std::thread;
use std::time::Duration;

pub(crate) fn check_file(file_path: &str) {
    let mut tasks = match crate::read_file::read_file(file_path) {
        Ok(vec) => vec,
        Err(e) => {
            eprintln!("> Error reading file {}: {}", file_path, e);
            return;
        }
    };
    if tasks.is_empty() {
        // println!("No tasks found in file: {}", file_path);
        return;
    }
    process_tasks(&mut tasks);

    if let Err(e) = crate::write_file::write_file(file_path, &tasks) {
        eprintln!("> Error writing file {}: {}", file_path, e);
    }

    // println!("Checked file: {}", file_path);
    // println!("Total tasks found: {}", tasks.len());
    // if tasks.len() > 0 {
    //     println!("Tasks executed.");
    // } else {
    //     // println!("No tasks ready to be executed.");
    // }
    // println!("Check completed for file: {}", file_path);
}

fn process_tasks(tasks: &mut Vec<crate::config::Task>) {
    for task in tasks {
        if task.status == crate::config::Status::Pending
            || (task.status == crate::config::Status::Completed && task.daily == Some(true))
        {
            // println!(
            //     "Task: {}, Scheduled at: {}, Status: {:?}, Lines: {}",
            //     task.name, task.scheduled, task.status, task.commands
            // );
            let local_now = chrono::Local::now().naive_local();
            if task.scheduled > local_now {
                // println!(
                //     "Skipping task {} as it is scheduled for the future.",
                //     task.name
                // );
                continue;
            }

            println!(
                "\n>  {}  Executing commands for task: {}",
                chrono::Local::now().format("%Y-%m-%d %H:%M:%S"),
                task.name
            );

            // If the task is daily, update its schedule to one day later
            if task.daily == Some(true) {
                // Get the scheduled date and add one day
                let current_time = task.scheduled.time();
                let tomorrow = chrono::Local::now().date_naive() + chrono::Duration::days(1);
                task.scheduled = tomorrow.and_time(current_time);
                println!(
                    "> Daily task '{}' rescheduled for: {}",
                    task.name,
                    task.scheduled.format("%Y-%m-%d %H:%M:%S")
                );
            }

            task.executed = Some(local_now);
            let output = Command::new("sh")
                .arg("-c")
                .arg(format!("set -e; {}", task.commands.clone()))
                .stdout(std::process::Stdio::inherit())
                .stderr(std::process::Stdio::inherit())
                .output();

            match output {
                Ok(output) => {
                    if output.status.success() {
                        println!("> Command '{}' executed successfully.", task.name);
                        task.status = crate::config::Status::Completed;
                    } else {
                        eprintln!(
                            "> Task '{}' failed with error: {}",
                            task.name,
                            String::from_utf8_lossy(&output.stderr)
                        );
                        task.status = crate::config::Status::Failed;
                    }
                }
                Err(e) => {
                    eprintln!("> Failed to execute task '{}': {}", task.name, e);
                    task.status = crate::config::Status::Failed;
                }
            }

            thread::sleep(Duration::from_secs(1));
        } else {
            // println!(
            //     "Task: {}, Status: {:?} (not pending, skipping)",
            //     task.name, task.status
            // );
        }
    }
}

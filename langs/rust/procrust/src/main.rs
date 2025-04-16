mod launch_process;
mod one_shot;
mod read_config_file;
mod types;

use std::env;
use std::process::Command;
use std::time::Duration;

fn main() {
    let mut args = env::args();
    if args.any(|arg| arg == "--one-shot") {
        one_shot::one_shot();
        return;
    } else if let Some(process_id) = args
        .position(|arg| arg == "--supervise-process")
        .and_then(|_| args.next())
    {
        let _ = launch_process::launch_process("laa -la", &process_id);
        return;
    } else if args.any(|arg| arg == "--uid") {
        println!("{}", uuid::Uuid::new_v4().to_string());
        return;
    }

    run_in_loop()
}

fn run_in_loop() {
    loop {
        let current_exe = env::current_exe().expect("CRITIC: Can't get current executable path");

        // Create a new process
        let mut child = Command::new(current_exe)
            .args(["--one-shot"])
            .spawn()
            .expect("CRITIC: Can't spawn child process");

        // println!("New process created with PID: {}", child.id());
        if child.wait().is_err() {
            eprintln!("Error waiting for child process");
        } else {
            println!("Shot finished");
        }
        std::thread::sleep(Duration::from_secs(2));
    }
}

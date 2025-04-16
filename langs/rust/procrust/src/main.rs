mod launch_process;
mod one_shot;
mod read_config_file;
mod types;

use std::env;
use std::os::unix::process;
use std::process::Command;
use std::time::Duration;

use chrono::format;
use nix::libc::abort;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 2 && args[1] == "--one-shot" {
        one_shot::one_shot();
        return;
    } else if args.len() == 6 && args[1].starts_with("--supervise-process") {
        if let (Some(process_id), Some(config_uid), Some(apply_on), Some(command)) =
            (args.get(2), args.get(3), args.get(4), args.get(5))
        {
            // panic!("testing");
            let _ = launch_process::launch_process(process_id, config_uid, apply_on, &command);
            return;
        }
    } else if args.len() == 2 && args[1] == "--uid" {
        println!("{}", uuid::Uuid::new_v4().to_string());
        return;
    } else if args.len() == 1 {
        run_in_loop()
    } else {
        panic!("Invalid arguments {}", args.join(" || "));
    }
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
        std::thread::sleep(Duration::from_secs(1));
    }
}

pub fn run_supervised_process(
    process_id: &str,
    config_id: &str,
    apply_on: &str,
    command: &str,
) -> u32 {
    let current_exe = env::current_exe().expect("CRITIC: Can't get current executable path");

    // Create a new process
    let mut child = Command::new(current_exe)
        .args([
            "--supervise-process",
            process_id,
            config_id,
            apply_on,
            command,
        ])
        .env("PROCRUST", &command)
        .spawn()
        .expect("CRITIC: Can't spawn child process");

    // if child.wait().is_err() {
    //     eprintln!("Error waiting for child process");
    // } else {
    //     println!("Shot finished");
    // }

    std::thread::sleep(Duration::from_secs(1));

    child.id()
}

mod one_shot;
mod read_config_file;
mod types;
mod cli_params;

use std::env;
use std::process::Command;
use std::time::Duration;


fn main() {

    let cli_params = cli_params::parse();

    match cli_params.command {
        cli_params::Commands::Run { processes_filename } => {
            println!("Running: {}", processes_filename);
            // todo:
        }
        cli_params::Commands::Check { filename } => {
            println!("Checking: {}", filename);
            // todo:
        }
        cli_params::Commands::Uid => {
            println!("uid you can use on processes config file:   {}", uuid::Uuid::new_v4().to_string());
            return;
            }
    }

    let args: Vec<String> = env::args().collect();
    if args.len() == 2 && args[1] == "--one-shot" {
        one_shot::one_shot();
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
        let mut child =     
        //Command::new(current_exe)
        Command::new("setsid")
        .arg(current_exe)
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

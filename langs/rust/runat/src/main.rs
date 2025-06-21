mod check_now;
mod config;
mod read_file;
mod write_file;

use clap::{Parser, Subcommand};
use std::process::Command;
use std::thread;
use std::time::Duration;

#[derive(Parser)]
#[command(
    author,
    version,
    about = "A tool to watch a file and execute commands based on time",
    long_about = "Runat allows you to monitor files and execute commands at specified times."
)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Watch a file and execute commands when conditions are met
    Watch {
        /// The file to watch
        filename: String,
    },
    /// Check a file now and execute commands if conditions are met
    CheckNow {
        /// The file to check
        filename: String,
    },
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Watch { filename } => {
            println!("> Watching file: {}\n", filename);

            loop {
                // println!("Executing check-now for file: {}", filename);
                let current_exe =
                    std::env::current_exe().expect("Failed to get current executable path");

                let output = Command::new(current_exe)
                    .arg("check-now")
                    .arg(filename)
                    .stdout(std::process::Stdio::inherit())
                    .stderr(std::process::Stdio::inherit())
                    .output()
                    .expect("Failed to execute command");

                // // Print current date and time
                // if !output.stdout.is_empty()
                //     || !output.stderr.is_empty()
                //     || !output.status.success()
                // {
                //     let now = chrono::Local::now();
                //     println!("\n\n{}  ----------------", now.format("%Y-%m-%d %H:%M:%S"));
                // }

                // if !output.stdout.is_empty() {
                //     println!("{}", String::from_utf8_lossy(&output.stdout));
                // }

                // if !output.stderr.is_empty() {
                //     println!("ERROR: {}", String::from_utf8_lossy(&output.stderr));
                // }

                if !output.status.success() {
                    eprintln!(
                        "> Error executing check-now: {}",
                        String::from_utf8_lossy(&output.stderr)
                    );
                }

                thread::sleep(Duration::from_secs(10));
            }
        }
        Commands::CheckNow { filename } => {
            // println!("Checking file now: {}", filename);
            check_now::check_file(filename);
        }
    }
}

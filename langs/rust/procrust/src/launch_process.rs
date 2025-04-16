use std::io::{BufRead, BufReader};
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

// fn run_process(process: &ProcessConfig) -> Result<u32, io::Error> {
//     let mut child: Child = Command::new("sh")
//         .arg("-c")
//         .arg(&process.command)
//         .env("PROCRUST", &process.command)
//         .spawn()?;

//     thread::sleep(Duration::from_secs(2));

//     match child.try_wait()? {
//         Some(status) => {
//             if status.success() {
//                 eprintln!(
//                     "Running process, finished OK  ??  {} (apply_on: {})",
//                     process.id, process.apply_on
//                 );
//             } else {
//                 eprintln!(
//                     "Running process, finished with error  :-(  {} (apply_on: {})",
//                     process.id, process.apply_on
//                 );
//             }
//         }
//         None => {}
//     }

//     Ok(child.id())
// }

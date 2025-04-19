use super::super::RunningStatus;
use std::fs;
use std::io::Write;

pub fn save(run_status: &RunningStatus, file_path: &str) {
    fs::create_dir_all(&file_path).expect(&format!("Failed to create directory on {}", file_path));
    let full_path = format!("{}/{}.toml", file_path, run_status.uid);

    let toml_content = toml::to_string(&run_status)
        .unwrap_or_else(|err| panic!("Failed to serialize RunningStatus to TOML: {}", err));

    let mut file = fs::File::create(&full_path)
        .unwrap_or_else(|err| panic!("Failed to create file {}: {}", full_path, err));

    file.write_all(toml_content.as_bytes())
        .unwrap_or_else(|err| panic!("Failed to write to file {}: {}", full_path, err));

    println!("RunningStatus saved to {}", full_path);
}

use std::fs;
use std::path::PathBuf;

pub fn get_cmd_by_pid(pid: u32) -> Result<String, String> {
    let proc_path = PathBuf::from(format!("/proc/{}/cmdline", pid));
    if !proc_path.exists() {
        return Err(format!("PID {} does not exist", pid));
    }

    match fs::read_to_string(proc_path) {
        Ok(content) => {
            let cmd = content.replace('\0', " ").trim().to_string();
            if cmd.is_empty() {
                Err(format!("No command found for PID {}", pid))
            } else {
                Ok(cmd)
            }
        }
        Err(e) => Err(format!("Failed to read cmdline for PID {}: {}", pid, e)),
    }
}

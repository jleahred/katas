use std::fs;

pub fn is_process_running(pid: u32) -> bool {
    let path = format!("/proc/{}", pid);
    fs::metadata(path).is_ok()
}

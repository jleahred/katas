use std::path::PathBuf;

mod cpp_gen;
mod h_gen;

pub(crate) fn generate_cpp_files(
    fsm: &[crate::parser::Status],
    orig_path: &PathBuf,
) -> std::result::Result<(), String> {
    h_gen::generate_header_fsm_code_generated(fsm, orig_path).map_err(|e| e.to_string())?;
    cpp_gen::generate_cpp_fsm_code_generated(fsm, orig_path).map_err(|e| e.to_string())?;
    Ok(())
}

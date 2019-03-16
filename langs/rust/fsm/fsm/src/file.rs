use chrono::prelude::*;
use fomat_macros::fomat;
use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;

pub(crate) fn process(path: &PathBuf) -> std::result::Result<(), String> {
    let mut file = File::open(path).map_err(|e| format!("{}", e))?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .map_err(|e| format!("{}", e))?;

    let fsm = crate::parser::parse(&contents)?;
    generate_cpp_code(&fsm, &path)?;
    Ok(())
}

fn generate_cpp_code(
    fsm: &[crate::parser::Status],
    orig_path: &PathBuf,
) -> std::result::Result<(), String> {
    generate_base_fsm_code(fsm, orig_path).map_err(|e| format!("{}", e))?;
    Ok(())
}

fn get_dir_stem_name(orig_path: &PathBuf) -> Result<(String, String), String> {
    let to_string = |p: &Option<&str>| -> Result<String, String> {
        Ok(p.ok_or(format!(
            "cannot take original file name from {:?}",
            orig_path
        ))?
        .to_string())
    };

    let dir = {
        let mut dir = orig_path.clone();
        dir.pop();
        dir
    };

    Ok((
        to_string(&dir.to_str())?,
        to_string(&orig_path.file_stem().and_then(|fname| fname.to_str()))?,
    ))
}

//  -------------
//      cpp

fn generate_base_fsm_code(
    fsm: &[crate::parser::Status],
    orig_path: &PathBuf,
) -> Result<(), String> {
    let (dir, stem_name) = get_dir_stem_name(&orig_path)?;
    let mut f =
        File::create(format!("{}/base_{}_fsm.h", dir, stem_name)).map_err(|e| format!("{}", e))?;
    let template = fomat!(
        r#"//  generated automatically  "# (Local::now().format("%Y-%m-%d %H:%M:%S").to_string()) r#"

#ifndef FSM_"# (stem_name.to_uppercase()) r#"_BASE_H
#define FSM_"# (stem_name.to_uppercase()) r#"_BASE_H

#include <iostream>

namespace "# (stem_name) r#" {

enum class State { init, w_login, login, logout };
std::ostream &operator<<(std::ostream &os, State state);

//  forward declaration
//  define by hand on .h
struct rq_key;
struct rq_login;
struct rq_logout;
struct heartbeat;
struct timer;

class BaseFsm {
protected:
  BaseFsm() : state(State::init) {}
  State state;

public:
  void in(const rq_key &in);
  void in(const rq_login &in);
  void in(const rq_logout &in);
  void in(const heartbeat &in);
  void in(const timer &in);

protected:
  //    actions
  virtual void log_error(const rq_key &in) = 0;
  virtual void log_error(const rq_login &in) = 0;
  virtual void log_error(const rq_logout &in) = 0;
  virtual void log_error(const heartbeat &in) = 0;
  virtual void log_error(const timer &in) = 0;
  virtual void send_key(const rq_key &in) = 0;
  virtual void send_login(const rq_login &in) = 0;
  virtual void send_logout(const rq_logout &in) = 0;

  //    guards
  virtual bool valid(const rq_login &in) = 0;
};

//  override by hand on cpp
class Fsm : public BaseFsm {
public:
  Fsm() : BaseFsm() {}

protected:
  //    actions
  void log_error(const rq_key &in) override;
  void log_error(const rq_login &in) override;
  void log_error(const rq_logout &in) override;
  void log_error(const heartbeat &in) override;
  void log_error(const timer &in) override;
  void send_key(const rq_key &in) override;
  void send_login(const rq_login &in) override;
  void send_logout(const rq_logout &in) override;

  //    guards
  bool valid(const rq_login &in) override;
};

} // namespace login

#endif // FSM_LOGIN_BASE_H
"#
    );

    f.write_all(template.as_bytes())
        .map_err(|e| format!("{}", e))?;

    f.sync_all().map_err(|e| format!("{}", e))?;
    println!("{:#?}", &fsm);
    Ok(())
}

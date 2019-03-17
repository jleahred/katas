mod file;
mod parser;

extern crate chrono;
extern crate structopt;

use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "fsm",
    about = r#"
    Generate code from a simple fsm file
    At the moment, it will generate c++ code
    "#
)]
struct Opt {
    /// Generate all files regardless of change date
    #[structopt(short = "f", long = "force")]
    debug: bool,

    /// List of fsm files
    #[structopt(parse(from_os_str))]
    fsm_files: Vec<PathBuf>,
}

fn main() {
    let opt = Opt::from_args();

    if opt.fsm_files.is_empty() {
        eprintln!("No files provied!!! If doubt,  --help");
    } else {
        for f in opt.fsm_files {
            match file::process(&f) {
                Ok(()) => (),
                Err(e) => eprintln!("error processing file: {:?}\n\n{}", f, e),
            }
        }
    }
}

// fn main() {
//     let parsed = parser::parse(
//         r#"
//         [init]
//             rq_key                  -   send_key        ->  w_login
//             rq_login                -   log_err         ->  logout
//             rq_logout               -   log_err         ->  logout
//             heartbeat               -   log_err         ->  logout
//             timer                   -   log_err         ->  logout

//         [w_login]
//             rq_key                  -   log_err         ->  logout
//             rq_login    &   valid   -   send_login      ->  login
//             rq_login                -   log_err         ->  logout
//             rq_logout               -   log_err         ->  logout
//             heartbeat               -   log_err         ->  logout
//             timer                   -                   ->  logout

//         [login]
//             rq_key                  -   log_err         ->  logout
//             rq_login                -   log_err         ->  logout
//             rq_logout               -   send_logout     ->  logout
//             heartbeat               -   log_err         ->  logout
//             timer       &   on_time -                   ->  login
//             timer                   -                   ->  logout

//         [logout]
//             rq_key                  -   log_err         ->  logout
//             rq_login                -   log_err         ->  logout
//             rq_logout               -   logout          ->  logout
//             heartbeat               -   log_err         ->  logout
//             timer                   -   log_err         ->  logout
//     "#,
//     );

//     println!("parsed {:#?}", parsed);
// }

//  [
//     Status {
//         name: "init",
//         transitions: [
//             Transition {
//                 input: "rq_key",
//                 guard: None,
//                 action: Some(
//                     "send_key"
//                 ),
//                 new_status: "w_login"
//             },
//             Transition {
//                 input: "rq_login",
//                 guard: None,
//                 action: Some(
//                     "log_err"
//                 ),
//                 new_status: "logout"
//             },
//             Transition {
//                 input: "rq_logout",
//                 guard: None,
//                 action: Some(
//                     "log_err"
//                 ),
//                 new_status: "logout"
//             },
//             Transition {
//                 input: "heartbeat",
//                 guard: None,
//                 action: Some(
//                     "log_err"
//                 ),
//                 new_status: "logout"
//             },
//             Transition {
//                 input: "timer",
//                 guard: None,
//                 action: Some(
//                     "log_err"
//                 ),
//                 new_status: "logout"
//             }
//         ]
//     },
//     Status {
//         name: "w_login",
//         transitions: [
//             Transition {
//                 input: "rq_key",
//                 guard: None,
//                 action: Some(
//                     "log_err"
//                 ),
//                 new_status: "logout"
//             },
//             Transition {
//                 input: "rq_login",
//                 guard: Some(
//                     "valid"
//                 ),
//                 action: Some(
//                     "send_login"
//                 ),
//                 new_status: "login"
//             },
//             Transition {
//                 input: "rq_login",
//                 guard: None,
//                 action: Some(
//                     "log_err"
//                 ),
//                 new_status: "logout"
//             },
//             Transition {
//                 input: "rq_logout",
//                 guard: None,
//                 action: Some(
//                     "log_err"
//                 ),
//                 new_status: "logout"
//             },
//             Transition {
//                 input: "heartbeat",
//                 guard: None,
//                 action: Some(
//                     "log_err"
//                 ),
//                 new_status: "logout"
//             },
//             Transition {
//                 input: "timer",
//                 guard: None,
//                 action: None,
//                 new_status: "logout"
//             }
//         ]
//     },
//     Status {
//         name: "login",
//         transitions: [
//             Transition {
//                 input: "rq_key",
//                 guard: None,
//                 action: Some(
//                     "log_err"
//                 ),
//                 new_status: "logout"
//             },
//             Transition {
//                 input: "rq_login",
//                 guard: None,
//                 action: Some(
//                     "log_err"
//                 ),
//                 new_status: "logout"
//             },
//             Transition {
//                 input: "rq_logout",
//                 guard: None,
//                 action: Some(
//                     "send_logout"
//                 ),
//                 new_status: "logout"
//             },
//             Transition {
//                 input: "heartbeat",
//                 guard: None,
//                 action: Some(
//                     "log_err"
//                 ),
//                 new_status: "logout"
//             },
//             Transition {
//                 input: "timer",
//                 guard: Some(
//                     "on_time"
//                 ),
//                 action: None,
//                 new_status: "login"
//             },
//             Transition {
//                 input: "timer",
//                 guard: None,
//                 action: None,
//                 new_status: "logout"
//             }
//         ]
//     },
//     Status {
//         name: "logout",
//         transitions: [
//             Transition {
//                 input: "rq_key",
//                 guard: None,
//                 action: Some(
//                     "log_err"
//                 ),
//                 new_status: "logout"
//             },
//             Transition {
//                 input: "rq_login",
//                 guard: None,
//                 action: Some(
//                     "log_err"
//                 ),
//                 new_status: "logout"
//             },
//             Transition {
//                 input: "rq_logout",
//                 guard: None,
//                 action: Some(
//                     "logout"
//                 ),
//                 new_status: "logout"
//             },
//             Transition {
//                 input: "heartbeat",
//                 guard: None,
//                 action: Some(
//                     "log_err"
//                 ),
//                 new_status: "logout"
//             },
//             Transition {
//                 input: "timer",
//                 guard: None,
//                 action: Some(
//                     "log_err"
//                 ),
//                 new_status: "logout"
//             }
//         ]
//     }
// ]

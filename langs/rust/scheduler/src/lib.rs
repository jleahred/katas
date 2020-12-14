// (Lines like the one below ignore selected Clippy rules
//  - it's useful when you want to check your code with `cargo make verify`
// but some rules are too "annoying" or are not applicable for your case.)
#![allow(clippy::wildcard_imports)]
#![recursion_limit = "512"]
// #[macro_use]
extern crate stdweb;
#[macro_use]
extern crate rpds;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;

mod model;
mod scheduler;

use scheduler::generate_schedule_from_init_config;

use seed::{prelude::*, *};

static VERSION: &str = "0.7";

fn init_config() -> &'static str {
    r#"
---
# ----------  initial configuration   ---------
recipes_todo:
    - recipe_id: recipe1
      priority: Low
      # pending ends_before
    - recipe_id: recipe2
      priority: High
available_products:
    - product: 
        prod_id: product1
        valid_for: 15m
      available_at: 7m
    - product: 
        prod_id: product2
        valid_for: 15m
      available_at: 7m


# -------------- database documents for recipes  ------------------
recipes_db:
  recipe1:
    description: recipe1
    processes:
          proc1:
            description: proc1
            inputs:
              - product1
              - product2
            outputs:
              - prod_id: prod3
                valid_for: 15m
              - prod_id: prod4
                valid_for: 15m
            sequence:
              - do first1
              - do second1
            required_time: 10m
  recipe2:
    description: recipe1
    processes:
          proc2:
            description: proc2
            inputs:
              - product1
              - product2
            # pending  resources and chefs
            outputs:
              - prod_id: prod5
                valid_for: 15m
              - prod_id: prod6
                valid_for: 10m
            sequence:
              - do first2
              - do second2
            required_time: 5m
"#
}

fn process_config(cfg: &str) -> Result<String, String> {
    let init_config = serde_yaml::from_str(cfg).or_else(|e| Err(format!("{}", e)))?;
    let final_status =
        generate_schedule_from_init_config(&init_config).or_else(|e| Err(format!("{:?}", e)))?;

    // let (status, value) = generate_schedule(&init_status).or_else(|e| Err(format!("{:?}", e)))?;

    Ok(format!(
        "result:\n{}\n",
        serde_yaml::to_string(&final_status.final_status).or_else(|e| Err(format!("{}", e)))?
    ))
}

// ------ ------
//     Init
// ------ ------

// `init` describes what should happen when your app started.
fn init(_: Url, _: &mut impl Orders<Msg>) -> Model {
    Model {
        config: init_config().to_string(),
        result: "here the result".to_string(),
        err: "".to_string(),
        exec_counter: 0,
    }
}

// ------ ------
//     Model
// ------ ------

// `Model` describes our app state.
struct Model {
    config: String,
    result: String,
    exec_counter: i32,

    err: String,
}

// ------ ------
//    Update
// ------ ------

// (Remove the line below once any of your `Msg` variants doesn't implement `Copy`.)
#[derive(Clone)]
// `Msg` describes the different events you can modify state with.
enum Msg {
    Run,
    ModifEditor(String),
}

// `update` describes how to handle each `Msg`.
fn update(msg: Msg, model: &mut Model, _: &mut impl Orders<Msg>) {
    match msg {
        Msg::Run => {
            // let ed_txt = stdweb::js! {
            //     // var ed = document.getElementById("editor");
            //     // console.log("hello there");
            //     // console.log(ed.getValue());
            //     return getEditorText();
            // };
            // let ed_txt = match ed_txt {
            //     stdweb::Value::String(s) => s,
            //     _ => "error reading data from editor".to_string(),
            // };
            model.exec_counter += 1;
            match process_config(&model.config) {
                Ok(result) => {
                    model.result = result;
                    model.err = "".to_string()
                }
                Err(err) => {
                    model.err = err;
                    model.result = "".to_string()
                }
            }
        }
        Msg::ModifEditor(txt) => {
            model.config = txt.to_string();
            // model.err = "".to_string();
            // model.edit_cfg_rows = txt.lines().count() + 1;
        }
    }
}

// ------ ------
//     View
// ------ ------

// (Remove the line below once your `Model` become more complex.)
// #[allow(clippy::trivially_copy_pass_by_ref)]
// `view` describes what to display.
// fn view(model: &Model) -> Node<Msg> {
//     div![
//         C!["container h-100"],
//         button![C!["btn btn-primary"], "Run", ev(Ev::Click, |_| Msg::Run),],
//         div![
//             C!["row h-100"],
//             div![
//                 C!["col-md-6"],
//                 textarea![
//                     C!["form-control"],
//                     C!["text-monospace"],
//                     attrs![At::Rows => "20"],
//                     "initial value"
//                 ],
//             ],
//             div![pre![code![C!["col-md-6"], &model.text]]],
//         ]
//     ]
// }

fn view(model: &Model) -> Node<Msg> {
    div![
        div![
            C!["container h-100"],
            VERSION,
            " ",
            button![C!["btn btn-primary"], "Run", ev(Ev::Click, |_| Msg::Run),],
            " ",
            &model.exec_counter,
        ],
        textarea![id!("fake_editor"), input_ev(Ev::Input, Msg::ModifEditor),],
        div![
            id!("editor"),
            &model.config,
            // input_ev(Ev::Input, Msg::ModifEditor),
            // textarea![
            //     C!["form-control"],
            //     C!["text-monospace"],
            //     attrs![
            //         At::Id => "cfg",
            //         At::Rows => format!("{}", model.edit_cfg_rows),
            //         At::SpellCheck => false,
            //         At::Wrap => "break-word",
            //     ],
            //     input_ev(Ev::Input, Msg::ModifEditor),
            //     &model.config
            // ],
        ],
        div![id!("div_right"), &model.err, pre![&model.result]],
    ]
}

// ------ ------
//     Start
// ------ ------

// (This function is invoked by `init` function in `index.html`.)
#[wasm_bindgen(start)]
pub fn start() {
    // Mount the `app` to the element with the `id` "app".
    App::start("app", init, update, view);
}

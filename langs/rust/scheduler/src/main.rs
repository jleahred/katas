#[macro_use]
extern crate rpds;

mod model;
mod scheduler;

use model::*;
use rpds::{HashTrieSet, Vector};
// use scheduler::{generate_schedule, get_status_from_init_cfg, InitStatus};

fn main() {
    let init_config: model::InitConfig = serde_yaml::from_str(init_config_yaml()).unwrap();

    println!(
        "\ninitial config:\n{}",
        serde_yaml::to_string(&init_config).unwrap()
    );

    let result_status = scheduler::generate_schedule_from_init_config(&init_config).unwrap();
    let result_yaml = serde_yaml::to_string(&result_status).unwrap();

    println!("--------------------------------------");
    println!("Final status");
    println!("{}", result_yaml);
    // let status = get_status_from_init_cfg(&init_config);

    // let (status, value) = generate_schedule(&status).unwrap();

    // println!("\n\nVALUE {:?} \n", value);
    // println!(
    //     "++++++++++++++++++\nlast_status:\nstatus:{}\n",
    //     serde_yaml::to_string(&status.dynamic_data).unwrap()
    // );
}

fn init_config_yaml() -> &'static str {
    r#"
---
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
              - do first
              - do second
            required_time: 10m
  recipe2:
    description: recipe1
    processes:
          proc1111:
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
              - do first
              - do second
            required_time: 10m

# ----------  initial configuration   ---------
recipes_todo:
    - recipe_id: recipe1
      priority: High
    - recipe_id: recipe2
      priority: Low
available_products:
    - product: 
        prod_id: product1
        valid_for: 15m
      available_at: 7m
    - product: 
        prod_id: product2
        valid_for: 15m
      available_at: 7m
"#
}

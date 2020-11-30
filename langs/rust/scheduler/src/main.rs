#[macro_use]
extern crate rpds;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;

mod scheduler;

use scheduler::{generate_schedule, get_status_from_init_cfg, InitStatus};

fn main() {
    let init_config: InitStatus = serde_yaml::from_str(init_config_yaml()).unwrap();

    println!(
        "\ninitial config:\n{}",
        serde_yaml::to_string(&init_config).unwrap()
    );

    let init_status = get_status_from_init_cfg(&init_config);

    let status = init_status;

    let (status, value) = generate_schedule(&status).unwrap();
    // println!(
    //     ">>>>>>>>>>>>>>>>>>>\nprocessed:\nvalue:{}\nexecs:\n{}",
    //     value,
    //     serde_yaml::to_string(&status.dynamic_data.executions).unwrap()
    // );

    println!("\n\nVALUE {:?} \n", value);
    println!(
        "++++++++++++++++++\nlast_status:\nstatus:{}\n",
        serde_yaml::to_string(&status.dynamic_data).unwrap()
    );
}

fn init_config_yaml() -> &'static str {
    r#"
---
tasks:
    t1:
      description: task 1
      start_after: 2m
      priority: Mandatory
      process:
          p1:
            description: p1
            inputs:
                - prod_i
            outputs:
                - id: prod1
                  description: result product
                  max_waitting: 1m
            required_time: 3m
            sequence:
                - description: action t1.1
                - description: action t1.2
          p2:
            description: p2
            inputs:
                - prod_i2
            outputs:
                - id: prod2
                  description: result product
                  max_waitting: 15m
            required_time: 20m
            sequence:
                - description: act p2
          p3:
            description: p3
            inputs:
                - prod2
                - prod1
            outputs:
                - id: p2
                  description: result product
                  max_waitting: 15m
            required_time: 20m
            sequence:
                - description: act p3
products:
    - id: prod_i
      description: initial product
      max_waitting: 15m
    - id: prod_i2
      description: initial product
      max_waitting: 15m
"#
}

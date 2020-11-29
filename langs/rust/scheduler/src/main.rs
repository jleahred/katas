#[macro_use]
extern crate rpds;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;

mod scheduler;

use scheduler::{get_status_from_init_cfg, process, InitStatus};

fn main() {
    let init_config: InitStatus = serde_yaml::from_str(init_config_yaml()).unwrap();

    println!(
        "\ninitial config:\n{}",
        serde_yaml::to_string(&init_config).unwrap()
    );

    let init_status = get_status_from_init_cfg(&init_config);

    let status = init_status;

    let (status, value) = process(&status).unwrap();
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
    start_after: 0m
    priority: Mandatory
    process:
      - description: pr1
        inputs:
          - prod_i
        outputs:
          - id: prod1
            description: prod1
            max_waitting: 15m
        required_time: 3m
        sequence:
            - description: proc1
      - description: pr2
        inputs:
          - prod1
        outputs:
          - id: prod3
            description: prod2
            max_waitting: 15m
        required_time: 3m
        sequence:
            - description: t2
products:
  - id: prod_i
    description: initial product
    max_waitting: 15m
"#
}

#[macro_use]
extern crate rpds;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;

mod scheduler;

use scheduler::{get_status_from_init_cfg, rec_process_pending_tasks, InitStatus};

fn main() {
    let init_config_yaml = "
---
tasks:
  t1:
    description: task 1
    start_after: 2m
    priority: Mandatory
    process:
      - description: process 1
        inputs:
          - prod_i
        outputs:
          - id: prod_t1
            description: result product
            max_waitting: 15m
        actions:
            - description: action t1.1
              required_time: 3m
  t2:
    description: task 2
    start_after: 21m
    priority: High
    process:
      - description: process 1
        inputs:
          - prod_t1
        outputs:
          - id: prod_r1
            description: result product t2
            max_waitting: 15s
          - id: prod_r12
            description: result product t22
            max_waitting: 5h
        actions:
            - description: action t2.1
              required_time: 6m
products:
  - id: prod_i
    description: initial product
    max_waitting: 15m
";

    let init_config: InitStatus = serde_yaml::from_str(init_config_yaml).unwrap();

    println!(
        "\ninitial config:\n{}",
        serde_yaml::to_string(&init_config).unwrap()
    );

    let init_status = get_status_from_init_cfg(&init_config);

    let status = init_status;

    let (status, value) = rec_process_pending_tasks(&status).unwrap();
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

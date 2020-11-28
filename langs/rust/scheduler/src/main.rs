#[macro_use]
extern crate rpds;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;

mod scheduler;

use scheduler::{get_status_from_init_cfg, rec_process_pending_tasks, InitStatus};

fn main() {
    let init_config_yaml = r#"
---
# after # is a comment, like this
tasks: # list of tasks
  t1:  # here the task id
    description: task 1
    start_after: 2m  # 2m means 2 minutes, for seconds, 2s. hours...
    priority: Mandatory  # priority could be Mandatory, High, Medium, Low
    process: # List of process to execute on this task
      - description: process 1
        inputs: # products necesary to execute this proces
          - prod_i
        outputs:  # list of products produced by this process
          - id: prod_t1
            description: result product
            max_waitting: 15m
        required_time: 3m
        sequence: # description of steps required on this process
            - description: action t1.1
            - description: action t1.2
  t2: # starting the description of a new task with id t2
    description: task 2
    start_after: 20m
    priority: High
    process:
      - description: process 2
        inputs:
          - prod_t1
        outputs:
          - id: prod_r1
            description: result product t2
            max_waitting: 15s
          - id: prod_r12
            description: result product t22
            max_waitting: 5h
        required_time: 6m
        sequence:
            - description: action t2.1
            - description: action t2.2
            - description: action t2.3
products:  # here the initial products
  - id: prod_i
    description: initial product
    max_waitting: 15m
"#;

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

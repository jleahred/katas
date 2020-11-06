#[macro_use]
extern crate rpds;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;

use rpds::Vector;
use serde::{Deserialize, Serialize};
use std::time::Duration;

#[derive(Debug, Serialize, Deserialize, Clone)]
struct TaskId(String);
#[derive(Debug, Serialize, Deserialize, Clone)]
struct ProdId(String);

#[derive(Debug, Serialize, Deserialize)]
struct Status {
    pending_tasks: Vector<TaskId>,
    available_products: Vector<ProdId>,
    executions: Vector<Execution>,
}

#[derive(Debug, Serialize, Deserialize)]
struct Execution {
    #[serde(with = "humantime_serde")]
    start_at: Duration,
    #[serde(with = "humantime_serde")]
    duration: Duration,
    task_desc: String,
    process_desc: String,
    action_desc: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct InitStatus {
    tasks: Vector<Task>,
    products: Vector<Product>,
}

#[derive(Debug, Serialize, Deserialize)]
struct Task {
    id: TaskId,
    description: String,
    #[serde(with = "humantime_serde")]
    start_after: Duration,
    #[serde(with = "humantime_serde")]
    ends_before: Duration,
    priority: Priority,
    process: Vec<Process>,
}

#[derive(Debug, Serialize, Deserialize)]
struct Product {
    id: ProdId,
    description: String,
    #[serde(with = "humantime_serde")]
    max_waitting: Duration,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
enum Priority {
    Mandatory,
    High,
    Medium,
    Low,
}

#[derive(Debug, Serialize, Deserialize)]
struct Process {
    description: String,
    inputs: Vec<ProdId>,
    outputs: Vec<Product>,
    actions: Vec<Action>,
}

#[derive(Debug, Serialize, Deserialize)]
struct Action {
    description: String,
    #[serde(with = "humantime_serde")]
    min_time: Duration,
    #[serde(with = "humantime_serde")]
    max_time: Duration,
}

fn main() {
    let prodi = Product {
        id: ProdId("prod_i".to_string()),
        description: "initial product".to_string(),
        max_waitting: Duration::from_secs(15),
    };
    let prodr = Product {
        id: ProdId("prod_r".to_string()),
        description: "result product".to_string(),
        max_waitting: Duration::from_secs(15),
    };
    let action = Action {
        description: "action 1".to_string(),
        min_time: Duration::from_secs(15),
        max_time: minutes(3),
    };
    let proc1 = Process {
        description: "process 1".to_string(),
        inputs: vec![prodi.id.clone()],
        outputs: vec![prodr],
        actions: vec![action],
    };
    let t1 = Task {
        id: TaskId("t1".to_string()),
        description: "task 1".to_string(),
        start_after: minutes(2),
        ends_before: minutes(15),
        priority: Priority::Mandatory,
        process: vec![proc1],
    };

    let init_config = InitStatus {
        tasks: vector![t1],
        products: vector![prodi],
    };

    println!("INIT:\n{}", serde_yaml::to_string(&init_config).unwrap());

    let init_status = Status {
        pending_tasks: init_config
            .tasks
            .iter()
            .fold(vector![], |acc, t| acc.push_back(t.id.clone())),
        available_products: init_config
            .products
            .iter()
            .fold(vector![], |acc, p| acc.push_back(p.id.clone())),
        executions: vector![],
    };

    println!(
        "\ninitial status:\n{}",
        serde_yaml::to_string(&init_status).unwrap()
    );
}

fn minutes(min: u64) -> Duration {
    Duration::from_secs(min * 60)
}

//  algorithm
//
// repeat for a while
//     repeat as much as possible
//         take a random task
//         process it with random actions
//     value the result
//     if better than previous solution
//         keep it as better

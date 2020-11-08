#[macro_use]
extern crate rpds;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;
// #[macro_use]
// extern crate time;

use rpds::{HashTrieSet, Vector};
use serde::{Deserialize, Serialize};
// use std::time::{Duration, Instant};
use std::time::Duration;
// use time::Duration;

#[derive(Debug)]
struct InternalError(String);

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
struct TaskId(String);
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
struct ProdId(String);

#[derive(Debug, Serialize, Deserialize, Clone)]
struct ValidTill(#[serde(with = "humantime_serde")] Option<Duration>);

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
    tasks: rpds::HashTrieMap<TaskId, Task>,
    products: Vector<Product>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct Task {
    description: String,
    #[serde(with = "humantime_serde")]
    start_after: Duration,
    // #[serde(with = "humantime_serde")]
    // ends_before: Duration,
    priority: Priority,
    process: Vector<Process>,
}

impl Task {
    fn get_input_prods(&self) -> Vector<ProdId> {
        self.process.iter().fold(vector![], |acc, p| {
            p.inputs.iter().fold(acc, |acc, i| acc.push_back(i.clone()))
        })
    }
    fn get_output_prods_time(&self) -> Result<Vector<(Product, Duration)>, InternalError> {
        let get_full_time_process = |p: &Process| {
            let max_time_action = p
                .actions
                .iter()
                .max_by(|a, b| a.required_time.cmp(&b.required_time));
            match max_time_action {
                Some(action) => Ok(action.required_time),
                None => Err(InternalError(format!(
                    "error on get_output_prods_time, no actions {:?}",
                    self
                ))),
            }
        };
        self.process.iter().fold(Ok(vector![]), |acc, p| {
            p.outputs
                .iter()
                .fold(acc, move |acc, prd| match (acc, get_full_time_process(p)) {
                    (Ok(acc), Ok(ftp)) => Ok(acc.push_back((prd.clone(), ftp))),
                    (Ok(_), Err(err)) => Err(err),
                    (Err(err), _) => Err(err),
                })
        })
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct Product {
    id: ProdId,
    description: String,
    #[serde(with = "humantime_serde")]
    max_waitting: Duration,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
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
    required_time: Duration,
}

fn main() {
    let prodi = Product {
        id: ProdId("prod_i".to_string()),
        description: "initial product".to_string(),
        max_waitting: minutes(15),
    };
    let prodr = Product {
        id: ProdId("prod_r".to_string()),
        description: "result product".to_string(),
        max_waitting: Duration::from_secs(15),
    };
    let action = Action {
        description: "action 1".to_string(),
        required_time: minutes(3),
    };
    let proc1 = Process {
        description: "process 1".to_string(),
        inputs: vec![prodi.id.clone()],
        outputs: vec![prodr],
        actions: vec![action],
    };
    let t1 = Task {
        description: "task 1".to_string(),
        start_after: minutes(2),
        // ends_before: minutes(15),
        priority: Priority::Mandatory,
        process: vector![proc1],
    };

    let init_config = InitStatus {
        tasks: rpds::ht_map![TaskId("t1".to_string()) => t1],
        products: vector![prodi],
    };

    println!("INIT:\n{}", serde_yaml::to_string(&init_config).unwrap());

    let init_status_dyn = StatusDynamicData {
        pending_tasks: init_config
            .tasks
            .iter()
            .fold(HashTrieSet::new(), |acc, t| acc.insert(t.0.clone())),
        available_products: init_config.products.iter().fold(ht_map![], |acc, p| {
            acc.insert(
                p.id.clone(),
                AvailableProduct {
                    prod: p.clone(),
                    created_on: Duration::from_secs(0),
                },
            )
        }),
        executions: vector![],
    };
    let init_status = Status {
        dynamic_data: init_status_dyn,
        static_data: StatusStaticData {
            tasks: init_config.tasks,
        },
    };

    println!(
        "\ninitial dyn status:\n{}",
        serde_yaml::to_string(&init_status.dynamic_data).unwrap()
    );

    let status = init_status;

    let status = status.process_task(&TaskId("t1".to_string())).unwrap();

    println!(
        "\nprocess task 1:\n{}",
        serde_yaml::to_string(&status.dynamic_data).unwrap()
    );
}

fn minutes(min: u64) -> Duration {
    Duration::from_secs(min * 60)
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct Status {
    static_data: StatusStaticData,
    dynamic_data: StatusDynamicData,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct StatusStaticData {
    tasks: rpds::HashTrieMap<TaskId, Task>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct AvailableProduct {
    prod: Product,
    #[serde(with = "humantime_serde")]
    created_on: Duration,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct StatusDynamicData {
    pending_tasks: HashTrieSet<TaskId>,
    available_products: rpds::HashTrieMap<ProdId, AvailableProduct>,
    executions: Vector<Execution>,
}

impl Status {
    fn get_task(&self, tid: &TaskId) -> Result<&Task, InternalError> {
        match self.static_data.tasks.get(tid) {
            Some(t) => Ok(t),
            None => Err(InternalError(format!("task {:?} not found", tid))),
        }
    }

    fn remove_pending_task(mut self, tid: &TaskId) -> Result<Status, InternalError> {
        if self.dynamic_data.pending_tasks.remove_mut(tid) {
            Ok(self)
        } else {
            Err(InternalError(format!("missing task {:?}", tid)))
        }
    }

    fn remove_pending_products(
        mut self,
        prods: &Vector<ProdId>,
        created_task: &Duration,
    ) -> Result<Status, InternalError> {
        let force_remove = |map_prod: rpds::HashTrieMap<ProdId, AvailableProduct>,
                            prod: &ProdId,
                            created_task: &Duration| {
            match map_prod.get(prod) {
                None => Err(InternalError(format!(
                    "error removing pending products with {:?}",
                    prod
                ))),
                Some(p) => {
                    if p.created_on > *created_task {
                        Err(InternalError(format!(
                            "error removing pending products (not created product) with {:?}",
                            prod
                        )))
                    } else if p.created_on + p.prod.max_waitting < *created_task {
                        Err(InternalError(format!(
                            "error removing pending products (caducated) with {:?} created_task: {:?}", 
                            prod, created_task
                        )))
                    } else {
                        Ok(map_prod.remove(prod))
                    }
                }
            }
        };

        let new_prods = prods
            .iter()
            .try_fold(self.dynamic_data.available_products, |acc, pr| {
                force_remove(acc, pr, created_task)
            })?;
        self.dynamic_data.available_products = new_prods;
        Ok(self)
    }

    fn add_available_products(mut self, task: &Task) -> Result<Status, InternalError> {
        let prds_time_2add = task.get_output_prods_time()?;
        let new_prods = prds_time_2add.iter().fold(
            self.dynamic_data.available_products,
            |acc, (pr, on_time)| {
                acc.insert(
                    pr.id.clone(),
                    AvailableProduct {
                        prod: pr.clone(),
                        created_on: task.start_after + *on_time,
                    },
                )
            },
        );
        self.dynamic_data.available_products = new_prods;
        Ok(self)
    }

    fn process_task(self, tid: &TaskId) -> Result<Self, InternalError> {
        let task = self.get_task(tid)?;

        let st = self.clone();
        let st = st
            .remove_pending_task(tid)?
            .remove_pending_products(&task.get_input_prods(), &task.start_after)?
            .add_available_products(&task)?;
        //  register actions on executions

        Ok(st)
    }
}

//  algorithm
//  repeat for a while
//      suffle pending tasks
//      process_pending_tasks |> ponderate
//      if better => save as better

//  agorithm  process_pending_tasks
//  get procesable tasks
//  if empty ptask
//      return execution
//  for task in ptask
//      proces(task)
//      process_pending_tasks

//  ponderate execution
//  if mandatory tasks pending => None
//  ponderate by priority  hight = 3 * normal, normal = 3 * low

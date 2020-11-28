use rpds::{HashTrieSet, Vector};
use serde::{Deserialize, Serialize};
use std::time::Duration;

pub(crate) fn get_status_from_init_cfg(init_config: &InitStatus) -> Status {
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
        executions: Executions {
            execs: vector![],
            log: vector![],
        },
    };
    Status {
        dynamic_data: init_status_dyn,
        static_data: StatusStaticData {
            tasks: init_config.tasks.clone(),
        },
    }
}

macro_rules! ierr {
    ($($arg:tt)*) => {{
        Err(InternalError(format!($($arg)*)))
    }}
}

#[derive(Debug)]
pub(crate) struct InternalError(String);

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
pub(crate) struct TaskId(String);
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
struct ProdId(String);

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct ValidTill(#[serde(with = "humantime_serde")] Option<Duration>);

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct Execution {
    #[serde(with = "humantime_serde")]
    start_at: Duration,
    #[serde(with = "humantime_serde")]
    duration: Duration,
    // task_desc: String,
    // process_desc: String,
    action_desc: String,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct Executions {
    execs: Vector<Execution>,
    log: Vector<String>,
}

impl Executions {
    fn push_action(mut self, start_at: Duration, a: &Action) -> Self {
        self.execs = self.execs.push_back(Execution {
            start_at,
            duration: a.required_time,
            action_desc: a.description.to_string(),
        });
        self
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct InitStatus {
    tasks: rpds::HashTrieMap<TaskId, Task>,
    products: Vector<Product>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct Task {
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
        Ok(self.process.iter().fold(vector![], |acc, p| {
            p.outputs.iter().fold(acc, move |acc, prd| {
                acc.push_back((prd.clone(), p.required_time))
            })
        }))
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
    required_time: Duration,
}

#[derive(Debug, Serialize, Deserialize)]
struct Action {
    description: String,
    #[serde(with = "humantime_serde")]
    required_time: Duration,
}

// fn minutes(min: u64) -> Duration {
//     Duration::from_secs(min * 60)
// }

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct Status {
    pub(crate) static_data: StatusStaticData,
    pub(crate) dynamic_data: StatusDynamicData,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct StatusStaticData {
    pub(crate) tasks: rpds::HashTrieMap<TaskId, Task>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct AvailableProduct {
    prod: Product,
    #[serde(with = "humantime_serde")]
    created_on: Duration,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct StatusDynamicData {
    pending_tasks: HashTrieSet<TaskId>,
    available_products: rpds::HashTrieMap<ProdId, AvailableProduct>,
    executions: Executions,
}

impl Status {
    fn get_task(&self, tid: &TaskId) -> Result<&Task, InternalError> {
        match self.static_data.tasks.get(tid) {
            Some(t) => Ok(t),
            None => ierr!("task {:?} not found", tid),
        }
    }

    fn remove_from_pending_task(mut self, tid: &TaskId) -> Result<Status, InternalError> {
        if self.dynamic_data.pending_tasks.remove_mut(tid) {
            Ok(self)
        } else {
            ierr!("missing task {:?}", tid)
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
                None => ierr!("error removing pending products with {:?}", prod),
                Some(p) => {
                    if p.created_on > *created_task {
                        ierr!("removing pending products (not created) with {:?}", prod)
                    } else if p.created_on + p.prod.max_waitting < *created_task {
                        ierr!("error removing pending products (caducated) with {:?} created_task: {:?}", 
                                prod, created_task)
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

    fn register_actions_on_execs(mut self, task: &Task) -> Result<Status, InternalError> {
        let prcss = &task.process;
        let execs = self.dynamic_data.executions;
        let execs = prcss.iter().fold(execs, |execs, p| {
            p.actions
                .iter()
                .fold(execs, |execs, a| execs.push_action(task.start_after, a))
        });
        self.dynamic_data.executions = execs;
        Ok(self)
    }

    fn add_log(mut self, text: &str) -> Self {
        self.dynamic_data
            .executions
            .log
            .push_back_mut(text.to_string());
        self
    }

    fn process_task(self, tid: &TaskId) -> Result<Self, InternalError> {
        let task = self.get_task(tid)?;

        let st = self.clone();
        let st = st
            .remove_from_pending_task(tid)?
            .remove_pending_products(&task.get_input_prods(), &task.start_after)?
            .add_available_products(&task)?
            .register_actions_on_execs(&task)?
            .add_log(&format!("{:?} exec task {}", task.start_after, tid.0));
        Ok(st)
    }

    fn can_execute_task(&self, tid: &TaskId) -> Result<bool, InternalError> {
        let task = self.get_task(tid)?;
        let ti_avail_prods = get_time_avail_all_prods(&self.dynamic_data, &task.get_input_prods());
        match ti_avail_prods {
            Some(tiap) => Ok(task.start_after >= tiap.0 && task.start_after <= tiap.1),
            None => Ok(false),
        }
    }

    fn get_ready2process_taskid(&self) -> Result<Vector<TaskId>, InternalError> {
        let mut result = vector![];
        for pt in self.dynamic_data.pending_tasks.iter() {
            if self.can_execute_task(pt)? {
                result.push_back_mut(pt.clone());
            }
        }
        Ok(result)
    }
}

fn get_time_avail_all_prods(
    dyndata: &StatusDynamicData,
    inprodis: &Vector<ProdId>,
) -> Option<(Duration, Duration)> {
    let mut result: Option<(Duration, Duration)> = None;
    for prdid in inprodis.iter() {
        let start_maxwaitting = get_start_max_waitting(dyndata, prdid);
        match (result, start_maxwaitting) {
            (Some(r), Some((start, max_waitting))) => {
                result = Some((
                    std::cmp::max(start, r.0),
                    std::cmp::min(start + max_waitting, r.1),
                ));
            }
            (None, Some((start, maxwaitting))) => result = Some((start, start + maxwaitting)),
            (_, None) => (),
        }
    }
    result
}

fn get_start_max_waitting(
    dyndata: &StatusDynamicData,
    prdid: &ProdId,
) -> Option<(Duration, Duration)> {
    dyndata
        .available_products
        .get(prdid)
        .and_then(|ap| Some((ap.created_on, ap.prod.max_waitting)))
}

pub(crate) fn rec_process_pending_tasks(
    st: &Status,
) -> Result<(Status, Option<i32>), InternalError> {
    let get_better = |r0: (_, Option<i32>), r1: (_, Option<i32>)| match (r0, r1) {
        ((st0, Some(v0)), (st1, Some(v1))) => {
            if v0 > v1 {
                (st0, Some(v0))
            } else {
                (st1, Some(v1))
            }
        }
        ((_st0, None), (st1, Some(v1))) => (st1, Some(v1)),
        ((st0, Some(v0)), (_st1, None)) => (st0, Some(v0)),
        ((st0, None), (_st1, None)) => (st0, None),
    };

    let mut st = st.clone();

    let mut result = (st.clone(), None);
    let taskid_ready2process = st.get_ready2process_taskid()?;
    if taskid_ready2process.is_empty() {
        Ok((st.clone(), ponderate_solution(&st)?))
    } else {
        for tid in taskid_ready2process.iter() {
            st = st.process_task(tid)?;
            result = get_better(result, rec_process_pending_tasks(&st)?);
        }
        Ok(result)
    }
}

fn ponderate_solution(st: &Status) -> Result<Option<i32>, InternalError> {
    let mut result = 0i32;
    for ptid in st.dynamic_data.pending_tasks.iter() {
        let task = st.get_task(ptid)?;
        match task.priority {
            Priority::Mandatory => return Ok(None),
            Priority::High => result += 100,
            Priority::Medium => result += 10,
            Priority::Low => result += 1,
        }
    }
    Ok(Some(-result))
}

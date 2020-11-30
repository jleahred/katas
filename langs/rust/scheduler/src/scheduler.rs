use rpds::{HashTrieSet, Vector};
use serde::{Deserialize, Serialize};
use std::time::Duration;

pub(crate) fn get_status_from_init_cfg(init_config: &InitStatus) -> Status {
    let init_status_dyn = StatusDynamicData {
        pending_processes: init_config.tasks.iter().fold(
            HashTrieSet::new(),
            |acc, (task_id, task)| {
                task.process.0.iter().fold(acc, |acc, p| {
                    acc.insert(ProcessIdInTaskId {
                        task_id: task_id.clone(),
                        process_id: p.0.clone(),
                    })
                })
            },
        ),
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
pub(crate) struct ProcessId(String);

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ProcessIdInTaskId {
    task_id: TaskId,
    process_id: ProcessId,
}

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
    process_desc: String,
    sequence: Vector<Action>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct Executions {
    execs: Vector<Execution>,
    log: Vector<String>,
}

impl Executions {
    fn push(
        mut self,
        start_at: Duration,
        duration: Duration,
        s: &str,
        sequence: &Vector<Action>,
    ) -> Self {
        self.execs = self.execs.push_back(Execution {
            start_at,
            duration,
            process_desc: s.to_string(),
            sequence: sequence.clone(),
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
struct Processes(rpds::HashTrieMap<ProcessId, Process>);
#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct Task {
    description: String,
    // #[serde(with = "humantime_serde")]
    // start_after: Duration,
    priority: Priority,
    process: Processes,
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
    inputs: Vector<ProdId>,
    outputs: Vector<Product>,
    sequence: Vector<Action>,
    #[serde(with = "humantime_serde")]
    required_time: Duration,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct Action {
    description: String,
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
    pending_processes: rpds::HashTrieSet<ProcessIdInTaskId>,
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

    fn get_process(&self, pt: &ProcessIdInTaskId) -> Result<&Process, InternalError> {
        let task = self.get_task(&pt.task_id)?;
        match task.process.0.get(&pt.process_id) {
            Some(t) => Ok(t),
            None => ierr!("task {:?} not found", pt.task_id),
        }
    }

    fn remove_from_pending_processes(
        mut self,
        process_in_task: &ProcessIdInTaskId,
    ) -> Result<Status, InternalError> {
        if self
            .dynamic_data
            .pending_processes
            .remove_mut(&process_in_task)
        {
            Ok(self)
        } else {
            ierr!("missing process {:?}", process_in_task)
        }
    }

    fn remove_pending_products(mut self, prods: &Vector<ProdId>) -> Result<Status, InternalError> {
        let new_prods = prods
            .iter()
            .fold(self.dynamic_data.available_products, |acc, pid| {
                acc.remove(pid)
            });
        self.dynamic_data.available_products = new_prods;
        Ok(self)
    }

    fn add_available_products(
        mut self,
        prods: &Vector<Product>,
        created_on: &Duration,
    ) -> Result<Status, InternalError> {
        self.dynamic_data.available_products =
            prods
                .iter()
                .fold(self.dynamic_data.available_products, |acc, prod| {
                    acc.insert(
                        prod.id.clone(),
                        AvailableProduct {
                            prod: prod.clone(),
                            created_on: *created_on,
                        },
                    )
                });
        Ok(self)
    }

    fn register_process_on_execs(
        mut self,
        p: &Process,
        start_at: &Duration,
    ) -> Result<Status, InternalError> {
        self.dynamic_data.executions = self.dynamic_data.executions.push(
            start_at.clone(),
            p.required_time,
            &p.description,
            &p.sequence,
        );
        Ok(self)
    }

    fn add_log(mut self, text: &str) -> Self {
        self.dynamic_data
            .executions
            .log
            .push_back_mut(text.to_string());
        self
    }

    fn run_process(
        self,
        pidtid: &ProcessIdInTaskId,
        run_at: &Duration,
    ) -> Result<Self, InternalError> {
        let process = self.get_process(&pidtid)?;

        let st = self.clone();
        let ends_at = &(*run_at + process.required_time);
        let st = st
            .remove_from_pending_processes(pidtid)?
            .remove_pending_products(&process.inputs)? //  todo: pending fail!!!
            .add_available_products(&process.outputs, &ends_at)?
            .register_process_on_execs(&process, &run_at)?
            .add_log(&format!("{:?} exec prod intask {:?}", run_at, pidtid));
        Ok(st)
    }

    fn can_execute_process(&self, process: &Process) -> Result<Option<Duration>, InternalError> {
        let ti_avail_prods = get_time_avail_all_prods(&self.dynamic_data, &process.inputs);
        dbg!(&ti_avail_prods);
        match ti_avail_prods {
            Some(tiap) => Ok(Some(tiap.0)), //  TODO:!!!
            None => Ok(None),
        }
    }

    fn get_one_random_ready2process(
        &self,
    ) -> Result<Option<(ProcessIdInTaskId, Duration)>, InternalError> {
        use rand::Rng;
        let mut rng = rand::thread_rng();

        let mut available = vector![];

        for ptintask in self.dynamic_data.pending_processes.iter() {
            let process = self.get_process(&ptintask)?;
            if let Some(start_at) = self.can_execute_process(process)? {
                available.push_back_mut((ptintask.clone(), start_at));
            }
        }
        if available.is_empty() {
            Ok(None)
        } else {
            let index = rng.gen_range(0, available.len());
            match available.get(index) {
                Some(pit_dur) => Ok(Some(pit_dur.clone())),
                None => ierr!("Failled on getting random task"),
            }
        }
    }
}

fn get_time_avail_all_prods(
    dyndata: &StatusDynamicData,
    inprodis: &Vector<ProdId>,
) -> Option<(Duration, Duration)> {
    let mut result: Option<(Duration, Duration)> = None;
    for prdid in inprodis.iter() {
        let start_maxwaitting = get_start_max_waitting(dyndata, prdid);
        dbg!(&prdid);
        dbg!(&start_maxwaitting);
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

fn get_better(r0: (Status, Option<i32>), r1: (Status, Option<i32>)) -> (Status, Option<i32>) {
    match (r0, r1) {
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
    }
}

pub(crate) fn generate_schedule(st: &Status) -> Result<(Status, Option<i32>), InternalError> {
    let mut result = (st.clone(), None);

    for _ in 0..100 {
        result = get_better(result, rec_process_pending_processes(st)?);
    }
    Ok(result)
}

fn rec_process_pending_processes(st: &Status) -> Result<(Status, Option<i32>), InternalError> {
    let mut st = st.clone();

    let mut result = (st.clone(), None);
    while let Some((procintask, start_at)) = st.get_one_random_ready2process()? {
        // dbg!(&start_at);
        st = st.run_process(&procintask, &start_at)?;
        result = get_better(result, rec_process_pending_processes(&st)?);
    }
    Ok((st.clone(), ponderate_solution(&st)?))
}

fn ponderate_solution(st: &Status) -> Result<Option<i32>, InternalError> {
    let mut result = 0i32;
    for procid_in_taskid in st.dynamic_data.pending_processes.iter() {
        let task = st.get_task(&procid_in_taskid.task_id)?;
        match task.priority {
            Priority::Mandatory => return Ok(None),
            Priority::High => result += 100,
            Priority::Medium => result += 10,
            Priority::Low => result += 1,
        }
    }
    Ok(Some(-result))
}

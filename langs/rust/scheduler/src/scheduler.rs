use crate::model::*;
// use rand::seq::SliceRandom;
// use rand::thread_rng;
use rand::seq::SliceRandom;
use rand::thread_rng;

// use rand::{thread_rng, Rng};
use rpds::{HashTrieSet, Vector};
use std::time::Duration;

macro_rules! ierr {
    ($($arg:tt)*) => {{
        Err(crate::scheduler::InternalError(format!($($arg)*)))
    }}
}

macro_rules! ierr_ {
    ($($arg:tt)*) => {{
        crate::scheduler::InternalError(format!($($arg)*))
    }}
}

mod model;
#[derive(Debug)]
pub(crate) struct InternalError(String);

pub(crate) fn generate_schedule_from_init_config(
    init_cfg: &InitConfig,
) -> Result<model::FinalStatus, InternalError> {
    let (status, recipes_db) = process_init_config(&init_cfg)?;
    // rec_process_pending_processes(status, &recipes_db)

    // todo, randomize list on each iteration
    (0..1000)
        .into_iter()
        .map(|_| shuflle_pending_processes(status.clone()))
        .map(|st| rec_process_pending_processes(st, &recipes_db))
        .try_fold(model::FinalStatus::Fail, |better_st, nw_st| {
            nw_st.and_then(|nw_st| Ok(get_better(better_st, nw_st)))
        })
    // Ok(status)
    // let mut result = (st.clone(), None);

    // for _ in 0..100 {
    //     result = get_better(result, rec_process_pending_processes(st)?);
    // }
    // Ok(result)
}

fn shuflle_pending_processes(mut status: model::Status) -> model::Status {
    let mut pp: Vec<_> = status.pending_processes.into_iter().collect();
    (&mut pp).shuffle(&mut thread_rng());

    status.pending_processes = pp.into_iter().map(|p| p.clone()).collect::<Vector<_>>();
    status
}

fn get_better(st1: model::FinalStatus, st2: model::Status) -> model::FinalStatus {
    let nw_mark = get_mark_status(&st2);

    match (&st1, nw_mark) {
        (st1, None) => st1.clone(),
        (model::FinalStatus::Fail, Some(m)) => {
            model::FinalStatus::Detail(model::FinalStatusDetail {
                status: st2,
                mark: m,
            })
        }
        (model::FinalStatus::Detail(det), Some(m)) => {
            if det.mark.0 < m.0 {
                model::FinalStatus::Detail(model::FinalStatusDetail {
                    status: st2,
                    mark: m,
                })
            } else {
                st1
            }
        }
    }
}

fn get_mark_status(st: &model::Status) -> Option<model::StatusMark> {
    st.pending_processes
        .into_iter()
        .map(|pp| pp.priority)
        .try_fold(0i32, |acc, m| match m {
            Priority::Mandatory => Err(()),
            Priority::High => Ok(acc - 100i32),
            Priority::Medium => Ok(acc - 10i32),
            Priority::Low => Ok(acc - 1i32),
        })
        .map_or(None, |m| Some(model::StatusMark(m)))
}

fn rec_process_pending_processes(
    status: model::Status,
    rec_db: &RecipesDb,
) -> Result<model::Status, InternalError> {
    if let Some((pp, proc, start_at)) = get_first_ready_process(&status, &rec_db)? {
        let status = exec_process(start_at, status, &pp, &proc)?;
        rec_process_pending_processes(status, &rec_db)
    } else {
        Ok(status)
    }
}

fn get_first_ready_process(
    status: &model::Status,
    rec_db: &RecipesDb,
) -> Result<Option<(model::PendingProcess, Process, Duration)>, InternalError> {
    status
        .pending_processes
        .iter()
        .map(|pp| get_process_if_executable(&pp, &status, &rec_db))
        .next()
        .map_or_else(|| Ok(None), std::convert::identity)
}

fn get_process_if_executable(
    pp: &model::PendingProcess,
    status: &model::Status,
    rec_db: &RecipesDb,
) -> Result<Option<(model::PendingProcess, Process, Duration)>, InternalError> {
    let (pp, process) = get_process_from_rec_db(pp, rec_db)?;

    if let Some(start_at) = get_starting_time_process(&process, &status.available_products) {
        Ok(Some((pp, process.clone(), start_at)))
    } else {
        Ok(None)
    }
}

fn get_process_from_rec_db(
    pp: &model::PendingProcess,
    rec_db: &RecipesDb,
) -> Result<(model::PendingProcess, Process), InternalError> {
    let process = rec_db
        .0
        .get(&pp.recipe_id)
        .ok_or_else(|| ierr_!("getting recipe from db {:?}", pp.recipe_id))?
        .processes
        .0
        .get(&pp.process_id)
        .ok_or_else(|| {
            ierr_!(
                "getting process {:?} from recipe {:?}",
                pp.recipe_id,
                pp.process_id
            )
        })?
        .clone();
    Ok((pp.clone(), process))
}

fn exec_process(
    start_at: Duration,
    status: model::Status,
    pp: &model::PendingProcess,
    process: &Process,
) -> Result<model::Status, InternalError> {
    Ok(status
        .remove_products(&process.inputs)?
        .add_available_products(start_at + process.required_time, &process.outputs)
        .remove_pending_process(&pp)
        .add_exec_info(start_at, pp, process))
}

fn get_starting_time_process(process: &Process, ap: &Vector<AvailableProduct>) -> Option<Duration> {
    process
        .inputs
        .iter()
        .map(|p| product_available_at(p, ap))
        .fold(None, |acc, d| match (acc, d) {
            (None, d) => d,
            (d, None) => d,
            (Some(acc), Some(d)) => Some(std::cmp::min(acc, d)),
        })
}

fn product_available_at(pid: &ProdId, ap: &Vector<AvailableProduct>) -> Option<Duration> {
    ap.iter()
        .filter(|&ap| &ap.product.prod_id == pid)
        .map(|ap| ap.available_at)
        .next()
}

fn process_init_config(
    init_cfg: &InitConfig,
) -> Result<(model::Status, crate::model::RecipesDb), InternalError> {
    let pending_processes = init_cfg
        .recipes_todo
        .into_iter()
        .try_fold(vector![], |acc, r2d| {
            add_procrecipes(
                acc,
                r2d.recipe_id.clone(),
                r2d.priority,
                &init_cfg.recipes_db,
            )
        })?;

    Ok((
        model::Status {
            available_products: init_cfg.available_products.clone(),
            pending_processes,
            executions: vector![],
        },
        init_cfg.recipes_db.clone(),
    ))
}

fn add_procrecipes(
    proc_recipes: Vector<model::PendingProcess>,
    recipe_id: RecipeId,
    priority: Priority,
    recipes_db: &RecipesDb,
) -> Result<Vector<model::PendingProcess>, InternalError> {
    let recipe = recipes_db
        .0
        .get(&recipe_id)
        .ok_or_else(|| ierr_!("getting recipe from db {:?}", recipe_id))?;
    Ok(recipe
        .processes
        .0
        .iter()
        .fold(proc_recipes, |acc, (process_id, _)| {
            acc.push_back(model::PendingProcess {
                recipe_id: recipe_id.clone(),
                process_id: process_id.clone(),
                priority,
            })
        }))
}

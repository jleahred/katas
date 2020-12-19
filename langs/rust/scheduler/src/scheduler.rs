use crate::model::*;
// use rand::seq::SliceRandom;
// use rand::thread_rng;

use rpds::{HashTrieMap, Vector};
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
) -> Result<model::FinalStatusExtended, InternalError> {
    let (status, recipes_db) = process_init_config(&init_cfg)?;

    let (status, final_status) = rec_process_pending_processes2(status, &recipes_db)?;
    Ok(model::FinalStatusExtended {
        status,
        final_status,
    })

    // (0..1000)
    //     .into_iter()
    //     .map(|_| shuflle_pending_processes(status.clone()))
    //     .map(|st| rec_process_pending_processes(st, &recipes_db))
    //     .try_fold(model::FinalStatus::Fail, |better_st, nw_st| {
    //         nw_st.and_then(|nw_st| Ok(get_better(better_st, nw_st)))
    //     })

    // Ok(status)
    // let mut result = (st.clone(), None);

    // for _ in 0..100 {
    //     result = get_better(result, rec_process_pending_processes(st)?);
    // }
    // Ok(result)
}

// fn shuflle_pending_processes(mut status: model::Status) -> model::Status {
//     let mut pp: Vec<_> = status.pending_processes.into_iter().collect();
//     (&mut pp).shuffle(&mut thread_rng());

//     status.pending_processes = pp.into_iter().map(|p| p.clone()).collect::<Vector<_>>();
//     status
// }

fn get_final_status(status: model::Status) -> model::FinalStatus {
    let (pend_recipes, completed_recipes) = get_pending_completed_recipes(&status);
    let status = remove_execs_recipes(status, &pend_recipes);

    match get_mark_status(&pend_recipes, &status.recipes_todo) {
        None => model::FinalStatus::Fail, //(status),
        Some(mark) => model::FinalStatus::Detail(model::FinalStatusDetail {
            // status: status.clone(),
            mark,
            completed_recipes,
            executions: status.executions.clone(),
        }),
    }
}

fn remove_execs_recipes(
    mut status: model::Status,
    pend_recipes: &HashTrieMap<RecipeId, Priority>,
) -> model::Status {
    let execs = status
        .executions
        .iter()
        .filter(|&ex| !pend_recipes.get(&ex.recipe_id).is_some())
        .map(|e| e.clone())
        .collect::<Vector<model::Execution>>();
    status.executions = execs;
    status
}

fn get_better_fs(st1: model::FinalStatus, st2: model::FinalStatus) -> model::FinalStatus {
    match (&st1, &st2) {
        (model::FinalStatus::Fail /*(_)*/, st2) => st2.clone(),
        (st1, model::FinalStatus::Fail /*(_)*/) => st1.clone(),
        (model::FinalStatus::Detail(d1), model::FinalStatus::Detail(d2)) => {
            if d1.mark > d2.mark {
                model::FinalStatus::Detail(d1.clone())
            } else {
                model::FinalStatus::Detail(d2.clone())
            }
        }
    }
}

fn get_mark_status(
    pend_recipes: &HashTrieMap<RecipeId, Priority>,
    recipes_todo: &Vector<crate::model::RecipeTodo>,
) -> Option<model::StatusMark> {
    // st.pending_processes
    //     .into_iter()
    //     .map(|pp| pp.priority)
    //     .try_fold(0i32, |acc, m| match m {
    //         Priority::Mandatory => Err(()),
    //         Priority::High => Ok(acc - 100i32),
    //         Priority::Medium => Ok(acc - 10i32),
    //         Priority::Low => Ok(acc - 1i32),
    //     })
    //     .map_or(None, |m| Some(model::StatusMark(m)))
    // get_pending_recipes(st)
    if pend_recipes.size() == recipes_todo.len() {
        None
    } else {
        pend_recipes
            .iter()
            .map(|(_rid, priority)| priority)
            .try_fold(0i32, |acc, m| match m {
                Priority::Mandatory => Err(()),
                Priority::High => Ok(acc - 100i32),
                Priority::Medium => Ok(acc - 10i32),
                Priority::Low => Ok(acc - 1i32),
            })
            .map_or(None, |m| Some(model::StatusMark(m)))
    }
}

fn get_pending_completed_recipes(
    status: &model::Status,
) -> (HashTrieMap<RecipeId, Priority>, Vector<RecipeId>) {
    let pend_rec = status
        .pending_processes
        .iter()
        .fold(HashTrieMap::new(), |acc, pp| {
            acc.insert(pp.recipe_id.clone(), pp.priority)
        });
    let compl_rec = status
        .recipes_todo
        .iter()
        .filter(|&rt| pend_rec.get(&rt.recipe_id).is_none())
        .map(|r| r.recipe_id.clone())
        .collect::<Vector<_>>();
    (pend_rec, compl_rec)
}

fn rec_process_pending_processes2(
    status: model::Status,
    rec_db: &RecipesDb,
) -> Result<(model::Status, model::FinalStatus), InternalError> {
    let pp = get_pending_executable_processes(&status, rec_db)?;

    if pp.is_empty() {
        Ok((status.clone(), get_final_status(status)))
    } else {
        let final_status = pp.iter().try_fold(
            model::FinalStatus::Fail, //(status.clone()),
            |acc, (pp, process, start_at)| {
                exec_process(*start_at, status.clone(), pp, process).and_then(|st| {
                    rec_process_pending_processes2(st, rec_db)
                        .and_then(|(_, fs)| Ok(get_better_fs(acc, fs)))
                })
            },
        )?;
        Ok((status, final_status))
    }
}

fn get_pending_executable_processes(
    status: &model::Status,
    rec_db: &RecipesDb,
) -> Result<Vector<(model::PendingProcess, Process, Duration)>, InternalError> {
    status
        .pending_processes
        .iter()
        .map(|pp| get_process_if_executable(&pp, &status, &rec_db))
        .try_fold(vector![], |acc, pp| match pp {
            Ok(Some(pp_info)) => Ok(acc.push_back(pp_info)),
            Ok(None) => Ok(acc),
            Err(e) => Err(e),
        })
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
    let o_start_end = process
        .inputs
        .iter()
        .map(|p| product_available(p, ap))
        .fold((false, None), |(found_missing, acc), d| {
            match (found_missing, acc, d) {
                (false, None, Some(ap)) => (
                    false,
                    Some((ap.available_at, ap.available_at + ap.product.valid_for)),
                ),
                (false, _, None) => (true, None),
                (false, Some(acc), Some(ap)) => (
                    false,
                    Some((
                        std::cmp::max(acc.0, ap.available_at),
                        std::cmp::min(acc.1, ap.available_at + ap.product.valid_for),
                    )),
                ),
                (true, _, _) => (true, None),
            }
        })
        .1;
    match o_start_end {
        None => None,
        Some((start, end)) => {
            if start < end {
                Some(start)
            } else {
                None
            }
        }
    }
}

fn product_available(pid: &ProdId, ap: &Vector<AvailableProduct>) -> Option<AvailableProduct> {
    ap.iter()
        .filter(|&ap| &ap.product.prod_id == pid)
        .map(|ap| ap.clone())
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
            recipes_todo: init_cfg.recipes_todo.clone(),
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

// fn get_first_ready_process(
//     status: &model::Status,
//     rec_db: &RecipesDb,
// ) -> Result<Option<(model::PendingProcess, Process, Duration)>, InternalError> {
//     status
//         .pending_processes
//         .iter()
//         .map(|pp| get_process_if_executable(&pp, &status, &rec_db))
//         .next()
//         .map_or_else(|| Ok(None), std::convert::identity)
// }

// fn get_better(st1: model::FinalStatus, st2: model::Status) -> model::FinalStatus {
//     // let st2 = match get_mark_status(&st2) {
//     //     None => model::FinalStatus::Fail,
//     //     Some(mark) => model::FinalStatus::Detail(model::FinalStatusDetail { status: st2, mark }),
//     // };

//     get_better_fs(st1, get_final_status(st2))
//     // let nw_mark = get_mark_status(&st2);

//     // match (&st1, nw_mark) {
//     //     (st1, None) => st1.clone(),
//     //     (model::FinalStatus::Fail, Some(m)) => {
//     //         model::FinalStatus::Detail(model::FinalStatusDetail {
//     //             status: st2,
//     //             mark: m,
//     //         })
//     //     }
//     //     (model::FinalStatus::Detail(det), Some(m)) => {
//     //         if det.mark.0 < m.0 {
//     //             model::FinalStatus::Detail(model::FinalStatusDetail {
//     //                 status: st2,
//     //                 mark: m,
//     //             })
//     //         } else {
//     //             st1
//     //         }
//     //     }
//     // }
// }

// fn rec_process_pending_processes(
//     status: model::Status,
//     rec_db: &RecipesDb,
// ) -> Result<model::Status, InternalError> {
//     if let Some((pp, proc, start_at)) = get_first_ready_process(&status, &rec_db)? {
//         let status = exec_process(start_at, status, &pp, &proc)?;
//         rec_process_pending_processes(status, &rec_db)
//     } else {
//         Ok(status)
//     }
// }

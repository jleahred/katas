use rpds::Vector;
use serde::{Deserialize, Serialize};

use crate::model::{AvailableAt, EndsBefore};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct Status {
    pub(crate) available_products: Vector<crate::model::AvailableProduct>,
    pub(crate) recipes_todo: Vector<crate::model::RecipeTodo>,
    pub(crate) pending_processes: Vector<PendingProcess>,
    pub(crate) executions: Vector<Execution>,
}

#[derive(Debug, Serialize, Deserialize, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct StatusMark(pub(crate) i32);

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) enum FinalStatus {
    Fail, //Fail(Status),
    Detail(FinalStatusDetail),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct FinalStatusExtended {
    pub(crate) final_status: FinalStatus,
    pub(crate) status: Status,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct FinalStatusDetail {
    //pub(crate) status: Status,
    pub(crate) mark: StatusMark,
    pub(crate) completed_recipes: Vector<crate::model::RecipeId>,
    pub(crate) executions: Vector<Execution>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct Execution {
    start_at: crate::model::StartAt,
    duration: crate::model::RequiredTime,

    pub(crate) recipe_id: crate::model::RecipeId,
    pub(crate) process_id: crate::model::ProcessId,
    process_desc: String,
    sequence: Vector<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub(crate) struct PendingProcessKey {
    pub(crate) recipe_id: crate::model::RecipeId,
    pub(crate) process_id: crate::model::ProcessId,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct PendingProcessValue {
    pub(crate) process: crate::model::Process,
    pub(crate) priority: crate::model::Priority,
    pub(crate) ends_before: EndsBefore,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct PendingProcess(pub(crate) PendingProcessKey, pub(crate) PendingProcessValue);

impl Status {
    pub(crate) fn remove_products(
        mut self,
        prods: &Vector<crate::model::ProdId>,
    ) -> Result<Self, super::InternalError> {
        self.available_products = prods
            .iter()
            .try_fold(self.available_products, |acc, p| remove_product(acc, p))?;
        Ok(self)
    }
    pub(crate) fn add_available_products(
        mut self,
        available_at: AvailableAt,
        prods: &Vector<crate::model::Product>,
    ) -> Self {
        self.available_products = prods
            .iter()
            .map(|product| crate::model::AvailableProduct {
                product: product.clone(),
                available_at,
            })
            .fold(self.available_products, |acc, ap| acc.push_back(ap.clone()));
        self
    }
    pub(crate) fn remove_pending_process(mut self, pp: &PendingProcess) -> Self {
        self.pending_processes = self
            .pending_processes
            .iter()
            .fold((vector![], false), |(acc, found), spp| {
                match (found, spp.0 == pp.0) {
                    (true, _) => (acc.push_back(spp.clone()), true),
                    (false, true) => (acc, true),
                    (false, false) => (acc.push_back(spp.clone()), false),
                }
            })
            .0;
        self
    }
    pub(crate) fn add_exec_info(
        mut self,
        start_at: crate::model::StartAt,
        pp: &PendingProcess,
    ) -> Self {
        self.executions = self.executions.push_back(Execution {
            start_at,
            duration: pp.1.process.required_time,
            recipe_id: pp.0.recipe_id.clone(),
            process_id: pp.0.process_id.clone(),
            process_desc: pp.1.process.description.clone(),
            sequence: pp.1.process.sequence.clone(),
        });
        self
    }
}
fn remove_product(
    aps: Vector<crate::model::AvailableProduct>,
    prod_id: &crate::model::ProdId,
) -> Result<Vector<crate::model::AvailableProduct>, super::InternalError> {
    let orig_prods = aps.len();

    let (new_available_products, _) = aps.iter().fold((vector![], false), |(acc, found), ap| {
        if !found {
            let found = ap.product.prod_id != *prod_id;
            if found {
                (acc, true)
            } else {
                (acc.push_back(ap.clone()), false)
            }
        } else {
            (acc, found)
        }
    });

    let end_prods = new_available_products.len();

    if end_prods == orig_prods {
        ierr!("remove products, not found {:?}", prod_id)
    } else {
        Ok(new_available_products)
    }
}

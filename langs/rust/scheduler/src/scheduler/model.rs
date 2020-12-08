use rpds::{HashTrieSet, Vector};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct Status {
    pub(crate) available_products: Vector<crate::model::AvailableProduct>,
    pub(crate) pending_processes: Vector<PendingProcess>,
    // executions: Vector<Execution>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct PendingProcess {
    pub(crate) recipe_id: crate::model::RecipeId,
    pub(crate) process_id: crate::model::ProcessId,
    pub(crate) priority: crate::model::Priority,
}

impl Status {
    pub(crate) fn remove_products(
        mut self,
        prods: &Vector<crate::model::ProdId>,
    ) -> Result<Self, super::InternalError> {
        self.available_products = prods
            .iter()
            .try_fold(self.available_products, |acc, p| remove_product(acc, p))?;
        Ok(self) // todo
    }
    pub(crate) fn add_available_products(
        mut self,
        aps: &Vector<crate::model::AvailableProduct>,
    ) -> Self {
        self.available_products = aps
            .iter()
            .fold(self.available_products, |acc, ap| acc.push_back(ap.clone()));
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
            let found = ap.prod_id != *prod_id;
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

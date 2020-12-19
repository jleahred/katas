use rpds::Vector;
use serde::{Deserialize, Serialize};
use std::time::Duration;

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct InitConfig {
    pub(crate) recipes_db: RecipesDb,
    pub(crate) recipes_todo: Vector<RecipeTodo>,
    pub(crate) available_products: Vector<AvailableProduct>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct RecipesDb(pub(crate) rpds::HashTrieMap<RecipeId, Recipe>);

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct Recipe {
    pub(crate) description: String,
    pub(crate) processes: Processes,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct AvailableProduct {
    pub(crate) product: Product,
    pub(crate) available_at: AvailableAt,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct Product {
    pub(crate) prod_id: ProdId,
    pub(crate) valid_for: ValidFor,
}

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct RecipeTodo {
    pub(crate) recipe_id: RecipeId,
    pub(crate) priority: Priority,
    pub(crate) ends_before: EndsBefore,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
pub(crate) struct RecipeId(String);

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ProcessId(String);

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct Process {
    pub(crate) description: String,
    pub(crate) inputs: Vector<ProdId>,
    pub(crate) outputs: Vector<Product>,
    pub(crate) sequence: Vector<String>,
    pub(crate) required_time: RequiredTime,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct Processes(pub(crate) rpds::HashTrieMap<ProcessId, Process>);

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub(crate) enum Priority {
    Mandatory,
    High,
    Medium,
    Low,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ProdId(String);

#[derive(Debug, Serialize, Deserialize, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct AvailableAt(#[serde(with = "humantime_serde")] pub(crate) Duration);

#[derive(Debug, Serialize, Deserialize, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct StartAt(#[serde(with = "humantime_serde")] pub(crate) Duration);

#[derive(Debug, Serialize, Deserialize, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ValidTill(#[serde(with = "humantime_serde")] pub(crate) Duration);

#[derive(Debug, Serialize, Deserialize, Clone, Copy)]
pub(crate) struct ValidFor(#[serde(with = "humantime_serde")] pub(crate) Duration);

#[derive(Debug, Serialize, Deserialize, Clone, Copy)]
pub(crate) struct RequiredTime(#[serde(with = "humantime_serde")] pub(crate) Duration);

#[derive(Debug, Serialize, Deserialize, Clone, Copy)]
pub(crate) struct EndsBefore(#[serde(with = "humantime_serde")] pub(crate) Duration);

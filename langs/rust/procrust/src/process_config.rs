use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ProcessConfig {
    pub id: String,
    pub command: String,
}

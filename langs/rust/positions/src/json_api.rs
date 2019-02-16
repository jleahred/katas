use crate::position::{Level, Position, Side};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct JsonApi {
    pub _msg_type: String,
    pub _payload: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct JsonApiAdd {
    pub id: String,
    pub desc: String,
    pub dt: String,
    pub isin: String,
    pub side: String,
    pub price: f64,
    pub qty: u64,
}

impl JsonApiAdd {
    pub fn to_position(&self) -> Result<Position, String> {
        Ok(Position {
            id: self.id.clone(),
            sub_group: "pending fill sub group".to_string(),
            desc: self.desc.clone(),
            isin: self.isin.clone(),
            side: side_from_string(&self.side)?,
            level: Level {
                price: self.price,
                qty: self.qty,
            },
            updated: self.dt.clone(),
        })
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct JsonApiDel {
    pub id: String,
}

fn side_from_string(sside: &str) -> Result<Side, String> {
    match sside {
        "BUY" => Ok(Side::Bid),
        "SELL" => Ok(Side::Ask),
        _ => Err(format!("unknow side {}", sside)),
    }
}

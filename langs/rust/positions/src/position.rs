pub type GroupsPos = Vec<PositionsGroup>;

pub struct PositionsGroup {
    pub group: String,
    pub pos_prods: PosByProds,
}

pub type PosByProds = Vec<PositionsByProduct>;
pub type Levels = Vec<Level>;
pub type Positions = Vec<Position>;

pub struct PositionsByProduct {
    pub isin: String,
    pub desc: String,
    pub updated: String,
    pub bids: Levels,
    pub asks: Levels,
}

#[derive(Debug, Clone)]
pub struct Level {
    pub price: f64,
    pub qty: u64,
}

#[derive(Debug, Clone)]
pub struct Position {
    pub id: String,
    pub sub_group: String, //  country?
    pub isin: String,
    pub desc: String,
    pub side: Side,
    pub level: Level,
    pub updated: String,
}

#[derive(Debug, Clone, Copy)]
pub enum Side {
    Bid,
    Ask,
}

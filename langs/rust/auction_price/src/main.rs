fn main() {
    let auction_levels = get_example_order_book()
        .get_more_qty_exec_levels()
        .get_min_diff_levels()
        .get_max_qty_levels()
        .get_proxim2_levels(Price(60));
        ;

    println!("__ {:#?}", auction_levels);
}

//  ----------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum Side {
    Bid,
    Ask,
}

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord, Copy, Clone)]
struct Price(i64);

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord, Copy, Clone)]
struct Qty(u64);

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord, Copy, Clone)]
struct QtyBid(u64);

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord, Copy, Clone)]
struct QtyAsk(u64);

#[derive(Debug, Eq, PartialEq, Clone)]
struct QtyBidAsk {
    qbid: QtyBid,
    qask: QtyAsk,
}

impl QtyBidAsk {
    fn new(qbid: QtyBid, qask: QtyAsk) -> Self {
        QtyBidAsk { qbid, qask }
    }
    fn inc_bid(&mut self, qty: QtyBid) {
        self.qbid.0 += qty.0;
    }
    fn inc_ask(&mut self, qty: QtyAsk) {
        self.qask.0 += qty.0;
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct Level(Price, QtyBidAsk);

use std::collections::BTreeMap;

#[derive(Debug, Eq, PartialEq)]
struct OrderBookAcc {
    limit: BTreeMap<Price, QtyBidAsk>,
    market: QtyBidAsk,
}

impl OrderBookAcc {
    fn new() -> Self {
        OrderBookAcc {
            limit: BTreeMap::new(),
            market: QtyBidAsk::new(QtyBid(0), QtyAsk(0)),
        }
    }
    fn get_prev_bid_ask(&self, price: Price) -> QtyBidAsk {
        let prev_bid = match self.limit.iter().find(|(&p, _)| p >= price) {
            Some((_price, qba)) => qba.qbid,
            None => self.market.qbid,
        };
        let prev_ask = match self.limit.iter().rev().find(|(&p, _)| p <= price) {
            Some((_price, qba)) => qba.qask,
            None => self.market.qask,
        };
        QtyBidAsk::new(prev_bid, prev_ask)
    }
    fn accumulate_bid_levels(&mut self, price: Price, qty: QtyBid) {
        for (_price, qba) in self.limit.iter_mut().take_while(|(&p, _)| p <= price) {
            qba.inc_bid(qty);
        }
    }
    fn accumulate_ask_levels(&mut self, price: Price, qty: QtyAsk) {
        for (_price, qba) in self.limit.iter_mut().rev().take_while(|(&p, _)| p >= price) {
            qba.inc_ask(qty);
        }
    }

    fn add_limit_order(mut self, side: Side, price: Price, qty: Qty) -> Self {
        let qty_bid_ask = self.get_prev_bid_ask(price);
        self.limit.entry(price).or_insert(qty_bid_ask);
        if side == Side::Bid {
            self.accumulate_bid_levels(price, QtyBid(qty.0));
        } else {
            self.accumulate_ask_levels(price, QtyAsk(qty.0));
        }
        self
    }
    fn add_market_order(mut self, side: Side, qty: Qty) -> Self {
        if side == Side::Bid {
            let qbid = QtyBid(qty.0);
            self.market.inc_bid(qbid);
            for (_price, qty_bid_ask) in self.limit.iter_mut() {
                qty_bid_ask.inc_bid(qbid);
            }
        } else {
            let qask = QtyAsk(qty.0);
            self.market.inc_ask(qask);
            for (_price, qty_bid_ask) in self.limit.iter_mut() {
                qty_bid_ask.inc_ask(qask);
            }
        }
        self
    }

    fn get_more_qty_exec_levels(&self) -> AuctionLevelsTrait {
        let some_max_qty_cross = self
            .limit
            .iter()
            .map(|(_price, qba)| std::cmp::min(qba.qbid.0, qba.qask.0))
            .max_by(|d1, d2| d1.cmp(d2));
        if let Some(max_qty_cross) = some_max_qty_cross {
            AuctionLevelsTrait(
                self.limit
                    .iter()
                    .filter(|(_price, qba)| std::cmp::min(qba.qbid.0, qba.qask.0) == max_qty_cross)
                    .map(|(&price, b_a)| Level(price, b_a.clone()))
                    .collect::<Vec<_>>(),
            )
        } else {
            AuctionLevelsTrait(vec![])
        }
    }
}

#[derive(Debug)]
struct AuctionLevelsTrait(Vec<Level>);

impl AuctionLevelsTrait {
    fn get_min_diff_levels(&self) -> AuctionLevelsTrait {
        let some_min_abs_diff = self
            .0
            .iter()
            .map(|Level(_price, qba)| (qba.qbid.0 as i64 - qba.qask.0 as i64).abs())
            .min();

        AuctionLevelsTrait(if let Some(min_abs_diff) = some_min_abs_diff {
            self.0
                .iter()
                .filter(|Level(_price, qba)| {
                    (qba.qbid.0 as i64 - qba.qask.0 as i64).abs() == min_abs_diff
                })
                .cloned()
                .collect::<Vec<_>>()
        } else {
            vec![]
        })
    }
    fn get_max_qty_levels(&self) -> AuctionLevelsTrait {
        let some_max_qty = self
            .0
            .iter()
            .map(|Level(_price, qba)| std::cmp::max(qba.qbid.0 as i64, qba.qask.0 as i64))
            .max();

        AuctionLevelsTrait(if let Some(max_qty) = some_max_qty {
            self.0
                .iter()
                .filter(|Level(_price, qba)| {
                    qba.qbid.0 as i64 == max_qty || qba.qask.0 as i64 == max_qty
                })
                .cloned()
                .collect::<Vec<_>>()
        } else {
            vec![]
        })
    }
    fn get_proxim2_levels(&self, price: Price) -> AuctionLevelsTrait {
        let some_min_dif_ref_price = self
            .0
            .iter()
            .map(|Level(p, _qba)| (p.0 - price.0).abs())
            .min();

        AuctionLevelsTrait(if let Some(min_dif_ref_price) = some_min_dif_ref_price {
            self.0
                .iter()
                .filter(|Level(p, _qba)| (p.0 - price.0).abs() == min_dif_ref_price)
                .cloned()
                .collect::<Vec<_>>()
        } else {
            vec![]
        })
    }
}

fn get_example_order_book() -> OrderBookAcc {
    //  zeida ejemplo
    // OrderBookAcc::new()
    //     .add_limit_order(Side::Bid, Price(65), Qty(100))
    //     .add_limit_order(Side::Bid, Price(64), Qty(15))
    //     .add_market_order(Side::Bid, Qty(50))
    //     .add_limit_order(Side::Ask, Price(66), Qty(80))
    //     .add_limit_order(Side::Ask, Price(62), Qty(50))
    //     .add_market_order(Side::Ask, Qty(170))

    OrderBookAcc::new()
        .add_limit_order(Side::Bid, Price(4), Qty(1))
        .add_limit_order(Side::Bid, Price(10), Qty(2))
        .add_limit_order(Side::Bid, Price(20), Qty(3))
        .add_limit_order(Side::Bid, Price(30), Qty(1))
        .add_limit_order(Side::Bid, Price(50), Qty(5))
        //
        .add_limit_order(Side::Ask, Price(4), Qty(1))
        .add_limit_order(Side::Ask, Price(10), Qty(4))
        .add_limit_order(Side::Ask, Price(20), Qty(1))
        .add_limit_order(Side::Ask, Price(50), Qty(1))
        //
        .add_market_order(Side::Bid, Qty(1))
        .add_market_order(Side::Bid, Qty(1))
        .add_market_order(Side::Bid, Qty(1))
        .add_market_order(Side::Ask, Qty(1))
        .add_market_order(Side::Ask, Qty(1))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_adding_simple() {
        let ob1 = OrderBookAcc::new()
            .add_limit_order(Side::Bid, Price(4), Qty(1))
            //
            .add_limit_order(Side::Ask, Price(10), Qty(1));

        let ob2 = OrderBookAcc::new()
            .add_limit_order(Side::Ask, Price(10), Qty(1))
            .add_limit_order(Side::Bid, Price(4), Qty(1));

        assert_eq!(ob1, ob2);
    }

    #[test]
    fn test_adding_order() {
        let ob1 = OrderBookAcc::new()
            .add_limit_order(Side::Bid, Price(4), Qty(1))
            .add_limit_order(Side::Bid, Price(10), Qty(1))
            .add_limit_order(Side::Bid, Price(20), Qty(1))
            .add_limit_order(Side::Bid, Price(30), Qty(1))
            //
            .add_limit_order(Side::Ask, Price(10), Qty(1))
            .add_limit_order(Side::Ask, Price(20), Qty(1))
            .add_limit_order(Side::Ask, Price(30), Qty(1))
            //
            .add_market_order(Side::Bid, Qty(1))
            .add_market_order(Side::Bid, Qty(1))
            .add_market_order(Side::Bid, Qty(1))
            .add_market_order(Side::Ask, Qty(1))
            // .add_market_order(Side::Ask, Qty(1));
            ;

        let ob2 = OrderBookAcc::new()
            .add_limit_order(Side::Ask, Price(10), Qty(1))
            .add_limit_order(Side::Bid, Price(10), Qty(1))
            .add_limit_order(Side::Bid, Price(20), Qty(1))
            .add_limit_order(Side::Ask, Price(20), Qty(1))
            .add_market_order(Side::Bid, Qty(1))
            .add_limit_order(Side::Ask, Price(30), Qty(1))
            .add_limit_order(Side::Bid, Price(30), Qty(1))
            .add_limit_order(Side::Bid, Price(4), Qty(1))
            .add_market_order(Side::Bid, Qty(1))
            .add_market_order(Side::Bid, Qty(1))
            .add_market_order(Side::Ask, Qty(1))
            //
            // .add_market_order(Side::Ask, Qty(1));
            ;

        assert_eq!(ob1, ob2);
    }
}

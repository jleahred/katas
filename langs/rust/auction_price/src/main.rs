fn main() {
    let price = get_example_order_book()
        .get_more_qty_exec_levels()
        .get_min_diff_levels()
        .get_proxim2(Price(60));

    println!("__ {:#?}", price);
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

use std::collections::BTreeMap;

#[derive(Debug, Eq, PartialEq)]
struct OrderBookAcc {
    limit: BTreeMap<Price, (QtyBid, QtyAsk)>,
    market: (QtyBid, QtyAsk),
}

impl OrderBookAcc {
    fn new() -> Self {
        OrderBookAcc {
            limit: BTreeMap::new(),
            market: (QtyBid(0), QtyAsk(0)),
        }
    }
    fn get_prev_bid_ask(&self, price: Price) -> (QtyBid, QtyAsk) {
        let prev_bid = match self.limit.iter().find(|(p, _)| p.0 >= price.0) {
            Some((_price, (qty_bid, _qty_ask))) => QtyBid(qty_bid.0),
            None => self.market.0,
        };
        let prev_ask = match self.limit.iter().rev().find(|(p, _)| p.0 <= price.0) {
            Some((_price, (_qty_bid, qty_ask))) => QtyAsk(qty_ask.0),
            None => self.market.1,
        };
        (prev_bid, prev_ask)
    }
    fn accumulate_bid_levels(&mut self, price: Price, qty: QtyBid) {
        for (_price, (bid, _ask)) in self.limit.iter_mut().take_while(|(p, _)| p.0 <= price.0) {
            bid.0 += qty.0;
        }
    }
    fn accumulate_ask_levels(&mut self, price: Price, qty: QtyAsk) {
        for (_price, (_bid, ask)) in self
            .limit
            .iter_mut()
            .rev()
            .take_while(|(p, _)| p.0 >= price.0)
        {
            ask.0 += qty.0;
        }
    }

    fn add_limit_order(mut self, side: Side, price: Price, qty: Qty) -> Self {
        let (prev_bid, prev_ask) = self.get_prev_bid_ask(price);
        self.limit.entry(price).or_insert((prev_bid, prev_ask));
        if side == Side::Bid {
            self.accumulate_bid_levels(price, QtyBid(qty.0));
        } else {
            self.accumulate_ask_levels(price, QtyAsk(qty.0));
        }
        self
    }
    fn add_market_order(mut self, side: Side, qty: Qty) -> Self {
        if side == Side::Bid {
            (self.market.0).0 += qty.0;
            for (_price, (bid, _ask)) in self.limit.iter_mut() {
                bid.0 += qty.0;
            }
        } else {
            (self.market.1).0 += qty.0;
            for (_price, (_bid, ask)) in self.limit.iter_mut() {
                ask.0 += qty.0;
            }
        }
        self
    }

    fn get_more_qty_exec_levels(&self) -> MoreExecLevels {
        let some_max_qty_cross = self
            .limit
            .iter()
            .map(|(_price, (b, a))| std::cmp::min(b.0, a.0))
            .max_by(|d1, d2| d1.cmp(d2));
        if let Some(max_qty_cross) = some_max_qty_cross {
            MoreExecLevels(
                self.limit
                    .iter()
                    .filter(|(_price, (b, a))| std::cmp::min(b.0, a.0) == max_qty_cross)
                    .map(|(price, (b, a))| (*price, (*b, *a)))
                    .collect::<Vec<_>>(),
            )
        } else {
            MoreExecLevels(vec![])
        }
    }
}

#[derive(Debug)]
struct MoreExecLevels(Vec<(Price, (QtyBid, QtyAsk))>);

impl MoreExecLevels {
    fn get_min_diff_levels(&self) -> MinDiffLevels {
        let some_min_abs_diff = self
            .0
            .iter()
            .map(|(_price, (b, a))| (b.0 as i64 - a.0 as i64).abs())
            .min();

        MinDiffLevels(if let Some(min_abs_diff) = some_min_abs_diff {
            self.0
                .iter()
                .filter(|(_price, (b, a))| (b.0 as i64 - a.0 as i64).abs() == min_abs_diff)
                .map(|(price, _)| *price)
                .collect::<Vec<_>>()
        } else {
            vec![]
        })
    }
}

#[derive(Debug)]
struct MinDiffLevels(Vec<Price>);

impl MinDiffLevels {
    fn get_proxim2(&self, price: Price) -> Option<Price> {
        let init: Option<(i64, Price)> = None; //  min diff, first found price
        self.0
            .iter()
            .fold(init, |some_acc, &p| {
                if let Some(acc) = some_acc {
                    let curr_diff = (p.0 - price.0).abs();
                    if curr_diff < acc.0 {
                        Some((curr_diff, p))
                    } else {
                        Some(acc)
                    }
                } else {
                    Some(((p.0 - price.0).abs(), p))
                }
            })
            .map(|(_diff, price)| price)
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

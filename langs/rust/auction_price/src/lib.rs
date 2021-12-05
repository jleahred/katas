//  ----------------------

/// Result price and qty of auction
#[derive(Debug, Eq, PartialEq, PartialOrd, Ord, Clone, Copy)]
pub struct Auction {
    pub price: Price,
    pub quantity: Qty,
}

impl Auction {
    pub fn new(price: Price, quantity: Qty) -> Self {
        Self { price, quantity }
    }
}

#[derive(Debug)]
pub enum Order {
    Limit(Limit),
    Market(Market),
}

impl Order {
    pub fn new_limit(side: Side, price: Price, qty: Qty) -> Self {
        Order::Limit(Limit { side, price, qty })
    }
    pub fn new_market(side: Side, qty: Qty) -> Self {
        Order::Market(Market { side, qty })
    }
}

#[derive(Debug)]
pub struct Limit {
    pub side: Side,
    pub price: Price,
    pub qty: Qty,
}

#[derive(Debug)]
pub struct Market {
    pub side: Side,
    pub qty: Qty,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Side {
    Buy,
    Sell,
}

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord, Copy, Clone)]
pub struct Price(pub i64);

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord, Copy, Clone)]
pub struct Qty(pub u64);

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord, Copy, Clone)]
struct QtyBuy(u64);

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord, Copy, Clone)]
struct QtySell(u64);

#[derive(Debug, Eq, PartialEq, Clone)]
struct QtyBuySell {
    qbuy: QtyBuy,
    qsell: QtySell,
}

impl QtyBuySell {
    fn new(qbuy: QtyBuy, qsell: QtySell) -> Self {
        QtyBuySell { qbuy, qsell }
    }
    fn inc_buy(&mut self, qty: QtyBuy) {
        self.qbuy.0 += qty.0;
    }
    fn inc_sell(&mut self, qty: QtySell) {
        self.qsell.0 += qty.0;
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct Level {
    price: Price,
    qty_buysell: QtyBuySell,
}

impl Level {
    fn new(price: Price, qty_buysell: QtyBuySell) -> Self {
        Self { price, qty_buysell }
    }
    fn get_auction(&self) -> Option<Auction> {
        let min_qty = std::cmp::min(self.qty_buysell.qbuy.0, self.qty_buysell.qsell.0);
        match min_qty {
            0 => None,
            qty => Some(Auction {
                price: self.price,
                quantity: Qty(qty),
            }),
        }
    }
}

use std::collections::BTreeMap;

/// To calculate the auction price in an incremental way
/// The cost will be O(n) depending on number of different prices,
/// not of number of orders
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct OrderBookAcc {
    limit: BTreeMap<Price, QtyBuySell>,
    market: QtyBuySell,
}

impl OrderBookAcc {
    pub fn new() -> Self {
        OrderBookAcc {
            limit: BTreeMap::new(),
            market: QtyBuySell::new(QtyBuy(0), QtySell(0)),
        }
    }

    pub fn auction(&self, price: Price) -> Option<Auction> {
        self.more_qty_exec_levels()
            .min_diffqty_levels()
            .max_qty_levels()
            .end_resolve_ref_price(price)
    }

    fn get_prev_buy_sell(&self, price: Price) -> QtyBuySell {
        let prev_buy = match self.limit.iter().find(|(&p, _)| p >= price) {
            Some((_price, qba)) => qba.qbuy,
            None => self.market.qbuy,
        };
        let prev_sell = match self.limit.iter().rev().find(|(&p, _)| p <= price) {
            Some((_price, qba)) => qba.qsell,
            None => self.market.qsell,
        };
        QtyBuySell::new(prev_buy, prev_sell)
    }
    fn accumulate_buy_levels(&mut self, price: Price, qty: QtyBuy) {
        for (_price, qba) in self.limit.iter_mut().take_while(|(&p, _)| p <= price) {
            qba.inc_buy(qty);
        }
    }
    fn accumulate_sell_levels(&mut self, price: Price, qty: QtySell) {
        for (_price, qba) in self.limit.iter_mut().rev().take_while(|(&p, _)| p >= price) {
            qba.inc_sell(qty);
        }
    }

    pub fn add_order(self, order: &Order) -> Self {
        match order {
            Order::Limit(order) => self.add_limit_order(order.side, order.price, order.qty),
            Order::Market(order) => self.add_market_order(order.side, order.qty),
        }
    }
    fn add_limit_order(mut self, side: Side, price: Price, qty: Qty) -> Self {
        let qty_buy_sell = self.get_prev_buy_sell(price);
        self.limit.entry(price).or_insert(qty_buy_sell);
        if side == Side::Buy {
            self.accumulate_buy_levels(price, QtyBuy(qty.0));
        } else {
            self.accumulate_sell_levels(price, QtySell(qty.0));
        }
        self
    }

    fn add_market_order(mut self, side: Side, qty: Qty) -> Self {
        if side == Side::Buy {
            let qbuy = QtyBuy(qty.0);
            self.market.inc_buy(qbuy);
            for (_price, qty_buy_sell) in self.limit.iter_mut() {
                qty_buy_sell.inc_buy(qbuy);
            }
        } else {
            let qsell = QtySell(qty.0);
            self.market.inc_sell(qsell);
            for (_price, qty_buy_sell) in self.limit.iter_mut() {
                qty_buy_sell.inc_sell(qsell);
            }
        }
        self
    }

    fn more_qty_exec_levels(&self) -> AuctionLevelsInProcess {
        let some_max_qty_cross = self
            .limit
            .iter()
            .map(|(_price, qba)| std::cmp::min(qba.qbuy.0, qba.qsell.0))
            .filter(|qba| *qba != 0u64)
            .max_by(|d1, d2| d1.cmp(d2));
        if let Some(max_qty_cross) = some_max_qty_cross {
            AuctionLevelsInProcess(
                self.limit
                    .iter()
                    .filter(|(_price, qba)| std::cmp::min(qba.qbuy.0, qba.qsell.0) == max_qty_cross)
                    .map(|(&price, b_a)| Level::new(price, b_a.clone()))
                    .collect::<Vec<_>>(),
            )
        } else {
            AuctionLevelsInProcess(vec![])
        }
    }
}

/// Provisional data while processing the rules to get the auction
/// It will provide a fluent API
#[derive(Debug)]
pub struct AuctionLevelsInProcess(Vec<Level>);

impl AuctionLevelsInProcess {
    fn min_diffqty_levels(&self) -> AuctionLevelsInProcess {
        let some_min_abs_diff = self
            .0
            .iter()
            .map(
                |Level {
                     price: _,
                     qty_buysell,
                 }| {
                    (qty_buysell.qbuy.0 as i64 - qty_buysell.qsell.0 as i64).abs()
                },
            )
            .min();

        AuctionLevelsInProcess(if let Some(min_abs_diff) = some_min_abs_diff {
            self.0
                .iter()
                .filter(
                    |Level {
                         price: _,
                         qty_buysell,
                     }| {
                        (qty_buysell.qbuy.0 as i64 - qty_buysell.qsell.0 as i64).abs()
                            == min_abs_diff
                    },
                )
                .cloned()
                .collect::<Vec<_>>()
        } else {
            vec![]
        })
    }
    fn max_qty_levels(&self) -> AuctionLevelsInProcess {
        let some_max_qty = self
            .0
            .iter()
            .map(
                |Level {
                     price: _,
                     qty_buysell,
                 }| {
                    std::cmp::max(qty_buysell.qbuy.0 as i64, qty_buysell.qsell.0 as i64)
                },
            )
            .max();

        AuctionLevelsInProcess(if let Some(max_qty) = some_max_qty {
            self.0
                .iter()
                .filter(
                    |Level {
                         price: _,
                         qty_buysell,
                     }| {
                        qty_buysell.qbuy.0 as i64 == max_qty
                            || qty_buysell.qsell.0 as i64 == max_qty
                    },
                )
                .cloned()
                .collect::<Vec<_>>()
        } else {
            vec![]
        })
    }

    /// if price < min prices in levels, auction = min_price_auction
    /// if price > min prices in levels, auction = max_price_auction
    /// else, Auction::new(price, min_prices.qty)!!!
    fn end_resolve_ref_price(&self, price: Price) -> Option<Auction> {
        let min_max: Option<(Auction, Auction)> =
            self.0
                .iter()
                .fold(None, |acc, e| match (acc, e.get_auction()) {
                    (None, None) => None,
                    (Some(a), None) => Some(a),
                    (None, Some(auction)) => Some((auction, auction)),
                    (Some(min_max), Some(new_auction)) => {
                        match (
                            new_auction.price < min_max.0.price,
                            new_auction.price > min_max.1.price,
                        ) {
                            (true, false) => Some((new_auction, min_max.1)),
                            (false, true) => Some((min_max.1, new_auction)),
                            _ => Some(min_max),
                        }
                    }
                });

        if let Some((min, max)) = min_max {
            match (price < min.price, price > max.price) {
                (true, true) => unreachable!("price cannot be less and bigger to min and max"),
                (true, false) => Some(min),
                (false, true) => Some(max),
                (false, false) => Some(Auction::new(price, min.quantity)),
            }
        } else {
            None
        }
    }
    //     let some_min_dif_ref_price = self
    //         .0
    //         .iter()
    //         .map(|Level{price, qty_buysell}| (price.0 - price.0).abs())
    //         .min();

    //     AuctionLevelsInProcess(if let Some(min_dif_ref_price) = some_min_dif_ref_price {
    //         self.0
    //             .iter()
    //             .filter(|Level(p, _qba)| (p.0 - price.0).abs() == min_dif_ref_price)
    //             .cloned()
    //             .collect::<Vec<_>>()
    //     } else {
    //         vec![]
    //     })
    // }

    //  fn get_first_level(&self) -> Option<(Price, Qty)> {
    //     let result = self.0.first();
    //     match result {
    //         Some(Level(price, qty_buy_sell)) => Some((
    //             *price,
    //             std::cmp::min(Qty(qty_buy_sell.qbuy.0), Qty(qty_buy_sell.qsell.0)),
    //         )),
    //         None => None,
    //     }
    // }

    // fn get_levels(&self) -> Vec<Level> {
    //     self.0.clone()
    // }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test_adding_simple() {
//         let ob1 = OrderBookAcc::new()
//             .add_limit_order(Side::Buy, Price(4), Qty(1))
//             //
//             .add_limit_order(Side::Sell, Price(10), Qty(1));

//         let ob2 = OrderBookAcc::new()
//             .add_limit_order(Side::Sell, Price(10), Qty(1))
//             .add_limit_order(Side::Buy, Price(4), Qty(1));

//         assert_eq!(ob1, ob2);
//     }

//     #[test]
//     fn test_adding_order() {
//         let ob1 = OrderBookAcc::new()
//             .add_limit_order(Side::Buy, Price(4), Qty(1))
//             .add_limit_order(Side::Buy, Price(10), Qty(1))
//             .add_limit_order(Side::Buy, Price(20), Qty(1))
//             .add_limit_order(Side::Buy, Price(30), Qty(1))
//             //
//             .add_limit_order(Side::Sell, Price(10), Qty(1))
//             .add_limit_order(Side::Sell, Price(20), Qty(1))
//             .add_limit_order(Side::Sell, Price(30), Qty(1))
//             //
//             .add_market_order(Side::Buy, Qty(1))
//             .add_market_order(Side::Buy, Qty(1))
//             .add_market_order(Side::Buy, Qty(1))
//             .add_market_order(Side::Sell, Qty(1))
//             // .add_market_order(Side::Sell, Qty(1));
//             ;

//         let ob2 = OrderBookAcc::new()
//             .add_limit_order(Side::Sell, Price(10), Qty(1))
//             .add_limit_order(Side::Buy, Price(10), Qty(1))
//             .add_limit_order(Side::Buy, Price(20), Qty(1))
//             .add_limit_order(Side::Sell, Price(20), Qty(1))
//             .add_market_order(Side::Buy, Qty(1))
//             .add_limit_order(Side::Sell, Price(30), Qty(1))
//             .add_limit_order(Side::Buy, Price(30), Qty(1))
//             .add_limit_order(Side::Buy, Price(4), Qty(1))
//             .add_market_order(Side::Buy, Qty(1))
//             .add_market_order(Side::Buy, Qty(1))
//             .add_market_order(Side::Sell, Qty(1))
//             //
//             // .add_market_order(Side::Sell, Qty(1));
//             ;

//         assert_eq!(ob1, ob2);
//     }
// }

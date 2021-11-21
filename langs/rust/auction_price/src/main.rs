extern crate auction;

use auction::*;

fn main() {
    // let auction_levels = get_example_order_book()
    //     .get_more_qty_exec_levels()
    //     .get_min_diff_levels()
    //     .get_max_qty_levels()
    //     .get_proxim2_levels(Price(60));

    // println!("{:#?}", auction_levels);
    let result = get_example_order_book().resolve(&Price(60));
    println!("{:#?}", result);
}

//  ----------------------

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

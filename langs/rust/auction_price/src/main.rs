extern crate auction;

#[cfg(test)]
use auction::*;

fn main() {
    println!("run cargo test...")
    // trivial_pt();
}

#[test]
fn one_case() {
    let auction = get_example_order_book().auction(Price(60));
    println!("{:#?}", auction);
    assert_eq!(auction.unwrap(), Auction::new(Price(50), Qty(8)));
}

#[test]
fn trivial_pt() {
    //  only one order buy, one sell, same price, same qty
    for _ in 0..100_000 {
        let price = get_random_price();
        let qty = get_random_quantity();

        println!("{:#?}", (price, qty));
        let res = OrderBookAcc::new()
            .add_limit_order(Side::Buy, price, qty)
            .add_limit_order(Side::Sell, price, qty)
            .auction(price)
            .unwrap();
        assert_eq!(res, Auction::new(price, qty))
    }
}

//  ----------------------

#[cfg(test)]
fn get_random_price() -> Price {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    Price(rng.gen_range(-100_000..100_000))
}

#[cfg(test)]
fn get_random_quantity() -> Qty {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    Qty(rng.gen_range(1..100_000))
}

#[cfg(test)]
fn get_example_order_book() -> OrderBookAcc {
    //  zeida ejemplo
    // OrderBookAcc::new()
    //     .add_limit_order(Side::Buy, Price(65), Qty(100))
    //     .add_limit_order(Side::Buy, Price(64), Qty(15))
    //     .add_market_order(Side::Buy, Qty(50))
    //     .add_limit_order(Side::Sell, Price(66), Qty(80))
    //     .add_limit_order(Side::Sell, Price(62), Qty(50))
    //     .add_market_order(Side::Sell, Qty(170))

    OrderBookAcc::new()
        .add_limit_order(Side::Buy, Price(4), Qty(1))
        .add_limit_order(Side::Buy, Price(10), Qty(2))
        .add_limit_order(Side::Buy, Price(20), Qty(3))
        .add_limit_order(Side::Buy, Price(30), Qty(1))
        .add_limit_order(Side::Buy, Price(50), Qty(5))
        //
        .add_limit_order(Side::Sell, Price(4), Qty(1))
        .add_limit_order(Side::Sell, Price(10), Qty(4))
        .add_limit_order(Side::Sell, Price(20), Qty(1))
        .add_limit_order(Side::Sell, Price(50), Qty(1))
        //
        .add_market_order(Side::Buy, Qty(1))
        .add_market_order(Side::Buy, Qty(1))
        .add_market_order(Side::Buy, Qty(1))
        .add_market_order(Side::Sell, Qty(1))
        .add_market_order(Side::Sell, Qty(1))
}

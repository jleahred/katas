extern crate auction;
use std::io::Write;

use auction::*;

macro_rules! run {
    ($fname:ident) => {
        run($fname, stringify!($fname));
    };
}

const ITERATIONS: u32 = 100_000;

fn main() {
    // let orders = test::gen_orders_auction_suffled_with_market(Price(100), Qty(3), Qty(4), Qty(1), Qty(2));
    // println!("{:#?}", orders);

    one_case(); //  simple example

    println!("running property tests...");
    run!(pt_multi_limit);
    run!(pt_trivial_with_market_multiorders_suffle);
    run!(pt_trivial);
    run!(pt_trivial_with_market);
}

fn run<F>(f: F, name: &str)
where
    F: FnOnce(),
{
    print!("running   {}...", name);
    std::io::stdout().flush().unwrap();
    f();
    println!(" OK");
}

fn one_case() {
    let auction = test::get_example_order_book().auction(Price(60));
    println!("{:#?}", auction);
    assert_eq!(auction.unwrap(), Auction::new(Price(50), Qty(8)));
}

// macro_rules! function {
//     () => {{
//         fn f() {}
//         fn type_name_of<T>(_: T) -> &'static str {
//             std::any::type_name::<T>()
//         }
//         let name = type_name_of(f);
//         &name[..name.len() - 3]
//     }};
// }

fn pt_trivial() {
    //  only one order buy, one sell, same price, diff qtys
    for _ in 0..ITERATIONS {
        let price = test::get_random_price();
        let qty1 = test::get_random_quantity();
        let qty2 = test::get_random_quantity();

        // println!("{:#?}", (price, qty));
        let res = OrderBookAcc::new()
            .add_order(&Order::new_limit(Side::Buy, price, qty1))
            .add_order(&Order::new_limit(Side::Sell, price, qty2))
            .auction(price)
            .unwrap();
        assert_eq!(res, Auction::new(price, std::cmp::min(qty1, qty2)))
    }
}

fn pt_trivial_with_market() {
    //  only one order buy, one sell, one as market, diff qtys
    for _ in 0..ITERATIONS {
        let price = test::get_random_price();
        let qty1 = test::get_random_quantity();
        let qty2 = test::get_random_quantity();

        let res = OrderBookAcc::new()
            .add_order(&Order::new_limit(Side::Buy, price, qty1))
            .add_order(&Order::new_market(Side::Sell, qty2))
            .auction(price)
            .unwrap();
        assert_eq!(res, Auction::new(price, std::cmp::min(qty1, qty2)));

        let res = OrderBookAcc::new()
            .add_order(&Order::new_limit(Side::Sell, price, qty1))
            .add_order(&Order::new_market(Side::Buy, qty2))
            .auction(price)
            .unwrap();
        assert_eq!(res, Auction::new(price, std::cmp::min(qty1, qty2)));
    }
}

fn pt_trivial_with_market_multiorders_suffle() {
    for _ in 0..ITERATIONS {
        let price = test::get_random_price();
        let limit_qty_buy = test::get_random_quantity();
        let limit_qty_sell = test::get_random_quantity();
        let market_qty_buy = test::get_random_quantity();
        let market_qty_sell = test::get_random_quantity();

        let orders = test::gen_orders_auction_suffled_with_market(
            price,
            limit_qty_buy,
            limit_qty_sell,
            market_qty_buy,
            market_qty_sell,
        );

        let auction = orders
            .iter()
            .fold(OrderBookAcc::new(), |acc, order| acc.add_order(order))
            .auction(test::get_random_price())
            .unwrap();

        assert_eq!(
            auction,
            Auction::new(
                price,
                Qty(std::cmp::min(
                    limit_qty_buy.0 + market_qty_buy.0,
                    limit_qty_sell.0 + market_qty_sell.0
                ))
            )
        );
    }
}

fn pt_multi_limit() {
    use rand::seq::SliceRandom;
    use rand::thread_rng;

    //  first, generate auction with a price
    //  later, add several orders outside this price
    //  shuffle
    //  check auction
    for _ in 0..ITERATIONS {
        let auction_price = test::get_random_price();
        let limit_qty_buy = test::get_random_quantity();
        let limit_qty_sell = test::get_random_quantity();

        //  gen orders auction
        let orders = test::gen_orders_auction_suffled_only_limit(
            auction_price,
            limit_qty_buy,
            limit_qty_sell,
        );

        //  gen orders random orders outside
        let orders = (0..1_000).into_iter().fold(orders, |mut acc, _| {
            acc.push(test::random_order_buy_under_price(auction_price));
            acc
        });
        // let orders = (0..1_000).into_iter().fold(orders, |mut acc, _| {
        //     acc.push(test::random_order_sell_over_price(auction_price));
        //     acc
        // });
        //  shuffle orders
        let orders = (|mut orders: Vec<Order>| {
            orders.shuffle(&mut thread_rng());
            orders
        })(orders);

        //  add orders and check
        let auction = orders
            .iter()
            .fold(OrderBookAcc::new(), |acc, order| acc.add_order(order))
            .auction(test::get_random_price())
            .unwrap();

        //  check
        assert_eq!(
            auction,
            Auction::new(auction_price, std::cmp::min(limit_qty_buy, limit_qty_sell))
        );
    }
}

//  ----------------------
mod test {
    use auction::*;

    pub fn gen_orders_auction_suffled_with_market(
        price: Price,
        limit_qty_buy: Qty,
        limit_qty_sell: Qty,
        market_qty_buy: Qty,
        market_qty_sell: Qty,
    ) -> Vec<Order> {
        use rand::seq::SliceRandom;
        use rand::thread_rng;

        let orders = vec![];
        let orders = add_limit_orders(orders, Side::Buy, price, limit_qty_buy);
        let orders = add_limit_orders(orders, Side::Sell, price, limit_qty_sell);
        let orders = add_market_orders(orders, Side::Buy, market_qty_buy);
        let orders = add_market_orders(orders, Side::Sell, market_qty_sell);
        let orders = (|mut orders: Vec<Order>| {
            orders.shuffle(&mut thread_rng());
            orders
        })(orders);
        orders
    }

    pub fn gen_orders_auction_suffled_only_limit(
        price: Price,
        limit_qty_buy: Qty,
        limit_qty_sell: Qty,
    ) -> Vec<Order> {
        use rand::seq::SliceRandom;
        use rand::thread_rng;

        let orders = vec![];
        let orders = add_limit_orders(orders, Side::Buy, price, limit_qty_buy);
        let orders = add_limit_orders(orders, Side::Sell, price, limit_qty_sell);
        let orders = (|mut orders: Vec<Order>| {
            orders.shuffle(&mut thread_rng());
            orders
        })(orders);
        orders
    }

    pub fn add_limit_orders(
        mut orders: Vec<Order>,
        side: Side,
        price: Price,
        limit_qty_buy: Qty,
    ) -> Vec<Order> {
        let mut total_to_distribute = limit_qty_buy.0;
        while total_to_distribute > 0 {
            let qty = get_random_quantity_max(total_to_distribute);
            total_to_distribute -= qty.0;
            orders.push(Order::new_limit(side, price, qty));
        }
        orders
    }

    pub fn add_market_orders(mut orders: Vec<Order>, side: Side, limit_qty_buy: Qty) -> Vec<Order> {
        let mut total_to_distribute = limit_qty_buy.0;
        while total_to_distribute > 0 {
            let qty = get_random_quantity_max(total_to_distribute);
            total_to_distribute -= qty.0;
            orders.push(Order::new_market(side, qty));
        }
        orders
    }

    pub fn random_order_buy_under_price(price: Price) -> Order {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        let price = Price(rng.gen_range((price.0 - 100_000)..(price.0 - 100)));
        Order::new_limit(Side::Buy, price, get_random_quantity())
    }
    pub fn random_order_sell_over_price(price: Price) -> Order {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        let price = Price(rng.gen_range((price.0 + 1)..(price.0 + 100_000)));
        Order::new_limit(Side::Sell, price, get_random_quantity())
    }
    pub fn get_random_price() -> Price {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        Price(rng.gen_range(-100_000..100_000))
    }

    pub fn get_random_quantity() -> Qty {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        Qty(rng.gen_range(1..100_000))
    }

    pub fn get_random_quantity_max(max: u64) -> Qty {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        Qty(rng.gen_range(1..=max))
    }

    pub fn get_example_order_book() -> OrderBookAcc {
        //  zeida ejemplo
        // OrderBookAcc::new()
        //     .add_limit_order(Side::Buy, Price(65), Qty(100))
        //     .add_limit_order(Side::Buy, Price(64), Qty(15))
        //     .add_market_order(Side::Buy, Qty(50))
        //     .add_limit_order(Side::Sell, Price(66), Qty(80))
        //     .add_limit_order(Side::Sell, Price(62), Qty(50))
        //     .add_market_order(Side::Sell, Qty(170))

        OrderBookAcc::new()
            .add_order(&Order::new_limit(Side::Buy, Price(4), Qty(1)))
            .add_order(&Order::new_limit(Side::Buy, Price(10), Qty(2)))
            .add_order(&Order::new_limit(Side::Buy, Price(20), Qty(3)))
            .add_order(&Order::new_limit(Side::Buy, Price(30), Qty(1)))
            .add_order(&Order::new_limit(Side::Buy, Price(50), Qty(5)))
            //
            .add_order(&Order::new_limit(Side::Sell, Price(4), Qty(1)))
            .add_order(&Order::new_limit(Side::Sell, Price(10), Qty(4)))
            .add_order(&Order::new_limit(Side::Sell, Price(20), Qty(1)))
            .add_order(&Order::new_limit(Side::Sell, Price(50), Qty(1)))
            //
            .add_order(&Order::new_market(Side::Buy, Qty(1)))
            .add_order(&Order::new_market(Side::Buy, Qty(1)))
            .add_order(&Order::new_market(Side::Buy, Qty(1)))
            .add_order(&Order::new_market(Side::Sell, Qty(1)))
            .add_order(&Order::new_market(Side::Sell, Qty(1)))
    }
}

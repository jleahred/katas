extern crate parkit;
use parkit::expr::builders::*;
use parkit::*;

fn main() {
    println!("Hello, world!");

    let rules = rules! {
           "main" =>    rule!(
                            or!(
                                and!(
                                    ref_rule("main"),
                                    // lit("a"),
                                    lit("a")
                                ),
                                lit("a")
                            )
                        )
    };

    // let r = parse("aaaaa", &rules).ok().unwrap();
    // print!("{:?}", r);

    // let p = parse("a", &rules);
    // print!("{:?}", p);
    // let p = parse("aaaa", &rules);
    // print!("{:?}", p);
    // let p = parse("aaaaaaa", &rules);
    // print!("{:?}", p);
    // let p = parse("aaaaaaaaaaaaaaaa", &rules);
    // print!("{:?}", p);
    let p = parse("aaaaaa", &rules);
    print!("{:?}", p);
}

mod config;
#[macro_use]
mod side;
mod cube;



#[cfg_attr(test, allow(dead_code))]
fn main() {
    let cube = cube::create(
                                &side::color(0),
            &side::color(1),    &side::color(2),    &side::color(3),
                                &side::color(4),
                                &side::color(5),
        );
    println!("{:?}", cube);

    println!("\n\n{}", cube);
}

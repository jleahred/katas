mod config;
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

    {
        use cube::rotation::*;

        let mv = cube::rotation::Item(Orientation::Horizontal, Direction(true), 0);
        println!("{}", mv);

        let rotationd1 = cube::rotation_horizontal(&cube, Direction(true), 0);
        println!("{}", rotationd1);

        //let rotationd2 = cube::rotation_horizontal(&rotationd1, Direction(false), 0);
        //println!("{}", rotationd2);
        let rotationd2 = cube::rotation_horizontal(&rotationd1, Direction(true), 1);
        println!("{}", rotationd2);

        let rotationd3 = cube::rotation_vertical(&cube, Direction(true), 1);
        println!("{}", rotationd3);

        let rotationd4 = cube::rotation_horizontal(&rotationd3, Direction(true), 1);
        println!("{}", rotationd4);
    }
}

mod config;
mod side;
mod cube;



#[cfg_attr(test, allow(dead_code))]
fn main() {

    println!("Heyyyy {}", ('0' as u8) - ('0' as u8));


    let cube_a = cube::create_from_strings(
                     ["0000",
                      "0000",
                      "0000",
                      "0000",
                 "1111 2222 3333",
                 "1111 2222 3333",
                 "1111 2222 3333",
                 "1111 2222 3333",
                      "4444",
                      "4444",
                      "4444",
                      "4444",

                      "5555",
                      "5555",
                      "5555",
                      "5555"]
        );

    println!("{}", cube_a);

    /*
    let cube = cube::create(
                                &side::color(0),
            &side::color(1),    &side::color(2),    &side::color(3),
                                &side::color(4),
                                &side::color(5),
        );
    println!("{:?}", cube);

    println!("\n\n{}", cube);


    {
        use cube::rotation;
        use cube::rotation::{Orientation, Direction};

        let rotation1 = rotation::process(&cube, &rotation::Item(Orientation::Horizontal, Direction::Plus, 0));
        let rotation2 = rotation::process(&rotation1, &rotation::Item(Orientation::Vertical, Direction::Minus, 2));
        println!("{}", rotation2);
    }

    {
        use cube::rotation;
        use cube::rotation::{Orientation, Direction};

        println!("{}",
            &cube.get_rotation(&rotation::Item(Orientation::Horizontal, Direction::Plus, 0))
                 .get_rotation(&rotation::Item(Orientation::Vertical, Direction::Minus, 2))
        );
    }
*/
/*
    {
        use cube::rotation::*;

        let mv = cube::rotation::Item(Orientation::Horizontal, Direction::Plus, 0);
        println!("{}", mv);

        let rotationd1 = cube::rotation::horizontal(&cube, Direction::Plus, 0);
        println!("{}", rotationd1);

        //let rotationd2 = cube::rotation::horizontal(&rotationd1, Direction(false), 0);
        //println!("{}", rotationd2);
        let rotationd2 = cube::rotation::horizontal(&rotationd1, Direction::Plus, 1);
        println!("{}", rotationd2);

        let rotationd3 = cube::rotation::vertical(&cube, Direction::Plus, 1);
        println!("{}", rotationd3);

        let rotationd4 = cube::rotation::horizontal(&rotationd3, Direction::Plus, 1);
        println!("{}", rotationd4);
    }

    {
        use cube::rotation::*;

        let rotation1 = cube::rotation::vertical(&cube, Direction::Plus, 0);
        println!("{}", rotation1);

        let rotation2 = cube::rotation::horizontal(&rotation1, Direction::Plus, 0);
        println!("{}", rotation2);
    }
    {
        use cube::rotation::*;

        let rotation1 = cube::rotation::front(&cube, Direction::Plus, 0);
        println!("{}", rotation1);
    }
    {
        use cube::rotation::*;

        let rotation1 = cube::rotation::front(&cube, Direction::Minus, 2);
        println!("{}", rotation1);
    }
    {
        use cube::rotation::*;

        let rotation1 = cube::rotation::vertical(&cube, Direction::Plus, 0);
        let rotation2 = cube::rotation::front(&rotation1, Direction::Plus, 0);
        println!("{}", rotation2);
    }
    */
}

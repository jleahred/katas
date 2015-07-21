mod config;
mod side;
mod cube;
mod tree;



#[cfg_attr(test, allow(dead_code))]
fn main() {

    {
        let init : cube::Sides = cube::create_from_strings(
                         ["0000",
                          "0000",
                          "0000",
                          "0000",
                     "0001 0000 0000",
                     "0002 0000 0000",
                     "0003 0000 0000",
                     "0004 0000 0000",
                          "0000",
                          "0000",
                          "0000",
                          "0000",

                          "0000",
                          "0000",
                          "0000",
                          "0000"]
            );

        let end : cube::Sides = cube::create_from_strings(
                        ["0000",
                         "0000",
                         "0000",
                         "0000",
                    "0000 0000 0000",
                    "0000 0000 0000",
                    "0000 0000 0000",
                    "0000 0000 0000",
                         "1234",
                         "0004",
                         "0000",
                         "0000",

                         "0000",
                         "0000",
                         "0000",
                         "0000"]
                    );

        let result = tree::explore(&init, &end, 4);
        println!("{}", result);


        {
            for depth  in 1..16 {
                let mut iterations = 1f64+24f64;
                for j in 0..depth - 1 {
                    iterations *= 24f64;
                    iterations += 1f64;
                };
                iterations *= 24f64;
                println!("depth: {} -> moves: {}", depth+1, iterations);
            }
        }
    }

/*
    {
        let init : cube::Sides = cube::create_from_strings(
                         ["0000",
                          "0000",
                          "0000",
                          "0000",
                     "0001 0000 0000",
                     "0002 0000 0000",
                     "0003 0000 0000",
                     "0004 0000 0000",
                          "0000",
                          "0000",
                          "0000",
                          "1234",

                          "0000",
                          "0000",
                          "0000",
                          "0000"]
            );

        let end : cube::Sides = cube::create_from_strings(
                        ["4321",
                         "0000",
                         "0000",
                         "4321",
                    "0000 0000 0000",
                    "0000 0000 0000",
                    "0000 0000 0000",
                    "0000 0000 0000",
                         "0000",
                         "0000",
                         "0000",
                         "0000",

                         "0000",
                         "0000",
                         "0000",
                         "0000"]
                    );

        let result = tree::explore(&init, &end, 3);
        println!("{}", result);
    }
    */
    /*
    {
        let init : cube::Sides = cube::create_from_strings(
                         ["0000",
                          "0000",
                          "0000",
                          "0000",
                     "0001 0000 0000",
                     "0002 0000 0000",
                     "0003 0000 0000",
                     "0004 0000 0000",
                          "0000",
                          "0000",
                          "0000",
                          "0000",

                          "0000",
                          "0000",
                          "0000",
                          "0000"]
            );

        let end : cube::Sides = cube::create_from_strings(
                        ["0000",
                         "0000",
                         "0000",
                         "0000",
                    "0000 0000 0000",
                    "0000 0000 0000",
                    "0000 0000 0000",
                    "0000 0000 0000",
                         "0000",
                         "0000",
                         "0000",
                         "0000",

                         "0000",
                         "0000",
                         "0000",
                         "0000"]
                    );

        let result = tree::explore(&init, &end, 4);
        println!("{}", result);
    }
    */

/*
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
*/
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
        use cube::rot;
        use cube::rot::{Orient, Dir};

        let rotation1 = rot::process(&cube, &rot::Item(Orient::Horizontal, Dir::Plus, 0));
        let rotation2 = rot::process(&rotation1, &rot::Item(Orient::Vertical, Dir::Minus, 2));
        println!("{}", rotation2);
    }

    {
        use cube::rotation;
        use cube::rot::{Orient, Dir};

        println!("{}",
            &cube.get_rotation(&rot::Item(Orient::Horizontal, Dir::Plus, 0))
                 .get_rotation(&rot::Item(Orient::Vertical, Dir::Minus, 2))
        );
    }
*/
/*
    {
        use cube::rot::*;

        let mv = cube::rot::Item(Orient::Horizontal, Dir::Plus, 0);
        println!("{}", mv);

        let rotationd1 = cube::rot::horizontal(&cube, Dir::Plus, 0);
        println!("{}", rotationd1);

        //let rotationd2 = cube::rot::horizontal(&rotationd1, Dir(false), 0);
        //println!("{}", rotationd2);
        let rotationd2 = cube::rot::horizontal(&rotationd1, Dir::Plus, 1);
        println!("{}", rotationd2);

        let rotationd3 = cube::rot::vertical(&cube, Dir::Plus, 1);
        println!("{}", rotationd3);

        let rotationd4 = cube::rot::horizontal(&rotationd3, Dir::Plus, 1);
        println!("{}", rotationd4);
    }

    {
        use cube::rot::*;

        let rotation1 = cube::rot::vertical(&cube, Dir::Plus, 0);
        println!("{}", rotation1);

        let rotation2 = cube::rot::horizontal(&rotation1, Dir::Plus, 0);
        println!("{}", rotation2);
    }
    {
        use cube::rot::*;

        let rotation1 = cube::rot::front(&cube, Dir::Plus, 0);
        println!("{}", rotation1);
    }
    {
        use cube::rot::*;

        let rotation1 = cube::rot::front(&cube, Dir::Minus, 2);
        println!("{}", rotation1);
    }
    {
        use cube::rot::*;

        let rotation1 = cube::rot::vertical(&cube, Dir::Plus, 0);
        let rotation2 = cube::rot::front(&rotation1, Dir::Plus, 0);
        println!("{}", rotation2);
    }
    */
}

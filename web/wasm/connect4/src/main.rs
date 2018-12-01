extern crate connect4;
extern crate yew;

use connect4::Model;
use yew::prelude::*;

fn main() {
    yew::initialize();
    App::<Model>::new().mount_to_body();
    yew::run_loop();
}

// extern crate connect4;

// mod engine;
// use engine::board;

// fn main() {
//     let board = board::board_from_string(
//         "
//         _______
//         _______
//         __O____
//         __O____
//         __OX___
//         __OOXX_
// ",
//     ).unwrap();

//     let bstring = format!("{:?}", board);
//     let new_board = board::board_from_string(&bstring).unwrap();

//     assert!(board == new_board);

//     println!("{:?}", board);
// }

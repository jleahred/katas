// extern crate connect4;
// extern crate yew;

// use connect4::Model;
// use yew::prelude::*;

// fn main() {
//     yew::initialize();
//     App::<Model>::new().mount_to_body();
//     yew::run_loop();
// }

extern crate connect4;

use connect4::engine::*;

fn main() {
    let game = Game::new(Player::X);

    let play_col = |game: Game, col| -> Game {
        match game.play(Col::new(col).unwrap()) {
            Ok(game) => game,
            _ => panic!("error processing move"),
        }
    };

    let game = play_col(game, 5);
    let game = play_col(game, 5);
    let game = play_col(game, 5);
    let game = play_col(game, 5);
    let game = play_col(game, 5);
    let game = play_col(game, 5);

    let egame = game.play(Col::new(5).unwrap());

    assert!(egame.is_err())
}

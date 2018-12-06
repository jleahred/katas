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
    let play_col = |game: Game, col| -> Game {
        match game.play(Col::new(col).unwrap()) {
            Ok(game) => game,
            _ => panic!("error processing move"),
        }
    };

    {
        let game = Game::new(Player::O);

        let game = play_col(game, 0);
        let game = play_col(game, 1);
        let game = play_col(game, 2);
        let game = play_col(game, 3);
        let game = play_col(game, 4);

        let game = play_col(game, 0);
        let game = play_col(game, 1);
        let game = play_col(game, 2);
        let game = play_col(game, 3);
        let game = play_col(game, 4);

        let game = play_col(game, 0);
        let game = play_col(game, 1);
        let game = play_col(game, 2);
        let game = play_col(game, 3);
        let game = play_col(game, 4);

        let game = play_col(game, 0);

        println!("{}", game);
        assert!(game.turn == Turn::Won(Player::X))
    }

    {
        let game = Game::new(Player::O);

        let game = play_col(game, 0);
        let game = play_col(game, 1);
        let game = play_col(game, 2);
        let game = play_col(game, 3);
        let game = play_col(game, 4);

        let game = play_col(game, 0);
        let game = play_col(game, 1);
        let game = play_col(game, 2);
        let game = play_col(game, 3);
        let game = play_col(game, 4);

        let game = play_col(game, 0);
        let game = play_col(game, 1);
        let game = play_col(game, 2);
        let game = play_col(game, 3);
        let game = play_col(game, 4);

        let game = play_col(game, 6);

        let game = play_col(game, 0);
        let game = play_col(game, 1);
        let game = play_col(game, 2);
        let game = play_col(game, 3);
        let game = play_col(game, 6);
        let game = play_col(game, 4);

        println!("{}", game);
        assert!(game.turn == Turn::Won(Player::X))
    }
}

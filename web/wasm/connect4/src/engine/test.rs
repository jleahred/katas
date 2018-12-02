use crate::engine::{Col, Game, Player, Turn};

#[test]
fn test_4_in_line() {
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
        let game = play_col(game, 0);
        let game = play_col(game, 1);
        let game = play_col(game, 0);
        let game = play_col(game, 1);
        let game = play_col(game, 0);

        // println!("{}", game);
        assert!(game.turn == Turn::Won(Player::O))
    }

    {
        let game = Game::new(Player::X);

        let game = play_col(game, 2);
        let game = play_col(game, 5);
        let game = play_col(game, 2);
        let game = play_col(game, 5);
        let game = play_col(game, 5);
        let game = play_col(game, 6);
        let game = play_col(game, 5);
        let game = play_col(game, 6);
        let game = play_col(game, 5);
        let game = play_col(game, 6);
        let game = play_col(game, 5);

        // println!("{}", game)
        assert!(game.turn == Turn::Won(Player::X))
    }
    {
        let game = Game::new(Player::O);

        let game = play_col(game, 2);
        let game = play_col(game, 2);
        let game = play_col(game, 3);
        let game = play_col(game, 3);
        let game = play_col(game, 4);
        let game = play_col(game, 4);
        let game = play_col(game, 5);

        // println!("{}", game)
        assert!(game.turn == Turn::Won(Player::O))
    }
    {
        let game = Game::new(Player::O);

        let game = play_col(game, 0);
        let game = play_col(game, 0);
        let game = play_col(game, 1);
        let game = play_col(game, 1);
        let game = play_col(game, 2);
        let game = play_col(game, 2);
        let game = play_col(game, 3);

        // println!("{}", game)
        assert!(game.turn == Turn::Won(Player::O))
    }
    {
        let game = Game::new(Player::O);

        let game = play_col(game, 3);
        let game = play_col(game, 3);
        let game = play_col(game, 4);
        let game = play_col(game, 4);
        let game = play_col(game, 5);
        let game = play_col(game, 5);
        let game = play_col(game, 6);

        // println!("{}", game)
        assert!(game.turn == Turn::Won(Player::O))
    }
}

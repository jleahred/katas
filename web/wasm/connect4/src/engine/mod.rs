/// Abstract type with game status
///
pub struct Game {
    pub board: Board,
    pub next2move: Next2Move,
}

/// Next player to move, or winner of game
///
pub enum Next2Move {
    Next(Player),
    Won(Player),
}

/// Player options
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Player {
    O,
    X,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Col(u8);
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Row(u8);

const NCOLS: u8 = 7;
const NROWS: u8 = 6;

/// Abstract type to manage the board
#[derive(PartialEq, Eq)]
pub struct Board([[Cell; NCOLS as usize]; NROWS as usize]);

/// Two players, O and X
#[derive(PartialEq, Eq)]
pub enum Cell {
    P(Player),
    Empty,
}

impl Col {
    pub fn new(v: u8) -> Option<Self> {
        if v < NCOLS {
            Some(Col(v))
        } else {
            None
        }
    }
}

impl Row {
    pub fn new(v: u8) -> Option<Self> {
        if v < NROWS {
            Some(Row(v))
        } else {
            None
        }
    }
}

impl Game {
    /// ```rust
    /// extern crate connect4;
    ///
    /// use connect4::engine::*;
    ///
    /// fn main() {
    ///     let game = Game::new(Player::X);
    ///
    ///     let board = connect4::engine::board_from_string(
    ///         "
    ///          _______
    ///          _______
    ///          _______
    ///          _______
    ///          _______
    ///          _______
    ///         ",
    ///     );
    ///
    ///     println!("{}", game);
    ///
    ///     assert!(game.board == board.unwrap());
    ///     if let Next2Move::Next(next_player) = game.next2move {
    ///         assert!(next_player == Player::X)
    ///     } else {
    ///         panic!("error on next move")
    ///     }
    /// }
    /// ```
    pub fn new(start: Player) -> Game {
        Game {
            board: empty_board(),
            next2move: Next2Move::Next(start),
        }
    }

    /// Example with one movement
    ///
    /// ```rust
    /// extern crate connect4;
    ///
    /// use connect4::engine::*;
    ///
    /// fn main() {
    ///     let game = Game::new(Player::O);
    ///
    ///     let board = connect4::engine::board_from_string(
    ///         "
    ///          _______
    ///          _______
    ///          _______
    ///          _______
    ///          _______
    ///          O______
    ///         ",
    ///     );
    ///
    ///     let play_col = |game: Game, col| -> Game {
    ///         match game.play(Col::new(col).unwrap()) {
    ///             Ok(game) => game,
    ///             _ => panic!("error processing move"),
    ///         }
    ///     };
    ///
    ///     let game = play_col(game, 0);
    ///
    ///     println!("{}", game);
    ///
    ///     assert!(game.board == board.unwrap());
    ///     if let Next2Move::Next(next_player) = game.next2move {
    ///         assert!(next_player == Player::X)
    ///     } else {
    ///         panic!("error on next move")
    ///     }
    /// }
    /// ```
    ///
    /// Example with several moves
    ///
    /// ```rust
    /// extern crate connect4;
    ///
    /// use connect4::engine::*;
    ///
    /// fn main() {
    ///     let game = Game::new(Player::X);
    ///
    ///     let board = connect4::engine::board_from_string(
    ///         "
    ///          _______
    ///          _______
    ///          _____O_
    ///          _____X_
    ///          _____O_
    ///          _____X_
    ///         ",
    ///     );
    ///
    ///     let play_col = |game: Game, col| -> Game {
    ///         match game.play(Col::new(col).unwrap()) {
    ///             Ok(game) => game,
    ///             _ => panic!("error processing move"),
    ///         }
    ///     };
    ///
    ///     let game = play_col(game, 5);
    ///     let game = play_col(game, 5);
    ///     let game = play_col(game, 5);
    ///     let game = play_col(game, 5);
    ///
    ///     println!("{}", game);
    ///
    ///     assert!(game.board == board.unwrap());
    ///     if let Next2Move::Next(next_player) = game.next2move {
    ///         assert!(next_player == Player::X)
    ///     } else {
    ///         panic!("error on next move")
    ///     }
    /// }
    /// ```
    ///
    /// What if you try to play when column is exausted???
    /// You get an error
    ///
    /// ```rust
    /// extern crate connect4;
    ///
    /// use connect4::engine::*;
    ///
    /// fn main() {
    ///     let game = Game::new(Player::X);
    ///
    ///     let play_col = |game: Game, col| -> Game {
    ///         match game.play(Col::new(col).unwrap()) {
    ///             Ok(game) => game,
    ///             _ => panic!("error processing move"),
    ///         }
    ///     };
    ///
    ///     let game = play_col(game, 5);
    ///     let game = play_col(game, 5);
    ///     let game = play_col(game, 5);
    ///     let game = play_col(game, 5);
    ///     let game = play_col(game, 5);
    ///     let game = play_col(game, 5);
    ///
    ///     let egame = game.play(Col::new(5).unwrap());
    ///
    ///     assert!(egame.is_err())
    /// }
    /// ```
    
    pub fn play(mut self, col: Col) -> std::result::Result<Game, Game> {
        match (row_for_play(&self, col), next_player(&self)) {
            (Some(row), Some(player)) => {
                self.board.0[row.0 as usize][col.0 as usize] = Cell::P(player);
                self.change_next_player()
            }
            _ => Err(self),
        }
    }

    fn change_next_player(mut self) -> std::result::Result<Game, Game> {
        let switch_player = |player| match player {
            Player::O => Player::X,
            Player::X => Player::O,
        };
        if let Some(player) = next_player(&self) {
            self.next2move = Next2Move::Next(switch_player(player));
            Ok(self)
        } else {
            Err(self)
        }
    }
}

fn row_for_play(game: &Game, col: Col) -> Option<Row> {
    for i in 0..NROWS {
        let r = NROWS - i - 1;
        if game.board.0[r as usize][col.0 as usize] == Cell::Empty {
            return Some(Row(r)); //  I know, I know
        }
    }
    None
}

fn next_player(game: &Game) -> Option<Player> {
    if let Next2Move::Next(ref player) = game.next2move {
        Some(*player)
    } else {
        None
    }
}

/// 6 lines, spaces at begin or end of line
/// \n to separate lines
///
/// In next example, we test the symmetry
///     board -> string -> board
///     
/// To simplify we start with a string
///
/// >    string1 -> board1 -> string2 -> board2
///
/// board1 == board2
///
///
/// ```rust
/// extern crate connect4;
///
///
/// fn main() {
///     let board = connect4::engine::board_from_string(
///         "
///         _______
///         _______
///         __O____
///         __O____
///         __OX___
///         __OOXX_
/// ",
///     ).unwrap();
///
///     let bstring = format!("{}", board);
///     let new_board = connect4::engine::board_from_string(&bstring).unwrap();
///
///     assert!(board == new_board);
///
///     println!("{}", board);
/// }
/// ```
pub fn board_from_string(blines: &str) -> Option<Board> {
    let (_, board) = blines
        .lines()
        .try_fold((0, empty_board()), |(row, board), line| {
            let line = line.trim();
            if !line.is_empty() {
                let board = sline2board_row(board, line, row)?;
                Some((row + 1, board))
            } else {
                Some((row, board))
            }
        })?;

    Some(board)
}

impl std::fmt::Display for Game {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.board)?;
        write!(f, "{}", self.next2move)
    }
}

impl std::fmt::Display for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for row in 0..6 {
            for cell in self.0[row].iter() {
                write!(f, "{}", cell)?;
            }
            write!(f, "\n");
        }
        write!(f, "")
    }
}

impl std::fmt::Display for Next2Move {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Next2Move::Next(player) => write!(f, "next: {}", player),
            Next2Move::Won(player) => write!(f, "won: {}", player),
        }
    }
}

impl std::fmt::Display for Cell {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Cell::P(player) => write!(f, "{}", player),
            Cell::Empty => write!(f, "_"),
        }
    }
}

impl std::fmt::Display for Player {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Player::O => write!(f, "O"),
            Player::X => write!(f, "X"),
        }
    }
}

//  ---------------------
fn empty_board() -> Board {
    fn empty_row() -> [Cell; 7] {
        [
            Cell::Empty,
            Cell::Empty,
            Cell::Empty,
            Cell::Empty,
            Cell::Empty,
            Cell::Empty,
            Cell::Empty,
        ]
    }

    Board([
        empty_row(),
        empty_row(),
        empty_row(),
        empty_row(),
        empty_row(),
        empty_row(),
    ])
}

fn set_cell_on_board(mut br: Board, col: usize, row: usize, player: Cell) -> Option<Board> {
    if col < 7 && row < 6 {
        br.0[col][row] = player;
        Some(br)
    } else {
        None
    }
}

fn sline2board_row(board: Board, sline: &str, row: usize) -> Option<Board> {
    let (_, br) = sline.chars().try_fold((0, board), |(col, br), ch| {
        let br = match ch {
            '_' => Some(br),
            'O' => set_cell_on_board(br, row, col, Cell::P(Player::O)),
            'X' => set_cell_on_board(br, row, col, Cell::P(Player::X)),
            _ => None,
        }?;
        Some((col + 1, br))
    })?;

    Some(br)
}

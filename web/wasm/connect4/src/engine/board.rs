/// Abstract type to manage the board
#[derive(PartialEq, Eq)]
pub struct Board([[Cell; 7]; 6]);

/// Two players, O and X
#[derive(PartialEq, Eq)]
pub enum Cell {
    O,
    X,
    Empty,
}

/// 6 lines, spaces at begin or end of line
/// \n to separate lines
///
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

impl std::fmt::Debug for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for row in 0..6 {
            for cell in self.0[row].iter() {
                write!(f, "{:?}", cell)?;
            }
            write!(f, "\n");
        }
        write!(f, "")
    }
}

impl std::fmt::Debug for Cell {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Cell::O => write!(f, "O"),
            Cell::X => write!(f, "X"),
            Cell::Empty => write!(f, "_"),
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
            'O' => set_cell_on_board(br, row, col, Cell::O),
            'X' => set_cell_on_board(br, row, col, Cell::X),
            _ => None,
        }?;
        Some((col + 1, br))
    })?;

    Some(br)
}

/// four_in_line
///
/// next_player_wins (from thee_with_hole)
///
/// _OO__  &  __OO_
///
/// three_holes_consecutives_same_column
///
/// three_with_hole  ->  count & hole matrix
///
/// two with hole -> count & hole matrix
///
use super::*;

pub(crate) fn find_4line(board: &Board) -> bool {
    find_4line_horizontal(board)
        || find_4line_vertical(board)
        || find_4line_diagonal1(board)
        || find_4line_diagonal2(board)
}

fn find_4line_g(board: &Board, limitc: usize, limitr: usize, incc: usize, incr: usize) -> bool {
    for c in 0..(NCOLS as usize) - limitc {
        for r in 0..(NROWS as usize) - limitr {
            let cellr = board.get_cell_dang(c, r);
            if let Cell::P(_) = cellr {
                let mut count_matches = 0;
                for i in 1..4 {
                    if cellr != board.get_cell_dang(c + i * incc, r + i * incr) {
                        break;
                    } else {
                        count_matches += 1;
                    }
                }
                if count_matches == 3 {
                    return true;
                }
            }
        }
    }
    false
}

fn find_4line_vertical(board: &Board) -> bool {
    find_4line_g(board, 0, 3, 0, 1)
}

fn find_4line_horizontal(board: &Board) -> bool {
    find_4line_g(board, 3, 0, 1, 0)
}

fn find_4line_diagonal1(_board: &Board) -> bool {
    false
}

fn find_4line_diagonal2(_board: &Board) -> bool {
    false
}

/// four_in_line
///
/// next_player_wins (from thee_with_hole)
///
/// _OO__  &  __OO_    imposible_avoid
///
/// three_holes_consecutives_same_column
///
/// three_with_hole  ->  count & hole matrix
///
/// two with hole -> count & hole matrix
///
use super::*;

pub(crate) enum Possition {
    FourInLine,
    P(PosPatterns),
}

pub(crate) struct PosPatterns {
    _next_player_wins: u16,
    _imposible_avoid: u16,
    _vert_consecutive_hole_3inline: u16,
    _one_hole_for_connect4: u16,
    _two_holes_for_connect4: u16,
}

fn score_4cells(cl: &CellsLine, pos: Possition, _board: &Board) -> Possition {
    score_4inline(cl, pos, _board)
}

fn score_4inline(cl: &CellsLine, pos: Possition, board: &Board) -> Possition {
    let get_cell_from_coord =
        |cc: &CellsCoord, board: &Board| board.get_cell_dangerous(cc.col, cc.row);
    let control_cell = get_cell_from_coord(&cl.0[0], board);

    if let Cell::P(_) = control_cell {
        let mut count_matches = 0;
        for i in 1..4 {
            if control_cell != get_cell_from_coord(&cl.0[i], board) {
                break;
            } else {
                count_matches += 1;
            }
        }
        if count_matches == 3 {
            return Possition::FourInLine;
        }
    }
    pos
}

impl PosPatterns {
    fn init() -> Self {
        PosPatterns {
            _next_player_wins: 0,
            _imposible_avoid: 0,
            _vert_consecutive_hole_3inline: 0,
            _one_hole_for_connect4: 0,
            _two_holes_for_connect4: 0,
        }
    }
}

pub(crate) fn get_patterns(board: &Board) -> Possition {
    let bp = (board, Possition::P(PosPatterns::init()));
    let bp = scan_horiz(bp);
    let bp = scan_vert(bp);
    let bp = scan_diag1(bp);
    let bp = scan_diag2(bp);
    bp.1
}

fn scan_horiz(bp: (&Board, Possition)) -> (&Board, Possition) {
    scan(
        bp,
        &ScanConf {
            startc: 0,
            limitc: 3,
            limitr: 0,
            incc: 1,
            incr: 0,
        },
    )
}

fn scan_vert(bp: (&Board, Possition)) -> (&Board, Possition) {
    scan(
        bp,
        &ScanConf {
            startc: 0,
            limitc: 0,
            limitr: 3,
            incc: 0,
            incr: 1,
        },
    )
}
fn scan_diag1(bp: (&Board, Possition)) -> (&Board, Possition) {
    scan(
        bp,
        &ScanConf {
            startc: 0,
            limitc: 3,
            limitr: 3,
            incc: 1,
            incr: 1,
        },
    )
}
fn scan_diag2(bp: (&Board, Possition)) -> (&Board, Possition) {
    scan(
        bp,
        &ScanConf {
            startc: 3,
            limitc: 0,
            limitr: 3,
            incc: -1,
            incr: 1,
        },
    )
}

struct ScanConf {
    startc: usize,
    limitc: usize,
    limitr: usize,
    incc: i16,
    incr: i16,
}

struct CellsLine([CellsCoord; 4]);

struct CellsCoord {
    row: usize,
    col: usize,
}

fn scan<'a>(bp: (&'a Board, Possition), sc: &ScanConf) -> (&'a Board, Possition) {
    let board = bp.0;
    let mut position = bp.1;
    for c in sc.startc..(NCOLS as usize) - sc.limitc {
        for r in 0..(NROWS as usize) - sc.limitr {
            let cl = CellsLine([
                CellsCoord {
                    row: r, // r + 0 * sc.incr,
                    col: c, // c + 0 * sc.incc,
                },
                CellsCoord {
                    row: (r as i16 + sc.incr) as usize, // r + 1 * sc.incr,
                    col: (c as i16 + sc.incc) as usize, // c + 1 * sc.incc,
                },
                CellsCoord {
                    row: (r as i16 + 2 * sc.incr) as usize,
                    col: (c as i16 + 2 * sc.incc) as usize,
                },
                CellsCoord {
                    row: (r as i16 + 3 * sc.incr) as usize,
                    col: (c as i16 + 3 * sc.incc) as usize,
                },
            ]);

            position = score_4cells(&cl, position, board)
        }
    }
    (board, position)
}

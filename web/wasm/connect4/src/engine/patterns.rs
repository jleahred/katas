/// four_in_line
///
/// imposible to avoid (next_move_wins > 1)
///
/// next_move_wins (from thee_with_hole)
///
/// three_holes_consecutives_same_column
///
/// three_with_hole  ->  count & hole matrix
///
/// two with hole -> count & hole matrix
///
use super::*;

pub enum Patterns {
    FourInLine,
    P(PatternsCountPlayer),
}

impl Patterns {
    pub(crate) fn init() -> Self {
        Patterns::P(PatternsCountPlayer::init())
    }
}

pub struct PatternsCountPlayer {
    pub(crate) player_o: PatternsCount,
    pub(crate) player_x: PatternsCount,
}

impl PatternsCountPlayer {
    fn init() -> Self {
        PatternsCountPlayer {
            player_o: PatternsCount::init(),
            player_x: PatternsCount::init(),
        }
    }
}

#[derive(Debug)]
pub struct PatternsCount {
    next_move_wins: u16,
    imposible_avoid: u16,
    vert_consecutive_hole_3inline: u16,
    line3: u16, //  a hole for win
    line2: u16, //  2 holes for win
}

pub(crate) fn get_patterns(board: &Board) -> Patterns {
    let patt = scan_horiz(board, Patterns::init());
    let patt = scan_vert(board, patt);
    let patt = scan_diag1(board, patt);
    scan_diag2(board, patt)
}

//  scan4line   ->  FourInLine or not
//  scan3line   ->  line3, next wins, vert_consecutive_hole_3inline
//  scan2line   ->  line2, Impossible avoid

fn score_4cells(cl: &CellsLine, patt: PatternsCountPlayer, board: &Board) -> Patterns {
    let check_impossible2avoid = |mut patt: PatternsCountPlayer| {
        let update_imp_avoid_player = |patt_player: &mut PatternsCount| {
            if patt_player.next_move_wins > 1 {
                patt_player.imposible_avoid = 1;
            }
        };
        update_imp_avoid_player(&mut patt.player_o);
        update_imp_avoid_player(&mut patt.player_x);
        patt
    };
    let fpatt = scan_4line(cl, patt, board);
    if let Patterns::P(patt) = fpatt {
        let patt = scan_3line(cl, patt, board);
        let patt = scan_2line(cl, patt, board);
        let patt = check_impossible2avoid(patt);
        Patterns::P(patt)
    } else {
        fpatt
    }
}

fn get_cell_from_coord(cc: &CellsCoord, board: &Board) -> Cell {
    board.get_cell_dangerous(cc.col, cc.row)
}

fn scan_4line(cl: &CellsLine, patt: PatternsCountPlayer, board: &Board) -> Patterns {
    let (count_po, count_px, _holes) = count_and_holes_4cells(cl, board);
    if count_po == 4 || count_px == 4 {
        Patterns::FourInLine
    } else {
        Patterns::P(patt)
    }
}

fn count_and_holes_4cells(cl: &CellsLine, board: &Board) -> (u8, u8, Vec<CellsCoord>) {
    let mut count_po = 0;
    let mut count_px = 0;
    let mut holes = vec![];

    for i in 0..(NLINE as usize) {
        match get_cell_from_coord(&cl.0[i], board) {
            Cell::P(Player::O) => count_po += 1,
            Cell::P(Player::X) => count_px += 1,
            _ => holes.push(cl.0[i]),
        }
    }
    (count_po, count_px, holes)
}

fn inmediate_cell(cco: &Option<CellsCoord>, board: &Board) -> bool {
    if let Some(cc) = cco {
        if cc.row == NROWS as usize - 1 {
            true
        } else if cc.row > NROWS as usize - 1 {
            false
        } else {
            !is_empty(
                &CellsCoord {
                    row: cc.row + 1,
                    col: cc.col,
                },
                board,
            )
        }
    } else {
        false
    }
}

fn scan_3line(cl: &CellsLine, mut patt: PatternsCountPlayer, board: &Board) -> PatternsCountPlayer {
    let (count_po, count_px, holes) = count_and_holes_4cells(cl, board);

    let mut oplayer = match (count_po, count_px) {
        (3, 0) => Some(&mut patt.player_o),
        (0, 3) => Some(&mut patt.player_x),
        _ => None,
    };
    if let Some(ref mut player) = oplayer {
        player.line3 += 1;
        if inmediate_cell(&Some(holes[0]), board) {
            player.next_move_wins += 1;
        }
    }
    patt
}

fn scan_2line(cl: &CellsLine, mut patt: PatternsCountPlayer, board: &Board) -> PatternsCountPlayer {
    let (count_po, count_px, _holes) = count_and_holes_4cells(cl, board);

    match (count_po, count_px) {
        (2, 0) => patt.player_o.line2 += 1,
        (0, 2) => patt.player_x.line2 += 1,
        _ => (),
    };
    patt
}

fn is_empty(cc: &CellsCoord, board: &Board) -> bool {
    match get_cell_from_coord(
        &CellsCoord {
            row: cc.row,
            col: cc.col,
        },
        board,
    ) {
        Cell::Empty => true,
        _ => false,
    }
}

impl PatternsCount {
    pub(crate) fn init() -> Self {
        PatternsCount {
            next_move_wins: 0,
            imposible_avoid: 0,
            vert_consecutive_hole_3inline: 0,
            line3: 0,
            line2: 0,
        }
    }
}

fn scan_horiz(board: &Board, pattern: Patterns) -> Patterns {
    scan(
        board,
        pattern,
        &ScanConf {
            startc: 0,
            limitc: (NLINE - 1) as usize,
            limitr: 0,
            incc: 1,
            incr: 0,
        },
    )
}

fn scan_vert(board: &Board, pattern: Patterns) -> Patterns {
    scan(
        board,
        pattern,
        &ScanConf {
            startc: 0,
            limitc: 0,
            limitr: (NLINE - 1) as usize,
            incc: 0,
            incr: 1,
        },
    )
}

fn scan_diag1(board: &Board, pattern: Patterns) -> Patterns {
    scan(
        board,
        pattern,
        &ScanConf {
            startc: 0,
            limitc: (NLINE - 1) as usize,
            limitr: (NLINE - 1) as usize,
            incc: 1,
            incr: 1,
        },
    )
}
fn scan_diag2(board: &Board, pattern: Patterns) -> Patterns {
    scan(
        board,
        pattern,
        &ScanConf {
            startc: (NLINE - 1) as usize,
            limitc: 0,
            limitr: (NLINE - 1) as usize,
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

#[derive(Clone, Copy)]
struct CellsCoord {
    row: usize,
    col: usize,
}

fn scan(board: &Board, mut patterns: Patterns, sc: &ScanConf) -> Patterns {
    for c in sc.startc..(NCOLS as usize) - sc.limitc {
        for r in 0..(NROWS as usize) - sc.limitr {
            match patterns {
                Patterns::FourInLine => return patterns,
                Patterns::P(pc) => {
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

                    patterns = score_4cells(&cl, pc, board);
                }
            }
        }
    }
    patterns
}

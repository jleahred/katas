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

#[derive(Debug)]
pub enum Patterns {
    FourInLine,
    P((PatternsCount, PatternsCount)),
}

impl Patterns {
    pub(crate) fn init() -> Self {
        Patterns::P((PatternsCount::init(), PatternsCount::init()))
    }
}

#[derive(Debug)]
pub struct PatternsCount {
    next_player_wins: u16,
    imposible_avoid: u16,
    vert_consecutive_hole_3inline: u16,
    line3: u16, //  a hole for win
    line2: u16, //  2 holes for win
}

struct BPP<'a> {
    board: &'a Board,
    patterns: Patterns,
    player: Player,
}

impl<'a> BPP<'a> {
    fn new(board: &'a Board, patterns: Patterns, player: Player) -> Self {
        BPP {
            board,
            patterns,
            player,
        }
    }
}

pub(crate) fn get_patterns(board: &Board, player: Player) -> Patterns {
    let emtpy_pc = || PatternsCount::init();
    let bpp = BPP::new(board, Patterns::P((emtpy_pc(), emtpy_pc())), player);
    let bpp = scan_horiz(bpp);
    let bpp = scan_vert(bpp);
    let bpp = scan_diag1(bpp);
    let bpp = scan_diag2(bpp);
    bpp.patterns
}

//  scan4line   ->  FourInLine or not
//  scan3line   ->  line3, next wins, vert_consecutive_hole_3inline
//  scan3line   ->  line2m Impossible avoid

fn score_4cells(cl: &CellsLine, patt: Patterns, board: &Board) -> Patterns {
    let patt = scan_4line(cl, patt, board);
    if let Patterns::P(ref _pc) = patt {
        scan_3line(cl, patt, board)
    } else {
        patt
    }
}

fn get_cell_from_coord(cc: &CellsCoord, board: &Board) -> Cell {
    board.get_cell_dangerous(cc.col, cc.row)
}

fn scan_4line(cl: &CellsLine, patt: Patterns, board: &Board) -> Patterns {
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
            return Patterns::FourInLine;
        }
    }
    patt
}

fn scan_3line(cl: &CellsLine, patt: Patterns, board: &Board) -> Patterns {
    // let control_cell = get_cell_from_coord(&cl.0[0], board);

    // if let Cell::P(_) = control_cell {
    //     let mut count_matches = 0;
    //     for i in 0..4 {
    //         if control_cell != get_cell_from_coord(&cl.0[i], board) {
    //             break;
    //         } else {
    //             count_matches += 1;
    //         }
    //     }
    //     if count_matches == 3 {
    //         return Patterns::FourInLine;
    //     }
    // }
    patt
}

impl PatternsCount {
    pub(crate) fn init() -> Self {
        PatternsCount {
            next_player_wins: 0,
            imposible_avoid: 0,
            vert_consecutive_hole_3inline: 0,
            line3: 0,
            line2: 0,
        }
    }
}

fn scan_horiz<'a>(bp: BPP<'a>) -> BPP<'a> {
    scan(
        bp,
        &ScanConf {
            startc: 0,
            limitc: (NLINE - 1) as usize,
            limitr: 0,
            incc: 1,
            incr: 0,
        },
    )
}

fn scan_vert<'a>(bp: BPP<'a>) -> BPP<'a> {
    scan(
        bp,
        &ScanConf {
            startc: 0,
            limitc: 0,
            limitr: (NLINE - 1) as usize,
            incc: 0,
            incr: 1,
        },
    )
}
fn scan_diag1<'a>(bp: BPP<'a>) -> BPP<'a> {
    scan(
        bp,
        &ScanConf {
            startc: 0,
            limitc: (NLINE - 1) as usize,
            limitr: (NLINE - 1) as usize,
            incc: 1,
            incr: 1,
        },
    )
}
fn scan_diag2<'a>(bp: BPP<'a>) -> BPP<'a> {
    scan(
        bp,
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

struct CellsCoord {
    row: usize,
    col: usize,
}

fn scan<'a>(bp: BPP<'a>, sc: &ScanConf) -> BPP<'a> {
    let board = bp.board;
    let mut patterns = bp.patterns;
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

            patterns = score_4cells(&cl, patterns, board)
        }
    }
    BPP::new(bp.board, patterns, bp.player)
}

use crate::matrix;
use crate::matrix::Matrix;

pub(crate) struct Position {
    pub(crate) row: usize,
    pub(crate) col: usize,
}

pub(crate) struct Map {
    pub(crate) matrix: Matrix,
    pub(crate) enter: Position,
    pub(crate) exit: Position,
}

pub(crate) fn new_zero(size: usize) -> Map {
    Map {
        matrix: matrix::new(size),
        enter: Position { row: 0, col: 0 },
        exit: Position {
            row: size - 1,
            col: size - 1,
        },
    }
}

fn new_matrix_zig_zag(size: usize) -> Matrix {
    let mut m = matrix::new(size);
    for row in 0..size {
        if row % 2 == 0 {
            for col in 0..size {
                m.set_rc(row, col, 1);
            }
        } else {
            for col in 0..size {
                m.set_rc(row, col, 200);
            }
        }
    }
    for row in 0..size {
        if (row + 3) % 4 == 0 {
            m.set_rc(row, size - 1, 1);
        } else if (row + 1) % 4 == 0 {
            m.set_rc(row, 0, 1);
        }
    }
    m
}

pub(crate) fn new_zigzag(size: usize) -> Map {
    Map {
        matrix: new_matrix_zig_zag(size),
        enter: Position { row: 0, col: 0 },
        exit: Position {
            row: size - 1,
            col: size - 1,
        },
    }
}

fn new_matrix_zigzag_v(size: usize) -> Matrix {
    let mut m = matrix::new(size);
    let z = new_matrix_zig_zag(size);
    for row in 0..size {
        for col in 0..size {
            m.set_rc(row, col, z.get_rc(col, row));
        }
    }
    m
}

pub(crate) fn new_zigzag_v(size: usize) -> Map {
    Map {
        matrix: new_matrix_zig_zag(size),
        enter: Position { row: 0, col: 0 },
        exit: Position {
            row: size - 1,
            col: size - 1,
        },
    }
}

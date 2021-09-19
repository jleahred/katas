#[derive(Debug)]
pub(crate) struct Matrix {
    pub(crate) size: usize,
    data: Vec<Vec<u8>>,
}

impl Matrix {
    pub(crate) fn get_rc(&self, row: usize, col: usize) -> u8 {
        self.data[row][col]
    }
    pub(crate) fn set_rc(&mut self, row: usize, col: usize, v: u8) {
        let v = if v > 200 { 200 } else { v };
        self.data[row][col] = v;
    }
}

pub(crate) fn new(size: usize) -> Matrix {
    Matrix {
        size,
        data: vec![vec![0; size]; size],
    }
}

// pub(crate) fn new_zigzag(size: usize) -> Matrix {
//     let mut m = Matrix {
//         size,
//         data: vec![vec![0; size]; size],
//     };
//     for r in 0..size {
//         if r % 2 == 0 {
//             for c in 0..size {
//                 m.data[r][c] = 1;
//             }
//         } else {
//             for c in 0..size {
//                 m.data[r][c] = 200;
//             }
//         }
//     }
//     for r in 0..size {
//         if (r + 3) % 4 == 0 {
//             m.data[r][size - 1] = 1;
//         } else if (r + 1) % 4 == 0 {
//             m.data[r][0] = 1;
//         }
//     }
//     m
// }

// pub(crate) fn new_zigzag_v(size: usize) -> Matrix {
//     let mut m = Matrix {
//         size,
//         data: vec![vec![0; size]; size],
//     };
//     let z = new_zigzag(size);
//     for r in 0..size {
//         for c in 0..size {
//             m.data[r][c] = z.data[c][r];
//         }
//     }
//     m
// }

#[cfg(test)]
mod tests {
    #[test]
    fn matrix_init() {
        let size = 10;
        let matrix = crate::matrix::new(size);

        for r in 0..size {
            for c in 0..size {
                assert_eq!(matrix.data[r][c], 0);
            }
        }
    }
}

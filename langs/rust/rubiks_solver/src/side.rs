use std::fmt;
use config::SIZE as SIZE;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Stickers(pub [[u8; SIZE]; SIZE]);


impl Stickers {
    pub fn row(&self, row: usize) -> [u8; SIZE] {
        let Stickers(stickers) = *self;
        stickers[row]
    }
}
impl Stickers {
    pub fn col(&self, col: usize) -> [u8; SIZE] {
        let Stickers(stickers) = *self;
        let mut result = [0; SIZE];
        for i in 0..SIZE {
            result[i] = stickers[i][col];
        }
        result
    }
}


impl fmt::Display for Stickers {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Stickers(stickers) = *self;
        for i in 0..SIZE {
            for j in 0..SIZE {
                try!(write!(f, "{}", stickers[i][j]));
            }
            try!(write!(f, "\n"));
        }
        write!(f, "")
    }
}


pub fn color(color : u8) -> Stickers {
    Stickers([[color; SIZE]; SIZE])
}


pub mod for_cube {
    use config;

    pub fn merge_row(stickers :&super::Stickers, row :usize, rstickers : &[u8;config::SIZE]) -> super::Stickers {
        let super::Stickers(mut new_stickers) = *stickers;
        for i in 0..config::SIZE {
            new_stickers[row][i] = rstickers[i];
        }
        super::Stickers(new_stickers)
    }

    pub fn merge_col(stickers :&super::Stickers, col :usize, cstickers : &[u8;config::SIZE]) -> super::Stickers {
        let super::Stickers(mut new_stickers) = *stickers;
        for i in 0..config::SIZE {
            new_stickers[i][col] = cstickers[i];
        }
        super::Stickers(new_stickers)
    }

    pub enum Dir { Clock, InvClock}

    pub fn rotation(stickers :&super::Stickers, dir : Dir) -> super::Stickers {
        let super::Stickers(ref orig_stickers) = *stickers;
        let super::Stickers(mut new_stickers) = *stickers;
        match dir {
            Dir::Clock    =>
                    for c in 0..config::SIZE {
                        for r in 0..config::SIZE {
                            new_stickers[c][config::SIZE-r-1] = orig_stickers[r][c];
                        }
                    },
            Dir::InvClock =>
                    for c in 0..config::SIZE {
                        for r in 0..config::SIZE {
                            new_stickers[config::SIZE-c-1][r] = orig_stickers[r][c];
                        }
                    }
        }
        super::Stickers(new_stickers)
    }

    ///  0 means, I'm not worried about this sticker
    pub fn equivalent_end(l: &super::Stickers, r: &super::Stickers) -> bool {
        let result = true;
        for i in 0..config::SIZE {
            for j in 0..config::SIZE {
                let super::Stickers(stickers1) = *l;
                let super::Stickers(stickers2) = *r;
                let sticker2 = stickers2[i][j];
                if sticker2!=0 {
                    if stickers1[i][j] != stickers2[i][j] {
                        return false;
                    }
                }
            }
        }
        result
    }
}



#[test]
fn convert_string () {
    {
        let side = Stickers(
            [
                [0,0,0,0],
                [0,0,0,0],
                [0,0,0,0],
                [0,0,0,0]
            ]
            );
        let str_side = format!("{}", side);
        let str_side_check = "0000\n0000\n0000\n0000\n";
        assert_eq!(str_side, str_side_check);
    }
    {
        let side = Stickers(
            [
                [1,5,6,7],
                [8,2,9,0],
                [1,2,3,3],
                [4,5,6,4]
            ]
            );
        let str_side = format!("{}", side);
        let str_side_check = "1567\n8290\n1233\n4564\n";
        assert_eq!(str_side, str_side_check);
    }
}



#[test]
fn test_check_equal_stickers() {
    {
        let side1 = Stickers(
            [
                [1,5,6,7],
                [8,2,9,0],
                [1,2,3,3],
                [4,5,6,4]
            ]
            );
        let side2 = Stickers(
            [
                [1,5,6,7],
                [8,2,9,0],
                [1,2,3,3],
                [4,5,6,4]
            ]
            );
        assert_eq!(side1, side2);
    }
    {
        let side1 = Stickers(
            [
                [1,5,6,7],
                [8,2,9,0],
                [1,2,3,3],
                [4,5,6,4]
            ]
            );
        let side2 = Stickers(
            [
                [0,0,6,0],
                [8,0,0,0],
                [1,2,0,0],
                [0,0,0,0]
            ]
            );
        assert!(side1 != side2);
        assert!(side2 != side1);
    }
}


#[test]
fn test_check_equivalent_end() {
    {
        let side1 = Stickers(
            [
                [1,5,6,7],
                [8,2,9,0],
                [1,2,3,3],
                [4,5,6,4]
            ]
            );
        let side2 = Stickers(
            [
                [1,5,6,7],
                [8,2,9,0],
                [1,2,3,3],
                [4,5,6,4]
            ]
            );
        assert!(for_cube::equivalent_end(&side1, &side2));
        assert!(for_cube::equivalent_end(&side2, &side1));
    }
    {
        let side1 = Stickers(
            [
                [1,5,6,7],
                [8,2,9,0],
                [1,2,3,3],
                [4,5,6,4]
            ]
            );
        let side2 = Stickers(
            [
                [0,0,6,0],
                [8,0,0,0],
                [1,2,0,0],
                [0,0,0,0]
            ]
            );
        assert!(for_cube::equivalent_end(&side1, &side2));
        assert!(for_cube::equivalent_end(&side2, &side1)==false);
    }
}


#[test]
fn test_merge_row() {
    {
        let new_side = for_cube::merge_row(&color(1), 2, &[2,3,4,5]);
        let result = Stickers(
            [
                [1,1,1,1],
                [1,1,1,1],
                [2,3,4,5],
                [1,1,1,1],
            ]
            );
        assert!(&new_side == &result);
        assert!(&result == &new_side);
    }
    {
        let new_side = for_cube::merge_row(&color(1), 3, &[2,3,4,5]);
        let result = Stickers(
            [
                [1,1,1,1],
                [1,1,1,1],
                [1,1,1,1],
                [2,3,4,5],
            ]
            );
        assert!(&new_side == &result);
        assert!(&result == &new_side);
    }
}


#[test]
fn test_merge_col() {
    {
        let new_side = for_cube::merge_col(&color(1), 2, &[2,3,4,5]);
        let result = Stickers(
            [
                [1,1,2,1],
                [1,1,3,1],
                [1,1,4,1],
                [1,1,5,1],
            ]
            );
        assert!(&new_side == &result);
        assert!(&result == &new_side);
    }
    {
        let new_side = for_cube::merge_col(&color(1), 3, &[2,3,4,5]);
        let result = Stickers(
            [
                [1,1,1,2],
                [1,1,1,3],
                [1,1,1,4],
                [1,1,1,5],
            ]
            );
        assert!(&new_side == &result);
        assert!(&result == &new_side);
    }
}


#[test]
fn test_rotation() {
    {
        let side = Stickers(
            [
                [1,5,6,7],
                [8,2,9,0],
                [1,2,3,3],
                [4,5,6,4]
            ]
            );
        let rotationd_clock = Stickers(
            [
                [4,1,8,1],
                [5,2,2,5],
                [6,3,9,6],
                [4,3,0,7]
            ]
            );
        assert!(for_cube::rotation(&side, for_cube::Dir::Clock) == rotationd_clock);
        assert!(for_cube::rotation(&rotationd_clock, for_cube::Dir::InvClock) == side);
    }
}

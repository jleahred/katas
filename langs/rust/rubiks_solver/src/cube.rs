/*
    movements
        V1+
        V2+
        V1-
        ...
        H1+
        H3-
        ...
        R1+
        R4-

    struct MoveDir {
        type : MoveType,
        pos  : usize
        dir  : MoveDir
    }
    pub fn move(dir : MoveDir)

    pub fn next_move(dir : MoveDir) -> MoveDir


*/

use std::fmt;
use config;
use side;


#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Sides {
                                    pub top   : side::Stickers,

    pub left  : side::Stickers,     pub front : side::Stickers,     pub right : side::Stickers,

                                    pub bottom: side::Stickers,

                                    pub back  : side::Stickers
}

pub mod Move {
    pub enum Orientation {
        horizontal,
        vertical,
        front
    }

    pub struct Direction(bool);

    pub struct Item(Orientation, Direction, usize);     //  ups!!! (usize)
    /*pub struct Item  {
        pub orientation : Orientation,
        pub dir         : Direction,
        pub layer       : usize     //  ups!!!
    }*/
}

pub fn create(
            top     :   &side::Stickers,
            left    :   &side::Stickers,
            front   :   &side::Stickers,
            right   :   &side::Stickers,
            bottom  :   &side::Stickers,
            back    :   &side::Stickers,
    ) -> Sides
{
    Sides{  top:    *top,
            left:   *left,
            front:  *front,
            right:  *right,
            bottom: *bottom,
            back:   *back,
        }
}


impl fmt::Display for Sides {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let write_row =
                |f: &mut fmt::Formatter, row_stickers : [u8; config::SIZE]|  {
            for c in 0..config::SIZE {
                try!(write!(f, "{}"  , row_stickers[c]));
            }
            write!(f, "  ")
        };

        let center = String::from_utf8(vec![b' '; config::SIZE+2]).unwrap();
        let write_center =
                |f: &mut fmt::Formatter, side : side::Stickers | {
            let side::Stickers(ref stickers) = side;
            for r in 0..config::SIZE {
                try!(write!(f, "{}", center));
                try!(write_row(f, stickers[r]));
                try!(write!(f, "\n"));
            }
            write!(f, "\n")
        };

        // print top
        try!(write_center(f, self.top));
        //  print middle
        {
            let side::Stickers(ref st_left) = self.left;
            let side::Stickers(ref st_front) = self.front;
            let side::Stickers(ref st_right) = self.right;
            for r in 0..config::SIZE {
                try!(write_row(f, st_left[r]));
                try!(write_row(f, st_front[r]));
                try!(write_row(f, st_right[r]));
                try!(write!(f, "\n"));
            }
            try!(write!(f, "\n"));
        }
        try!(write_center(f, self.bottom));
        try!(write_center(f, self.back));
        write!(f, "")
    }
}



#[test]
fn test_display() {
    let cube = create(
                                &side::color(0),
            &side::color(1),    &side::color(2),    &side::color(3),
                                &side::color(4),
                                &side::color(5),
        );

    let str_cube = concat!(
            "      0000  \n",
            "      0000  \n",
            "      0000  \n",
            "      0000  \n",
            "\n",
            "1111  2222  3333  \n",
            "1111  2222  3333  \n",
            "1111  2222  3333  \n",
            "1111  2222  3333  \n",
            "\n",
            "      4444  \n",
            "      4444  \n",
            "      4444  \n",
            "      4444  \n",
            "\n",
            "      5555  \n",
            "      5555  \n",
            "      5555  \n",
            "      5555  \n",
            "\n");

    assert_eq!(str_cube, format!("{}", cube));
}

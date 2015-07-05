/*
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


pub mod rotation {
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub enum Orientation {
        Horizontal,
        Vertical,
        Front
    }

    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub enum Direction { Plus, Minus }

    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub struct Item(pub Orientation, pub Direction, pub usize);     //  ups!!! (usize)
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


pub fn rotation_horizontal(sides: &Sides, dir: rotation::Direction, level: usize) -> Sides
{
    macro_rules! get_row {
        ($side:ident, $level:expr)  =>  (sides.$side.row($level))
    }

    let mut result = *sides;

    macro_rules! switch_rows {
        ($ssource:ident, $sdest:ident)  =>
            (result.$sdest  = side::for_cube::merge_row(&sides.$sdest,  level, get_row!($ssource, level));)
    }

    macro_rules! rotate_edge {
        ( $side:ident, (rotation::Direction::$dir1:ident =>  $side_rotation1:ident), (rotation::Direction::$dir2:ident =>  $side_rotation2:ident) )  => (
            result.$side =
                match dir {
                    rotation::Direction::$dir1  =>
                            side::for_cube::rotation(&sides.$side,  side::for_cube::Direction::$side_rotation1),
                    rotation::Direction::$dir2  =>
                            side::for_cube::rotation(&sides.$side,  side::for_cube::Direction::$side_rotation2),
                };
        )
    }


    match dir {
        rotation::Direction::Plus  => {
            switch_rows!(back,  left);
            switch_rows!(left,  front);
            switch_rows!(front, right);
            switch_rows!(right, back);
        },
        rotation::Direction::Minus => {
            switch_rows!(front, left);
            switch_rows!(back,  right);
            switch_rows!(right, front);
            switch_rows!(left,  back);
        }
    };
    match level+1 {
        1               =>     rotate_edge!(top,    (rotation::Direction::Plus => InvClock), (rotation::Direction::Minus => Clock)),
        config::SIZE    =>     rotate_edge!(bottom, (rotation::Direction::Plus => Clock),    (rotation::Direction::Minus => InvClock)),
        _ => ()
    }
    return result;
}

pub fn rotation_vertical(sides: &Sides, dir: rotation::Direction, level: usize) -> Sides
{
    macro_rules! get_col {
        ($side:ident, $level:expr)  =>  (sides.$side.col($level))
    }

    let mut result = *sides;

    macro_rules! switch_cols {
        ($ssource:ident, $sdest:ident)  =>
            (result.$sdest  = side::for_cube::merge_col(&sides.$sdest,  level, get_col!($ssource, level));)
    }

    match dir {
        rotation::Direction::Plus  => {
            switch_cols!(top,    front);
            switch_cols!(front,  bottom);
            switch_cols!(bottom, back);
            switch_cols!(back,   top);
        },
        rotation::Direction::Minus => {
            switch_cols!(top,    back);
            switch_cols!(back,   bottom);
            switch_cols!(bottom, front);
            switch_cols!(front,  top);
        }
    };

    result.left =
        match (level+1, dir) {
            (1, rotation::Direction::Plus)  =>
                    side::for_cube::rotation(&sides.left,  side::for_cube::Direction::Clock),
            (1, rotation::Direction::Minus)  =>
                    side::for_cube::rotation(&sides.left,  side::for_cube::Direction::InvClock),
            (_,_) => result.left
        };
    result.bottom =
        match (level+1, dir) {
            (config::SIZE, rotation::Direction::Plus)  =>
                    side::for_cube::rotation(&sides.bottom,  side::for_cube::Direction::Clock),
            (config::SIZE, rotation::Direction::Minus)  =>
                    side::for_cube::rotation(&sides.bottom,  side::for_cube::Direction::InvClock),
            (_,_) => result.top
        };
    return result;
}


impl fmt::Display for rotation::Orientation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let sorientation = match *self {
            rotation::Orientation::Front        => "F",
            rotation::Orientation::Horizontal   => "H",
            rotation::Orientation::Vertical     => "V"
        };
        write!(f, "{}", sorientation)
    }
}

impl fmt::Display for rotation::Direction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let sdir = match *self {
            rotation::Direction::Plus  => "+",
            rotation::Direction::Minus => "-"
        };
        write!(f, "{}", sdir)
    }
}

impl fmt::Display for rotation::Item {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let rotation::Item(orientation, direction, pos) = *self;
        write!(f, "{}{}{}", orientation, direction, pos)
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
            let side::Stickers(ref st_right) = self.right;
            let side::Stickers(ref st_front) = self.front;
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

#[test]
fn test_display_rotation()
{
    use cube::rotation::*;

    {
        let mv = rotation::Item(Orientation::Horizontal, Direction::Plus, 0);
        assert_eq!(format!("{}", mv), "H+0");
    }
    {
        let mv = rotation::Item(Orientation::Front, Direction::Minus, 2);
        assert_eq!(format!("{}", mv), "F-2");
    }
    {
        let mv = rotation::Item(Orientation::Vertical, Direction::Plus, 4);
        assert_eq!(format!("{}", mv), "V+4");
    }
}

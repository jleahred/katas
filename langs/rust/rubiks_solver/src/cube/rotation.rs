use std::fmt;
use side;
use config;
use super::Sides;
use cube;



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


pub fn process(sides: &Sides, item : &Item) -> Sides {
    match *item {
        Item(Orientation::Horizontal, direction, level) =>    horizontal(sides, direction, level),
        Item(Orientation::Vertical, direction, level)   =>    vertical  (sides, direction, level),
        Item(Orientation::Front, direction, level)      =>    front     (sides, direction, level),
    }
}



impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Item(orientation, direction, pos) = *self;
        write!(f, "{}{}{}", orientation, direction, pos)
    }
}


impl fmt::Display for Orientation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let sorientation = match *self {
            Orientation::Front        => "F",
            Orientation::Horizontal   => "H",
            Orientation::Vertical     => "V"
        };
        write!(f, "{}", sorientation)
    }
}

impl fmt::Display for Direction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let sdir = match *self {
            Direction::Plus  => "+",
            Direction::Minus => "-"
        };
        write!(f, "{}", sdir)
    }
}





macro_rules! rotate_edge {
    ( $result:expr, $sides:expr, $dir:expr, $side:ident, (Direction::$dir1:ident =>  $side_rotation1:ident), (Direction::$dir2:ident =>  $side_rotation2:ident) )  => (
        $result.$side =
            match $dir {
                Direction::$dir1  =>
                        side::for_cube::rotation(&$sides.$side,  side::for_cube::Direction::$side_rotation1),
                Direction::$dir2  =>
                        side::for_cube::rotation(&$sides.$side,  side::for_cube::Direction::$side_rotation2),
            };
    )
}

fn horizontal(sides: &Sides, dir: Direction, level: usize) -> Sides
{
    let mut result = *sides;

    macro_rules! get_row {
        ($side:ident, $level:expr)  =>  (sides.$side.row($level))
    }

    macro_rules! switch_rows {
        ($ssource:ident, $sdest:ident)  =>
            (result.$sdest  = side::for_cube::merge_row(&sides.$sdest,  level, get_row!($ssource, level));)
    }

    macro_rules! rotate_edge_local {
            ( $side:ident, (Direction::$dir1:ident =>  $side_rotation1:ident), (Direction::$dir2:ident =>  $side_rotation2:ident) )  =>
                (rotate_edge!(result, sides, dir, $side, (Direction::$dir1 => $side_rotation1), (Direction::$dir2 => $side_rotation2)))
    }

    match dir {
        Direction::Plus  => {
            switch_rows!(back,  left);
            switch_rows!(left,  front);
            switch_rows!(front, right);
            switch_rows!(right, back);
        },
        Direction::Minus => {
            switch_rows!(front, left);
            switch_rows!(back,  right);
            switch_rows!(right, front);
            switch_rows!(left,  back);
        }
    };
    match level+1 {
        1               =>     rotate_edge_local!(top,    (Direction::Plus => InvClock), (Direction::Minus => Clock)),
        config::SIZE    =>     rotate_edge_local!(bottom, (Direction::Plus => Clock),    (Direction::Minus => InvClock)),
        _ => ()
    }
    return result;
}

fn vertical(sides: &Sides, dir: Direction, level: usize) -> Sides
{
    let mut result = *sides;

    macro_rules! get_col {
        ($side:ident, $level:expr)  =>  (sides.$side.col($level))
    }

    macro_rules! rotate_edge_local {
            ( $side:ident, (Direction::$dir1:ident =>  $side_rotation1:ident), (Direction::$dir2:ident =>  $side_rotation2:ident) )  =>
                (rotate_edge!(result, sides, dir, $side, (Direction::$dir1 => $side_rotation1), (Direction::$dir2 => $side_rotation2)))
    }

    macro_rules! switch_cols {
        ($ssource:ident, $sdest:ident)  =>
            (result.$sdest  = side::for_cube::merge_col(&sides.$sdest,  level, get_col!($ssource, level));)
    }

    match dir {
        Direction::Plus  => {
            switch_cols!(top,    front);
            switch_cols!(front,  bottom);
            switch_cols!(bottom, back);
            switch_cols!(back,   top);
        },
        Direction::Minus => {
            switch_cols!(top,    back);
            switch_cols!(back,   bottom);
            switch_cols!(bottom, front);
            switch_cols!(front,  top);
        }
    };
    match level+1 {
        1               =>     rotate_edge_local!(left,    (Direction::Plus => Clock),    (Direction::Minus => InvClock)),
        config::SIZE    =>     rotate_edge_local!(right,   (Direction::Plus => InvClock), (Direction::Minus => Clock)),
        _ => ()
    }
    return result;
}

fn front(sides: &Sides, dir: Direction, level: usize) -> Sides
{
    let mut result = *sides;

    macro_rules! get_col {
        ($side:ident, $level:expr)  =>  (sides.$side.col($level))
    }
    macro_rules! get_row {
        ($side:ident, $level:expr)  =>  (sides.$side.row($level))
    }

    macro_rules! rotate_edge_local {
            ( $side:ident, (Direction::$dir1:ident =>  $side_rotation1:ident), (Direction::$dir2:ident =>  $side_rotation2:ident) )  =>
                (rotate_edge!(result, sides, dir, $side, (Direction::$dir1 => $side_rotation1), (Direction::$dir2 => $side_rotation2)))
    }

    macro_rules! switch_col2row {
        (($ssource:ident, $slevel:expr) =>  ($sdest:ident, $dlevel:expr))  =>
            (result.$sdest  = side::for_cube::merge_row(&sides.$sdest,  $dlevel, get_col!($ssource, $slevel));)
    }
    macro_rules! switch_row2col {
        (($ssource:ident, $slevel:expr)  =>  ($sdest:ident, $dlevel:expr))  =>
            (result.$sdest  = side::for_cube::merge_col(&sides.$sdest,  $dlevel, get_row!($ssource, $slevel));)
    }

    match dir {
        Direction::Plus  => {
            switch_row2col! ((top,    config::SIZE - level-1)   =>      (right, level) );
            switch_col2row! ((right,  level)                    =>      (bottom, level));
            switch_row2col! ((bottom, level)                    =>      (left, config::SIZE - level-1));
            switch_col2row! ((left, config::SIZE - level-1)     =>      (top, config::SIZE - level-1));
        },
        Direction::Minus => {
            switch_row2col! ((top, config::SIZE - level-1)      =>      (left, config::SIZE - level-1));
            switch_col2row! ((left, config::SIZE - level-1)     =>      (bottom, level));
            switch_row2col! ((bottom, level)                    =>      (right, level));
            switch_col2row! ((right, level)                     =>      (top, config::SIZE - level-1));
        }
    };
    match level+1 {
        1               =>     rotate_edge_local!(front,   (Direction::Plus => Clock),    (Direction::Minus => InvClock)),
        config::SIZE    =>     rotate_edge_local!(back,    (Direction::Plus => InvClock), (Direction::Minus => Clock)),
        _ => ()
    }
    return result;
}


#[test]
fn test_display_rotation()
{
    {
        let mv = Item(Orientation::Horizontal, Direction::Plus, 0);
        assert_eq!(format!("{}", mv), "H+0");
    }
    {
        let mv = Item(Orientation::Front, Direction::Minus, 2);
        assert_eq!(format!("{}", mv), "F-2");
    }
    {
        let mv = Item(Orientation::Vertical, Direction::Plus, 4);
        assert_eq!(format!("{}", mv), "V+4");
    }
}


#[test]
fn test_rotation() {
    let cube = cube::create(
                                &side::color(0),
            &side::color(1),    &side::color(2),    &side::color(3),
                                &side::color(4),
                                &side::color(5),
        );

    let str_cube = concat!(
            "      0000  \n",
            "      0000  \n",
            "      0000  \n",
            "      5555  \n",
            "\n",
            "4555  1111  0222  \n",
            "1111  0222  3333  \n",
            "1111  0222  3333  \n",
            "1111  0222  3333  \n",
            "\n",
            "      2444  \n",
            "      2444  \n",
            "      2444  \n",
            "      2444  \n",
            "\n",
            "      3333  \n",
            "      4555  \n",
            "      4555  \n",
            "      4555  \n",
            "\n");

    let rotation1 = vertical(&cube, Direction::Plus, 0);
    let rotation2 = horizontal(&rotation1, Direction::Plus, 0);

    assert_eq!(str_cube, format!("{}", rotation2));
}

#[test]
fn test_front() {
    let cube = cube::create(
                                &side::color(0),
            &side::color(1),    &side::color(2),    &side::color(3),
                                &side::color(4),
                                &side::color(5),
        );

    {
        let str_cube = concat!(
                "      0000  \n",
                "      0000  \n",
                "      0000  \n",
                "      1111  \n",
                "\n",
                "1114  2222  0333  \n",
                "1114  2222  0333  \n",
                "1114  2222  0333  \n",
                "1114  2222  0333  \n",
                "\n",
                "      3333  \n",
                "      4444  \n",
                "      4444  \n",
                "      4444  \n",
                "\n",
                "      5555  \n",
                "      5555  \n",
                "      5555  \n",
                "      5555  \n",
                "\n");
        let rotation1 = front(&cube, Direction::Plus, 0);
        assert_eq!(str_cube, format!("{}", rotation1));
    }
    {
        let str_cube = concat!(
                "      0000  \n",
                "      3333  \n",
                "      0000  \n",
                "      0000  \n",
                "\n",
                "1011  2222  3343  \n",
                "1011  2222  3343  \n",
                "1011  2222  3343  \n",
                "1011  2222  3343  \n",
                "\n",
                "      4444  \n",
                "      4444  \n",
                "      1111  \n",
                "      4444  \n",
                "\n",
                "      5555  \n",
                "      5555  \n",
                "      5555  \n",
                "      5555  \n",
                "\n");
        let rotation1 = front(&cube, Direction::Minus, 2);
        assert_eq!(str_cube, format!("{}", rotation1));
    }
    {
        let str_cube = concat!(
                "      5000  \n",
                "      5000  \n",
                "      5000  \n",
                "      1111  \n",
                "\n",
                "1112  0000  5333  \n",
                "1114  2222  0333  \n",
                "1114  2222  0333  \n",
                "1114  2222  0333  \n",
                "\n",
                "      3333  \n",
                "      2444  \n",
                "      2444  \n",
                "      2444  \n",
                "\n",
                "      4555  \n",
                "      4555  \n",
                "      4555  \n",
                "      4555  \n",
                "\n");
        let rotation1 = vertical(&cube, Direction::Plus, 0);
        let rotation2 = front(&rotation1, Direction::Plus, 0);
        assert_eq!(str_cube, format!("{}", rotation2));
    }
}

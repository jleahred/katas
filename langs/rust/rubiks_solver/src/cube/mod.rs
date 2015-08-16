use std::fmt;
use config;
use side;
pub mod rot;



#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Sides {
                                    pub top   : side::Stickers,

    pub left  : side::Stickers,     pub front : side::Stickers,     pub right : side::Stickers,

                                    pub bottom: side::Stickers,

                                    pub back  : side::Stickers
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

pub fn create_from_strings (lines : [&str;config::SIZE*4])  -> Sides
{
    let get_row_from_string = |s:&str| -> [u8; config::SIZE] {
        if s.len() != config::SIZE {
            panic!(format!("invalid length {} expected 4", s));
        }

        let mut result = [0u8; config::SIZE];

        {
            let mut counter = 0usize;
            for  ch in s.chars() {
                result[counter] = ch as u8 - ('0' as u8);
                counter += 1;
            }
        }

        result
    };
    let get_rows_from_long_string = |s:&str| -> ([u8; config::SIZE],
                                                 [u8; config::SIZE],
                                                 [u8; config::SIZE])  {
        let mut left  = [0u8; config::SIZE];
        let mut front = [0u8; config::SIZE];
        let mut right = [0u8; config::SIZE];
        if s.len() != config::SIZE*3 + 2 {
            panic!(format!("invalid length {} expected {}", s, config::SIZE*3 + 2));
        }

        for i in 0..config::SIZE {
            left[i]  = s.as_bytes()[i] as u8 - ('0' as u8);
            front[i] = s.as_bytes()[config::SIZE+1   +i] as u8 - ('0' as u8);
            right[i] = s.as_bytes()[config::SIZE*2+2 +i] as u8 - ('0' as u8);
        }

        (left, front, right)
    };

    let fill_single_side = |init_index: usize| -> side::Stickers {
        let mut side = side::color(0);
        for i in init_index .. init_index + config::SIZE {
            side = side::for_cube::merge_row(&side, i-init_index, &get_row_from_string(lines[i]));
        }
        side
    };
    let fill3side = || -> (side::Stickers, side::Stickers, side::Stickers) {
        let mut left  = side::color(0);
        let mut front = side::color(0);
        let mut right = side::color(0);
        for i in config::SIZE .. config::SIZE*2 {
            let (left_row, front_row, right_row) = get_rows_from_long_string(lines[i]);
            left  = side::for_cube::merge_row(&left, i-config::SIZE,  &left_row);
            front = side::for_cube::merge_row(&front, i-config::SIZE, &front_row);
            right = side::for_cube::merge_row(&right, i-config::SIZE, &right_row);
        }
        (left, front, right)
    };

    let top = fill_single_side(0);
    let bottom = fill_single_side(config::SIZE*2);
    let back = fill_single_side(config::SIZE*3);
    let (left, front, right) = fill3side();

    create(&top, &left, &front, &right, &bottom, &back)
}




impl Sides {
    pub fn get_rotation(&self, item : &rot::Item) -> Sides {
        rot::process(self, item)
    }
}

pub fn equivalent_end(l: &Sides, r: &Sides) -> bool {
    side::for_cube::equivalent_end(&l.top,    &r.top)  &&
    side::for_cube::equivalent_end(&l.left,   &r.left)  &&
    side::for_cube::equivalent_end(&l.front,  &r.front)  &&
    side::for_cube::equivalent_end(&l.right,  &r.right)  &&
    side::for_cube::equivalent_end(&l.bottom, &r.bottom)  &&
    side::for_cube::equivalent_end(&l.back,   &r.back)
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
fn test_rotation() {
    let cube = create(
                                &side::color(0),
            &side::color(1),    &side::color(2),    &side::color(3),
                                &side::color(4),
                                &side::color(5),
        );

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


    assert_eq!(str_cube, format!("{}",
            &cube
                .get_rotation(&rot::Item(rot::Orient::Vertical, rot::Dir::Plus, 0))
                .get_rotation(&rot::Item(rot::Orient::Front,rot::Dir::Plus, 0))
        ));
}

#[test]
fn test_equivalent_end() {
    let cube1 = create(
                                &side::color(0),
            &side::color(1),    &side::color(0),    &side::color(0),
                                &side::color(4),
                                &side::color(5),
        );

    let side_a = side::Stickers(
        [
            [0,0,0,0],
            [0,0,0,0],
            [0,0,0,0],
            [0,0,0,0]
        ]
        );
    let side_b = side::Stickers(
        [
            [0,0,0,0],
            [0,0,0,0],
            [0,0,0,0],
            [0,0,0,0]
        ]
        );


    let cube2 = create(
                                &side_a,
            &side::color(1),    &side_b,             &side_a,
                                &side::color(4),
                                &side::color(5),
        );

    assert!(equivalent_end(&cube1, &cube2));
}


#[test]
fn test_create_from_strings() {
    let cube_a = create_from_strings(
                     ["0000",
                      "0000",
                      "0000",
                      "0000",
                 "1111 2222 3333",
                 "1111 2222 3333",
                 "1111 2222 3333",
                 "1111 2222 3333",
                      "4444",
                      "4444",
                      "4444",
                      "4444",

                      "5555",
                      "5555",
                      "5555",
                      "5555"]
        );

    let cube_b = create(
                                &side::color(0),
            &side::color(1),    &side::color(2),             &side::color(3),
                                &side::color(4),
                                &side::color(5),
        );

    assert_eq!(&cube_a, &cube_b);

}

#[test]
fn test_equivalent_end2() {
    let cube_a = create_from_strings(
                     ["1234",
                      "1234",
                      "1234",
                      "1234",
                 "1111 2222 3333",
                 "2111 2222 3333",
                 "3111 2222 3333",
                 "4111 2222 3333",
                      "4444",
                      "4444",
                      "4444",
                      "4444",

                      "5555",
                      "5555",
                      "5555",
                      "5555"]
        );

    let cube_b = create_from_strings(
                     ["0000",
                      "0000",
                      "0000",
                      "0000",
                 "0111 2222 3333",
                 "0111 2222 3333",
                 "0111 2222 3333",
                 "0111 2222 3333",
                      "4444",
                      "4444",
                      "4444",
                      "4444",

                      "5555",
                      "5555",
                      "5555",
                      "5555"]
        );


    assert!(equivalent_end(&cube_a, &cube_b));
}

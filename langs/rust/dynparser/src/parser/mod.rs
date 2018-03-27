mod atom;

use Possition;

//pub(self)

#[derive(PartialEq)]
struct Status {
    pos: Possition,
    curr_line: String,
}

impl Status {
    #[allow(dead_code)]
    fn init() -> Self {
        Status {
            pos: Possition::init(),
            curr_line: "".to_owned(),
        }
    }
}

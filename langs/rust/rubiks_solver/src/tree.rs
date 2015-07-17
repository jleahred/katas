use std::fmt;
use cube;
use side;


#[derive(Debug, Clone, Copy)]
pub struct ResultFound {
    depth       : u8,
    iterations  : u64,
}

#[derive(Debug, Clone, Copy)]
pub struct Result {
    iterations  : u64,
    found       : Option<ResultFound>,
}



impl fmt::Display for ResultFound {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "found.depth:  {}\n", self.depth));
        try!(write!(f, "found.iter:   {}\n", self.iterations));
        write!(f, "")
    }
}

impl fmt::Display for Result {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "iterations:  {}\n", self.iterations));
        match self.found {
            Some(found) =>  try!(write!(f, "{}\n", found)),
            None        =>  try!(write!(f, "not found\n")),
        }
        write!(f, "")
    }
}


struct ExploringStatus {
    depth       : u8,
    max_depth   : u8,
    iterations  : u64,
}


pub fn explore(origin : &cube::Sides, end : &cube::Sides, max_depth : u8) -> Result
{
    internal_explore(origin, end, ExploringStatus{  depth:      0,
                                                    max_depth:  max_depth,
                                                    iterations: 1})
}

fn internal_explore(origin : &cube::Sides, end : &cube::Sides, status : ExploringStatus) -> Result
{
    //println!("depth: {}", status.depth);
    //println!("depth: {}", origin);

    if cube::equivalent_end(origin, end) {
        let result = Result{ iterations: status.iterations,
                found: Some(ResultFound{depth: status.depth, iterations: status.iterations}) };
        println!("Found...... {}", &result);
        result
    } else {
        if status.depth < status.max_depth {
            //  TODO:  continue here

            use cube::rot::{Orient, Dir};
            let next =
                origin.get_rotation(&cube::rot::Item(
                        Orient::Front,
                        Dir::Plus,
                        0));
            internal_explore(&next, end, ExploringStatus{  depth:      status.depth+1,
                                                            max_depth:  status.max_depth,
                                                            iterations: status.iterations+1})
        }
        else {
            Result{ iterations: status.iterations,
                    found: None }
        }
    }
}


/*
#[test]
fn test_tree_simple() {
    let init : cube::Sides = cube::create_from_strings(
                     ["0000",
                      "0000",
                      "0000",
                      "0000",
                 "0001 0000 0000",
                 "0002 0000 0000",
                 "0003 0000 0000",
                 "0004 0000 0000",
                      "0000",
                      "0000",
                      "0000",
                      "0000",

                      "0000",
                      "0000",
                      "0000",
                      "0000"]
        );

    let end : cube::Sides = cube::create_from_strings(
                    ["0000",
                     "0000",
                     "0000",
                     "4321",
                "0000 0000 0000",
                "0000 0000 0000",
                "0000 0000 0000",
                "0000 0000 0000",
                     "0000",
                     "0000",
                     "0000",
                     "0000",

                     "0000",
                     "0000",
                     "0000",
                     "0000"]
                );

}
*/

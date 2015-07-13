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

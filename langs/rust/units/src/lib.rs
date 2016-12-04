//! Simple exercice working with units
//!
//! # Usage
//!
//! ```toml
//! [dependencies]
//!
//! ```
//!
//! And this in your crate root:
//!
//! ```rust
//! extern crate units;
//! ```


#[macro_use]

#[cfg(test)]
mod test_diff;

pub mod distance;




// mod units;


// fn main() {
//     use units::distance::Distance::*;
//     use units::distance::Distance;

//     let d1 = Meters(1.);
//     let d2 = Centimeters(200.);
//     let d3 = Millimeters(3000.);
//     println!("{:?}", d1);
//     println!("{:?}", d2);
//     println!("{:?}", d3);
//     println!("{:?}", d1 + d2);
//     println!("{:?}", d1 + d3);
//     println!("{:?}", d2 + d3);

//     println!("{:?}", Meters(2.) + Millimeters(500.));
//     println!("{:?}", Meters(2.) + Millimeters(500.) + Killometers(0.3));

//     println!("{:?}", std::mem::size_of::<Distance>());
//     println!("{:?}", std::mem::size_of::<f64>());

//     println!("{:?}", 3. * Millimeters(500.));
//     println!("{:?}", -1. * Millimeters(500.));
//     println!("{:?}", Millimeters(500.) * 5.);

//     println!("{:?}", Meters(2.) + Millimeters(500.) - Killometers(0.3));
// }
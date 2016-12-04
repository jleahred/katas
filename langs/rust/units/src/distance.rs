//! Distance units
//!
//! # Examples
//!
//! ```rust
//! use units::distance::Distance::*;
//!
//! assert!(Meters(2.)              == Meters(2.));
//! assert!(Meters(2.)              == Millimeters(2000.));
//!
//! assert!(Meters(2.) + Meters(2.) == Meters(4.));
//! assert!(Meters(2.) + Centimeters(20.) + Millimeters(4.)
//!                                 == Millimeters(2204.));
//!
//! assert!(2. * Meters(2.)         == Meters(4.));
//! assert!(2. * Meters(2.002)      == Millimeters(4_004.));
//!
//! assert!(Meters(2.) * 2.         == Meters(4.));
//! assert!(Meters(2.002) * 2.      == Millimeters(4_004.));
//!
//! assert!(Meters(2.) - Meters(4.) == Meters(-2.));
//! assert!(Meters(2.) - Millimeters(400.)
//!                                 == Meters(1.6));
//! ```


/// Data type representing Distance
///
/// Add new units here
#[derive(Debug, Clone, Copy)]
pub enum Distance {
    Meters(f64),
    Centimeters(f64),
    Millimeters(f64),
    Killometers(f64),
}  //  << ----------   new unit here  *****************


/// You have to add new units here (and two conversions)
impl Distance {
    fn unwrap_f64_meters(&self) -> f64 {
        match self {
            &Distance::Meters(i) => i,
            &Distance::Centimeters(i) => i / 100.,
            &Distance::Millimeters(i) => i / 1_000.,
            &Distance::Killometers(i) => i * 1_000.,
            //  <<---------  new unit here  **************
        }
    }
}


impl PartialEq for Distance {
    fn eq(&self, other: &Distance) -> bool {
        //  TODO: this comparision is risky
        self.unwrap_f64_meters() == other.unwrap_f64_meters()
    }
}

#[test]
fn test_equallity() {
    use super::test_diff;
    use self::Distance::*;
    assert_eq!(Meters(2.), Meters(2.));
    assert_eq!(Meters(2.), Millimeters(2000.));
    assert_eq_dif!(Meters(2.), Millimeters(2000.));
}



impl ::std::ops::Add for Distance {
    type Output = Distance;

    fn add(self, r: Distance) -> Distance {
        let self_im = self.unwrap_f64_meters();
        let r_im = r.unwrap_f64_meters();
        Distance::Meters(self_im + r_im)
    }
}

#[test]
fn test_add() {
    use super::test_diff;
    use self::Distance::*;
    assert_eq_dif!(Meters(2.) + Meters(2.), Meters(4.));
    assert_eq_dif!(Meters(2.) + Centimeters(20.) + Millimeters(4.),
                   Millimeters(2204.));
    assert_eq_dif!(Meters(2.) - Centimeters(20.) + Millimeters(4.),
                   Millimeters(1_804.));
}


impl ::std::ops::Mul<Distance> for f64 {
    type Output = Distance;

    fn mul(self, r: Distance) -> Distance {
        let r_im = r.unwrap_f64_meters();
        Distance::Meters(r_im * self)
    }
}

impl ::std::ops::Mul<f64> for Distance {
    type Output = Distance;

    fn mul(self, r: f64) -> Distance {
        r * self
    }
}

#[test]
fn test_mult() {
    use super::test_diff;
    use self::Distance::*;
    assert_eq_dif!(2. * Meters(2.), Meters(4.));
    assert_eq_dif!(2. * Meters(2.002), Millimeters(4_004.));
    assert_eq_dif!(Meters(2.) * 2., Meters(4.));
    assert_eq_dif!(Meters(2.002) * 2., Millimeters(4_004.));
}


impl ::std::ops::Sub for Distance {
    type Output = Distance;

    fn sub(self, r: Distance) -> Distance {
        self + (-1. * r)
    }
}

#[test]
fn test_sub() {
    use super::test_diff;
    use self::Distance::*;
    assert_eq_dif!(Meters(2.) - Meters(4.), Meters(-2.));
    assert_eq_dif!(Meters(2.) - Millimeters(400.), Meters(1.6));
}

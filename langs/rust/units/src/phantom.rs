//! Distance2 units
//!
//! Just an exercice
//!
//! # Examples
//!
//! ```rust
//! use std::marker::PhantomData;
//! use units::phantom::Unit;
//!
//! enum Meter {};
//! #[allow(non_upper_case_globals)]
//! const m: Unit<f64, Meter> = Unit(1., PhantomData);
//!
//! enum Centimeter {};
//! #[allow(non_upper_case_globals)]
//! const cm: Unit<f64, Centimeter> = Unit(1., PhantomData);
//!
//! assert!(2. * m + 3. * m == 5. * m);
//! assert!(2. * m + 3. * m == 5. * m);
//! assert!(3. * cm + 5. * cm == 8. * cm);
//! assert!(3. * cm  != 8. * cm);
//! // assert!(3. * m + 5. * cm == 8. * m);  //~ ERROR mixing dimensions
//! // assert!(3. * m == 300. * cm);  //~ ERROR mixing dimensions
//!
//!
//! enum NumEggs {};
//! #[allow(non_upper_case_globals)]
//! const neggs: Unit<f64, NumEggs> = Unit(1., PhantomData);
//!
//! assert!(2. * neggs + 3. * neggs == 5. * neggs);
//!
//! ```
//!
//! Remove boiler plate with a macro
//!
//! ```
//! #[macro_use]
//! #[path="src/phantom.rs"]mod units;
//! use units::phantom::Unit;
//!
//! define_unit!(Meter, m, f64);
//! define_unit!(NumEggs, neggs, f64);
//!
//! assert!(2. * m + 3. * m == 5. * m);
//! assert!(2. * m + 3. * m != 6. * m);
//! assert!(neggs + 3. * neggs == 4. * neggs);
//! //  assert!(neggs + 3. * neggs == 4. * m);  //~ERROR different units
//! ```





use std::marker::PhantomData;


#[derive(Debug)]
pub struct Unit<T, UNIT>(pub T, pub PhantomData<UNIT>);


pub fn unit_f64<UNIT>(val: f64) -> Unit<f64, UNIT> {
    Unit(val, PhantomData)
}


impl<UNIT, T: PartialEq> PartialEq for Unit<T, UNIT> {
    fn eq(&self, other: &Unit<T, UNIT>) -> bool {
        let &Unit::<T, UNIT>(ref iself, ::std::marker::PhantomData) = self;
        let &Unit::<T, UNIT>(ref iother, ::std::marker::PhantomData) = other;
        iself == iother
    }
}



#[test]
fn test_equal() {
    enum Meter {};
    enum Centimeter {};

    assert!(unit_f64::<Meter>(1.) == unit_f64::<Meter>(1.));
    assert!(unit_f64::<Centimeter>(1.) == unit_f64::<Centimeter>(1.));
    // assert!(unit_f64::<Meter>(1.) == unit_f64::<Centimeter>(1.));   //~ ERROR Diferent units
}




impl<UNIT> ::std::ops::Mul<Unit<f64, UNIT>> for f64 {
    type Output = Unit<f64, UNIT>;

    fn mul(self, other: Unit<f64, UNIT>) -> Unit<f64, UNIT> {
        let Unit::<f64, UNIT>(iother, unit) = other;
        Unit(iother * self, unit)
    }
}

impl<UNIT, T> ::std::ops::Mul<T> for Unit<T, UNIT>
    where T: ::std::ops::Mul<T, Output = T>
{
    type Output = Unit<T, UNIT>;

    fn mul(self, other: T) -> Unit<T, UNIT> {
        let Unit::<T, UNIT>(iself, unit) = self;
        Unit(iself * other, unit)
    }
}



#[test]
fn test_mult_ones() {
    enum Meter {};
    enum Centimeter {};

    #[allow(non_upper_case_globals)]
    const m: Unit<f64, Meter> = Unit(1., PhantomData);
    #[allow(non_upper_case_globals)]
    const cm: Unit<f64, Centimeter> = Unit(1., PhantomData);

    assert!(2. * m == Unit::<_, Meter>(2., PhantomData));
    assert!(2. * m == Unit::<_, Meter>(2., PhantomData));
    assert!(3. * cm == Unit::<_, Centimeter>(3., PhantomData));
    assert!(3. * cm != Unit::<_, Centimeter>(4., PhantomData));
}



impl<UNIT> ::std::ops::Add<Unit<f64, UNIT>> for Unit<f64, UNIT> {
    type Output = Unit<f64, UNIT>;

    fn add(self, other: Unit<f64, UNIT>) -> Unit<f64, UNIT> {
        let Unit::<f64, UNIT>(iself, unit) = self;
        let Unit::<f64, UNIT>(iother, _) = other;
        Unit(iother + iself, unit)
    }
}


#[test]
fn test_add() {
    enum Meter {};
    enum Centimeter {};

    #[allow(non_upper_case_globals)]
    const m: Unit<f64, Meter> = Unit(1., PhantomData);
    #[allow(non_upper_case_globals)]
    const cm: Unit<f64, Centimeter> = Unit(1., PhantomData);

    assert!(2. * m + 3. * m == 5. * m);
    assert!(2. * m + 3. * m == 5. * m);
    assert!(3. * cm + 5. * cm == 8. * cm);
    // assert!(3. * m + 5. * cm == 8. * m);  //~ ERROR mixing dimensions
    // assert!(3. * m == 300. * cm);  //~ ERROR mixing dimensions
}


#[macro_export]
macro_rules! define_unit_internal {
    ( $phanton_type:ident,  $unit_symbol:ident, $_type:ty)  => (
        enum $phanton_type {};

        #[allow(non_upper_case_globals)]
        const $unit_symbol: $crate::phantom::Unit<$_type, $phanton_type> = 
                    $crate::phantom::Unit(1., ::std::marker::PhantomData);
    )
}

#[macro_export]
macro_rules! define_unit {
    ( $phanton_type:ident,  $unit_symbol:ident, $_type:ty)  => (
        enum $phanton_type {};

        #[allow(non_upper_case_globals)]
        const $unit_symbol: Unit<$_type, $phanton_type> = 
                    Unit(1., ::std::marker::PhantomData);
    )
}

#[macro_export]
macro_rules! define_unit3 {
    ( $phanton_type:ident,  $unit_symbol:ident, $_type:ty)  => (
        enum $phanton_type {};

        #[allow(non_upper_case_globals)]
        const $unit_symbol: unit::phantom::Unit<$_type, $phanton_type> = 
                    Unit(1., ::std::marker::PhantomData);
    )
}


#[test]
fn test_macro() {
    define_unit_internal!(Meter, m, f64);
    define_unit_internal!(Centimeter, cm, f64);

    assert!(2. * m + 3. * m == 5. * m);
    assert!(2. * m + 3. * m == 5. * m);
    assert!(3. * cm + 5. * cm == 8. * cm);
    assert!(3. * cm + 5. * cm != 9. * cm);
    // assert!(3. * m + 5. * cm == 8. * m);  //~ ERROR mixing dimensions
    // assert!(3. * m == 300. * cm);  //~ ERROR mixing dimensions
}

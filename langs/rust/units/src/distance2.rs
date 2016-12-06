use std::marker::PhantomData;


#[derive(Debug)]
pub struct Unit<T, UNIT>(pub T, PhantomData<UNIT>);


pub fn unit<UNIT>(val: f64) -> Unit<f64, UNIT> {
    Unit(val, PhantomData)
}


impl<UNIT, T: Clone + PartialEq> PartialEq for Unit<T, UNIT> {
    fn eq(&self, other: &Unit<T, UNIT>) -> bool {
        let &Unit::<T, UNIT>(ref iself, ::std::marker::PhantomData) = self;
        let &Unit::<T, UNIT>(ref iother, ::std::marker::PhantomData) = other;
        *iself == *iother
    }
}



#[test]
fn test_equal() {
    enum Meter {};
    enum Centimeter {};

    assert!(unit::<Meter>(1.) == unit::<Meter>(1.));
    assert!(unit::<Centimeter>(1.) == unit::<Centimeter>(1.));
    // assert!(unit::<Meter>(1.) == unit::<Centimeter>(1.));   //~ ERROR Diferent units
}




impl<'a, UNIT> ::std::ops::Mul<Unit<f64, UNIT>> for &'a f64 {
    type Output = Unit<f64, UNIT>;

    fn mul(self, other: Unit<f64, UNIT>) -> Unit<f64, UNIT> {
        let Unit::<f64, UNIT>(iother, unit) = other;
        Unit(iother * self, unit)
    }
}

impl<'a, UNIT, T> ::std::ops::Mul<T> for &'a Unit<T, UNIT>
    where T: ::std::ops::Mul<T, Output = T>
{
    type Output = Unit<T, UNIT>;

    fn mul(self, other: T) -> Unit<T, UNIT> {
        let &Unit::<T, UNIT>(ref iself, unit) = self;
        Unit(*iself * other, unit)
    }
}

#[test]
fn test_mult() {
    #[derive(Debug)]
    enum Meter {};
    #[derive(Debug)]
    enum Centimeter {};

    // assert_eq!(1. * unit::<Meter>(2.), unit::<Meter>(2.));
    // assert_eq!(1. * unit::<Centimeter>(2.), unit::<Centimeter>(2.));
    // assert_eq!(2. * unit::<Meter>(2.), unit::<Meter>(4.));
    // assert_eq!(unit::<Meter>(2.) * 2., unit::<Meter>(4.));
}

#[test]
fn test_mult_ones() {
    #[derive(Debug)]
    enum Meter {};
    #[derive(Debug)]
    enum Centimeter {};

    let m = unit::<Meter>(1.);
    let cm = unit::<Centimeter>(1.);

    // assert_eq!(2. * m, unit::<Meter>(2.));
    // assert_eq!(3. * m, unit::<Meter>(3.));
    // assert_eq!(2. * cm, unit::<Centimeter>(2.));
    // assert_eq!(m * 2., unit::<Meter>(2.));
    // assert_eq!(m * 3., unit::<Meter>(3.));
}

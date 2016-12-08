use std::marker::PhantomData;


#[derive(Debug)]
pub struct Unit<T, UNIT>(pub T, PhantomData<UNIT>);


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




impl<'a, UNIT> ::std::ops::Mul<&'a Unit<f64, UNIT>> for f64 {
    type Output = Unit<f64, UNIT>;

    fn mul(self, other: &Unit<f64, UNIT>) -> Unit<f64, UNIT> {
        let &Unit::<f64, UNIT>(iother, unit) = other;
        Unit(iother * self, unit)
    }
}

impl<'a, UNIT, T> ::std::ops::Mul<T> for &'a Unit<T, UNIT>
    where T: ::std::ops::Mul<T, Output = T> + Clone
{
    type Output = Unit<T, UNIT>;

    fn mul(self, other: T) -> Unit<T, UNIT> {
        let &Unit::<T, UNIT>(ref iself, unit) = self;
        Unit(iself.clone() * other, unit)
    }
}

#[test]
fn test_mult() {
    #[derive(Debug)]
    enum Meter {};
    #[derive(Debug)]
    enum Centimeter {};

    assert_eq!(1. * &unit_f64::<Meter>(2.), unit_f64::<Meter>(2.));
    assert_eq!(1. * &unit_f64::<Centimeter>(2.), unit_f64::<Centimeter>(2.));
    assert_eq!(2. * &unit_f64::<Meter>(2.), unit_f64::<Meter>(4.));
    assert_eq!(&unit_f64::<Meter>(2.) * 2., unit_f64::<Meter>(4.));
}

#[test]
fn test_mult_ones() {
    enum Meter {};
    enum Centimeter {};

    let m = &unit_f64::<Meter>(1.);
    let cm = &unit_f64::<Centimeter>(1.);

    assert!(2. * m == unit_f64::<Meter>(2.));
    assert!(3. * m == unit_f64::<Meter>(3.));
    assert!(2. * cm == unit_f64::<Centimeter>(2.));
    assert!(m * 2. == unit_f64::<Meter>(2.));
    assert!(m * 3. == unit_f64::<Meter>(3.));
}

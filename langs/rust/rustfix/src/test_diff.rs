extern crate difference;
extern crate term;
use std::io::Write;
use self::difference::Difference;


macro_rules!  assert_eq_dif {
    ($f:expr, $s:expr)  => (
        if $f != $s {
            let (fs, ss) = (format!("{:?}", $f), format!("{:?}", $s));
            test_diff::print_diff(fs, ss);
        }
        assert!($f == $s);
    );
}

pub fn print_diff(text1: String, text2: String) {
    let mut t = term::stdout().unwrap();

    let (_, changes) = difference::diff(&text1, &text2, "");

    for c in changes.iter() {
        match c {
            &Difference::Same(ref z) => {
                t.fg(term::color::RED).unwrap();
                let _ = write!(t, "{}", z);
            }
            &Difference::Rem(ref z) => {
                t.fg(term::color::WHITE).unwrap();
                t.bg(term::color::RED).unwrap();
                let _ = write!(t, "{}", z);
                t.reset().unwrap();
            }
            _ => (),
        }
    }
    t.reset().unwrap();

    let _ = writeln!(t, "");

    for c in changes.iter() {
        match c {
            &Difference::Same(ref z) => {
                t.fg(term::color::GREEN).unwrap();
                let _ = write!(t, "{}", z);
            }
            &Difference::Add(ref z) => {
                t.fg(term::color::WHITE).unwrap();
                t.bg(term::color::GREEN).unwrap();
                let _ = write!(t, "{}", z);
                t.reset().unwrap();
            }
            _ => (),
        }
    }
    let _ = write!(t, "\n");
    t.reset().unwrap();
    t.flush().unwrap();
}

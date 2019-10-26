// Write a simple parser that will parse and run Deadfish.

// Deadfish has 4 commands, each 1 character long:

//     i increments the value (initially 0)
//     d decrements the value
//     s squares the value
//     o outputs the value into the return array

// Invalid characters should be ignored.

fn parse(code: &str) -> Vec<i32> {
    let (_curr, acc) = code.chars().fold((0, vec![]), |(curr, acc), ch| match ch {
        'i' => (curr + 1, acc),
        'd' => (curr - 1, acc),
        's' => (curr * curr, acc),
        'o' => (curr, acc.ipush(curr)),
        _ => panic!("invalid input char"),
    });
    acc
}

pub trait IVec<T> {
    fn ipush(self, _: T) -> Self;
}

impl<T> IVec<T> for Vec<T> {
    fn ipush(mut self, v: T) -> Self {
        self.push(v);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn sample_tests() {
        assert_eq!(parse("iiisdoso"), vec![8, 64]);
        assert_eq!(parse("iiisdosodddddiso"), vec![8, 64, 3600]);
    }
}

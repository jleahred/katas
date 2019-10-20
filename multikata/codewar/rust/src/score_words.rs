fn high(input: &str) -> &str {
    input
        .split_ascii_whitespace()
        .max_by_key(|w| score_word(w))
        .unwrap_or("")
}

fn score_word(word: &str) -> u64 {
    word.chars()
        .fold(0, |acc, ch| acc + ch as u64 - 'a' as u64 + 1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic() {
        assert_eq!(high("man i need a taxi up to ubud"), "taxi");
        assert_eq!(high("what time are we climbing up the volcano"), "volcano");
        assert_eq!(high("take me to semynak"), "semynak");
        assert_eq!(high("massage yes massage yes massage"), "massage");
        assert_eq!(high("take two bintang and a dance please"), "bintang");
        assert_eq!(high("aaa b"), "aaa");
        assert_eq!(high(""), "");
    }
}

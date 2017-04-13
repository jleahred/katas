pub mod ind_tok;
mod process_line;





#[cfg(test)]
mod tests {
    pub use ind_tok;


    #[test]
    fn single_line() {
        let result = ind_tok::tokenize("....").unwrap();
        println!("{:?}________________", result);
        assert!(result.len() == 1);
    }

    #[test]
    fn empty_input() {
        let result = ind_tok::tokenize("").unwrap();
        assert!(result.len() == 0);
    }

    #[test]
    fn empty_lines() {
        let result = ind_tok::tokenize("

    ")
            .unwrap();
        assert!(result.len() == 0);


        let result = ind_tok::tokenize("
            ")
            .unwrap();
        assert!(result.len() == 0);
    }



}

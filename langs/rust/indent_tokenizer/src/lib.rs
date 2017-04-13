pub mod ind_tok;
mod process_line;





#[cfg(test)]
mod tests {
    pub use ind_tok;


    #[test]
    fn single_line() {
        let result = ind_tok::tokenize("....").unwrap();
        assert!(result.len() == 1);
        assert!(result[0].lines.len() == 1);
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

    #[test]
    fn some_lines_one_token() {
        let result = ind_tok::tokenize("....
....")
            .unwrap();
        assert!(result.len() == 1);
        assert!(result[0].lines.len() == 2);


        let result = ind_tok::tokenize("....
....
....
....")
            .unwrap();
        assert!(result.len() == 1);
        assert!(result[0].lines.len() == 4);
    }

    #[test]
    fn some_tokens_root_level() {
        let result = ind_tok::tokenize("1111

2222

3333
")
            .unwrap();
        assert!(result.len() == 3);
        assert!(result[0].lines[0] == "1111");
        assert!(result[1].lines[0] == "2222");
        assert!(result[2].lines[0] == "3333");



        let result = ind_tok::tokenize("00
01
02

10
11
12
13

20
21

")
            .unwrap();
        assert!(result.len() == 3);
        assert!(result[0].lines[0] == "00");
        assert!(result[0].lines[1] == "01");
        assert!(result[0].lines[2] == "02");
        assert!(result[1].lines[0] == "10");
        assert!(result[1].lines[1] == "11");
        assert!(result[1].lines[2] == "12");
        assert!(result[1].lines[3] == "13");
        assert!(result[2].lines[0] == "20");
        assert!(result[2].lines[1] == "21");
    }
}

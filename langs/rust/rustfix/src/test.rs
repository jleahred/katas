// use msg_parse::{ParsingResult, Parsing, add_char};

// use test_diff;


// pub const MSG_TEST_WIKIPEDIA1: &'static str = "8=FIX.4.2|9=178|35=8|49=PHLX|56=PERS|52=20071123-05:\
//                                                30:00.000|11=ATOMNOCCC9990900|20=3|150=E|39=E|55=MSFT|167=CS|54=1|38=15|40=2|44=15|58=PHLX \
//                                                EQUITY TESTING|59=0|47=C|32=0|31=0|151=15|14=0|6=0|10=128|";



// fn add_char_incomplete(mut parser: Parsing, ch: char) -> Parsing {
//     match add_char(parser, ch) {
//         ParsingResult::Parsing(parsing) => parser = parsing,
//         ParsingResult::ParsedOK(_) => panic!("ParsedOK in icomplete message"),
//         ParsingResult::ParsedErrors { parsed: _, errors: _ } => {
//             panic!("ParsedErrors in icomplete message")
//         }
//     }
//     parser
// }


// #[test]
// fn init_add_char() {
//     let mut parser = Parsing::new();

//     parser = add_char_incomplete(parser, '1');

//     ass_eqdf!{
//         parser.parsed.orig_msg => "1".to_string(),
//         parser.parsed.msg_length => 1,
//         parser.reading_tag => 1
//     };
// }

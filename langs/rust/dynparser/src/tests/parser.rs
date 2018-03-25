// #[test]
// fn parse_literal() {
//     let rules = map!(symbol("main") => lit("aaaa"));
//     let parsed = parse(&text2parse("aaaa"), &symbol("main"), &rules);
//     assert!(parsed.is_ok());

//     let rules = map!(symbol("main") => lit("aaaa"));
//     let parsed = parse(&text2parse("aaa"), &symbol("main"), &rules);
//     assert!(parsed.is_err());

//     let rules = map!(symbol("main") => lit("aaaa"));
//     let parsed = parse(&text2parse("aaaaa"), &symbol("main"), &rules);
//     assert!(parsed.is_err());

//     let rules = map!(symbol("main") => lit("aaaa"));
//     let parsed = parse(&text2parse("bbbb"), &symbol("main"), &rules);
//     assert!(parsed.is_err());
// }

//  move tags and errors to different files


use msg_parse::{ParsingInfo, add_char, errors, ParsingState};

use test_diff;


macro_rules! hashmap {
    ($( $key: expr => $val: expr ),*) => {{
         let mut map = ::std::collections::HashMap::new();
         $( map.insert($key, $val); )*
         map
    }}
    }

macro_rules! btree {
    ($( $key: expr => $val: expr ),*) => {{
         let mut result = ::std::collections::BTreeMap::new();
         $( result.insert($key, $val); )*
         result
    }}
    }


fn add_chars(mut pi: ParsingInfo, s: &'static str) -> ParsingInfo {
    for ch in s.chars() {
        pi = add_char(pi,
                      if ch == '^' {
                          1u8 as char
                      } else {
                          ch
                      });
    }
    pi
}





#[test]
fn init_add_char() {
    let mut parser = ParsingInfo::new();
    parser = add_char(parser, '1');

    ass_eqdf!{
        parser.parsed.orig_msg => "1".to_string(),
        parser.parsed.msg_length => 1,
        parser.reading_tag => 1
    };
}


#[test]
fn invalid_first_char() {
    let mut parser = ParsingInfo::new();
    parser = add_char(parser, 'a');

    ass_eqdf!{
        parser.parsed.orig_msg => "a".to_string(),
        parser.parsed.msg_length => 1,
        parser.reading_tag => 0
    };
}

#[test]
fn invalid_second_char() {
    let mut parser = ParsingInfo::new();
    parser = add_char(parser, '1');
    parser = add_char(parser, 'a');

    ass_eqdf!{
        parser.parsed.orig_msg => "1a".to_string(),
        parser.parsed.msg_length => 2,
        parser.reading_tag => 1
    };
}


#[test]
fn invalid_chars_2errors() {
    let mut parser = ParsingInfo::new();
    parser = add_char(parser, '1');
    parser = add_char(parser, 'a');
    parser = add_char(parser, 'b');

    ass_eqdf!{
        parser.parsed.orig_msg => "1ab".to_string(),
        parser.parsed.msg_length => 3,
        parser.reading_tag => 1,
        parser.current_field_error =>
                    Some((2, errors::TAG_INVALID_CHAR))
    };
}

#[test]
fn invalid_chars_2errors_andvalids() {
    let mut parser = ParsingInfo::new();
    parser = add_chars(parser, "12ab34");

    ass_eqdf!{
        parser.parsed.orig_msg => "12ab34".to_string(),
        parser.parsed.msg_length => 6,
        parser.reading_tag => 12,
        parser.current_field_error =>
                    Some((3, errors::TAG_INVALID_CHAR))
    };
}


#[test]
fn invalid_chars_2errors_and_valids_non_consecutives() {
    let mut parser = ParsingInfo::new();
    parser = add_chars(parser, "12a3b45");

    ass_eqdf!{
        parser.parsed.orig_msg => "12a3b45".to_string(),
        parser.parsed.msg_length => 7,
        parser.reading_tag => 12,
        parser.current_field_error =>
                    Some((3, errors::TAG_INVALID_CHAR))
    };
}


//  too long
//      error and igonre big tag

#[test]
fn too_long_tag() {
    let mut parser = ParsingInfo::new();
    parser = add_chars(parser, "1234567890");

    ass_eqdf!{
        parser.parsed.orig_msg => "1234567890".to_string(),
        parser.parsed.msg_length =>  10,
        parser.reading_tag =>  123_456_7,
        parser.current_field_error => Some((7, errors::TAG_TOO_LONG))
    };
}


#[test]
fn too_long_tag_ignore_excess() {
    let mut parser = ParsingInfo::new();
    parser = add_chars(parser, "123456789012345");

    ass_eqdf!{
        parser.parsed.orig_msg => "123456789012345".to_string(),
        parser.parsed.msg_length => 15,
        parser.reading_tag => 123_456_7,
        parser.current_field_error => Some((7, errors::TAG_TOO_LONG))
    };
}



//  =
//      start receiving val
#[test]
fn finish_tag() {
    let mut parser = ParsingInfo::new();
    parser = add_chars(parser, "123456=");

    ass_eqdf!{
        parser.parsed.orig_msg => "123456=".to_string(),
        parser.parsed.msg_length=>  7,
        parser.reading_tag => 123_456,
        parser.state => ParsingState::StReadingValue
    };
}




//  receiving val
//      "a"
#[test]
fn reading_val() {
    let mut parser = ParsingInfo::new();
    parser = add_chars(parser, "123456=a");

    ass_eqdf!{
        parser.parsed.orig_msg => "123456=a".to_string(),
        parser.parsed.msg_length => 8,
        parser.reading_tag => 123_456,
        parser.reading_val => "a".to_string(),
        parser.state => ParsingState::StReadingValue
    };
}


//      "abcdefg"
#[test]
fn reading_val2() {
    let mut parser = ParsingInfo::new();
    parser = add_chars(parser, "123456=abcdefg");

    ass_eqdf!{
        parser.parsed.orig_msg => "123456=abcdefg".to_string(),
        parser.parsed.msg_length => 14,
        parser.reading_tag => 123_456,
        parser.reading_val => "abcdefg".to_string(),
        parser.state => ParsingState::StReadingValue
    };
}


//      too long val received
#[test]
fn too_long_val() {
    let mut parser = ParsingInfo::new();
    parser = add_chars(parser,
                       "123456=abcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefg");

    ass_eqdf!{
        parser.parsed.orig_msg => 
            "123456=abcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefg"
            .to_string(),
        parser.parsed.msg_length => 77,
        parser.reading_tag => 123_456,
        parser.reading_val => "abcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefga".to_string(),
        parser.state => ParsingState::StReadingValue,
        parser.current_field_error => Some((57, errors::VAL_TOO_LONG))
    };
}


//  received field  0x01
//      insert in map
#[test]
fn complete_field() {
    let mut parser = ParsingInfo::new();
    parser = add_chars(parser, "123456=abcdefg\u{01}");


    ass_eqdf!{
        parser.parsed.msg_map => btree![123456 => "abcdefg".to_string()],
        parser.parsed.orig_msg => "123456=abcdefg^".to_string(),
        parser.parsed.msg_length => 15,
        parser.reading_tag => 0,
        parser.reading_val => "".to_string(),
        parser.state => ParsingState::StReadingValue
    };
}



//  completed two fields

//  check_body_length1
//  2 fields


//  check_body_length1
//  3 fields, one is tags::BODY_LENGTH


// check position fields
//  ...



//  completed field with two errors



//  received field  0x01 ERROR
//      at the beginning of tag
//      reading tag
//      after =


//  detected end of message
//      finished status
//      check message length
//      check original message
//      check checksum

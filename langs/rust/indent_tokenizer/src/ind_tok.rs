// use std::str::Lines;

// pub fn split_lines(s: &String) -> Lines {
//     s.lines()
// }

const INDENT_CHAR: char = '|';
const EOL_CHAR: char = '|';

#[derive(Eq, PartialEq)]
struct LineInfo {
    indent: usize,
    content: String,
}


fn process_line(line: &String) -> Option<LineInfo> {
    let mut indent = 0;
    let mut lresult = String::new();
    let mut located_start_indent = false;
    for c in line.chars() {
        if located_start_indent == false {
            if c == ' ' {
                indent += 1;
            } else if c == INDENT_CHAR {
                indent += 1;
                located_start_indent = true;
            } else {
                located_start_indent = true;
                lresult.push(c);
            }
        } else {
            lresult.push(c);
        }
    }
    if lresult.is_empty() && !located_start_indent {
        None
    } else {
        if lresult.chars().last() == Some(EOL_CHAR) {
            lresult.pop();
        }

        Some(LineInfo {
            indent: indent,
            content: lresult,
        })
    }
}

#[test]
fn test_process_line() {
    //  simple -------------------------------------------
    assert!(process_line(&"    abcdef".to_string()) ==
            Some(LineInfo {
        indent: 4,
        content: "abcdef".to_string(),
    }));

    assert!(process_line(&" abcdef".to_string()) ==
            Some(LineInfo {
        indent: 1,
        content: "abcdef".to_string(),
    }));

    //  empty line --------------------------------------
    assert!(process_line(&"".to_string()) == None);

    assert!(process_line(&"  ".to_string()) == None);


    //  not indented-------------------------------------
    assert!(process_line(&"abcdef".to_string()) ==
            Some(LineInfo {
        indent: 0,
        content: "abcdef".to_string(),
    }));

    //  indentation indicator ---------------------------
    assert!(process_line(&"   |abcdef".to_string()) ==
            Some(LineInfo {
        indent: 4,
        content: "abcdef".to_string(),
    }));

    assert!(process_line(&"|abcdef".to_string()) ==
            Some(LineInfo {
        indent: 1,
        content: "abcdef".to_string(),
    }));

    assert!(process_line(&"   |".to_string()) ==
            Some(LineInfo {
        indent: 4,
        content: "".to_string(),
    }));

    assert!(process_line(&"|abcdef".to_string()) ==
            Some(LineInfo {
        indent: 1,
        content: "abcdef".to_string(),
    }));

    //  spaces at end of line ---------------------------
    assert!(process_line(&"    abcdef  ".to_string()) ==
            Some(LineInfo {
        indent: 4,
        content: "abcdef  ".to_string(),
    }));

    assert!(process_line(&"    abcdef  |".to_string()) ==
            Some(LineInfo {
        indent: 4,
        content: "abcdef  ".to_string(),
    }));

    assert!(process_line(&"   |  |".to_string()) ==
            Some(LineInfo {
        indent: 4,
        content: "  ".to_string(),
    }));

    //  pipe end of line ---------------------------
    assert!(process_line(&"    abcdef  ||".to_string()) ==
            Some(LineInfo {
        indent: 4,
        content: "abcdef  |".to_string(),
    }));

}

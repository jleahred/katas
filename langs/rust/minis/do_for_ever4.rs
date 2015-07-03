fn make_a_division_for_ever() {
    loop {
        println!("Enter divisor...");

        let line_resutl :Result<String, std::io::IoError>  = std::io::stdin().read_line();

        let line_string : String = match line_resutl {
            Ok(line) => line,
            Err(why) => why.desc.to_string(),
        };

        let line_string_trimmed : &str = line_string.trim();

        let parsed_string2_u32 : Option<u32> = line_string_trimmed.parse::<u32>();

        let readed_u32 : u32 = match parsed_string2_u32 {
            Some(value) => value,
            None        => { println!("Invalid value, I will die"); panic!("Invalid value") }
        }

        println!("readed {}", readed_u32);

        let dangerous : u32 =  1_000_000 / divisor;

        println!("DIV RESULT... {}", dangerous);

    }
} 

// Add a test to verify deserialization
#[cfg(test)]
mod tests {
    use super::super::*;
    use toml;
    #[test]
    fn test_toml_parsing() {
        let toml_data = r#"
            uid = "550e8400-e29b-41d4-a716-446655440000"
            file_format = "0"

            [[process]]
            id = "example_process"
            command = "echo 'Hello, World!' && sleep 10"
            apply_on = "2023-10-01T12:00:00"

            [[process]]
            id = "PRUEBA_C"
            command = "echo $(date)  '  bb'"
            apply_on = "2029-11-01T12:20:00"

            [[process]]
            id = "example_process"
            command = "echo 'Hello, World!' && sleep 10"
            apply_on = "2023-10-01T12:00:00"
            # week_days = ["mon", "wed", "thu", "fri"]     # optional  
            week_days = "mon-fri"   # optional  
            start_time = "09:00:00" # optional
            stop_time = "23:00:00"  # optional
            type = "normal"         # optional  normal/fake

            [[process]]
            id = "example_process"
            command = "echo 'Hello, World!' && sleep 10"
            apply_on = "2023-10-01T12:00:00"
            # week_days = ["mon", "wed", "thu", "fri"]     # optional  
            week_days = "mon-fri"   # optional  
            start_time = "09:00:00" # optional
            stop_time = "23:00:00"  # optional
            type = "normal"         # optional  normal/fake

        "#;

        // Parse the TOML data
        let parsed: Result<Config, _> = toml::from_str(toml_data);

        // Assert that parsing was successful, providing error details if it fails
        assert!(
            parsed.is_ok(),
            "Failed to parse TOML data:\nError: {}\nRaw Data: {:?}",
            parsed
                .as_ref()
                .err()
                .map(|e| e.to_string())
                .unwrap_or_default(),
            toml_data
        );

        // Verify the parsed data
        let config = parsed.unwrap();
        assert_eq!(config.process.as_ref().unwrap().len(), 4);
        assert_eq!(config.process.as_ref().unwrap()[0].id, "example_process");
        assert_eq!(config.process.as_ref().unwrap()[1].id, "PRUEBA_C");
    }
}

use super::expr::builders::*;
use super::parse;

#[test]
fn test_single_literal_longer() {
    let rules = rules! {"main" => ri(lit("hello")) };

    parse("hello world", &rules).err().unwrap();
}

#[test]
fn test_single_literal_shorted() {
    let rules = rules! {"main" => ri(lit("hello")) };

    parse("hell", &rules).err().unwrap();
}

#[test]
fn test_single_literal_empty() {
    let rules = rules! {"main" => ri(lit("hello")) };

    parse("", &rules).err().unwrap();
}

#[test]
fn test_simple_and() {
    let rules = rules! {
        "main" =>
            rand!(
                lit("hello"),
                lit(" "),
                lit("world")
    )};

    parse("hello world", &rules).ok().unwrap();
}

#[test]
fn test_simple_and_fail() {
    let rules = rules! {
        "main" =>
                rand!(
                    lit("hello"),
                    lit(" "),
                    lit("world")
    )};

    parse("hello", &rules).err().unwrap();
}

#[test]
fn test_simple_and_eof() {
    let rules = rules! {
        "main" =>
            rand!(
                lit("hello"),
                lit(" "),
                lit("world"),
                eof()
    )};

    parse("hello world", &rules).ok().unwrap();
}

#[test]
fn test_simple_and_eof2() {
    let rules = rules! {
        "main" =>
            rand!(
                lit("hello"),
                lit(" "),
                lit("world"),
                eof()
    )};

    parse("hello world...", &rules).err().unwrap();
}

#[test]
fn test_simple_or() {
    let rules = rules! {
        "main" =>
            ror!(
                lit("hello"),
                lit("hi"),
                lit("hola")
    )};

    parse("hello", &rules).ok().unwrap();
    parse("hi", &rules).ok().unwrap();
    parse("hola", &rules).ok().unwrap();
    parse("jambo", &rules).err().unwrap();
}

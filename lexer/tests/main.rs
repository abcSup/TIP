use std::fs;
use lexer::{Lexer, Token};

#[test]
fn lex_all_files() {
    for file in fs::read_dir("../examples").unwrap() {
        match file {
            Ok(entry) => {
                println!("Lexing {:?}...", entry.path());
                let bytes = fs::read(entry.path()).unwrap();
                let content = String::from_utf8(bytes).unwrap();
                let lexer = Lexer::new(&content);
                let _tokens: Vec<Token> = lexer.into_iter().collect();
            }
            _ => panic!()
        }
    }
}
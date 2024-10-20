use std::fs;
use parser::Parser;

fn main() {
    for file in fs::read_dir("../examples").unwrap() {
        match file {
            Ok(entry) => {
                let path = &entry.path();
                let filename = path.iter().last().unwrap().to_string_lossy();
                let is_error = filename.starts_with("err_");

                println!("Parsing {:?}...", filename);
                let bytes = fs::read(path).unwrap();
                let content = String::from_utf8(bytes).unwrap();

                let mut parser = Parser::new(&content);
                let program = parser.parse_program();
                if is_error {
                    // assert!(program.is_err());
                } else {
                    let program = program.unwrap();
                    // println!("{:#?}", program);
                }
            }
            _ => panic!()
        }
    }
}
mod token;
mod lexer;
mod parser;

use lexer::Lexer;
use parser::Parser;

use std::fs;

fn main() {
    let contents = fs::read_to_string("main.ur")
        .expect("Something went wrong reading the file");
    println!("'{}' read as content", contents);
    let tokens = Lexer::new(&contents);
    let program = Parser::new(tokens).program();
    println!("{:?}", program);
}


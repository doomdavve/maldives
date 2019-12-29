mod token;
mod lexer;
mod parser;

use token::Token;
use lexer::Lexer;

use std::fs;

fn main() {
    let contents = fs::read_to_string("main.ur")
        .expect("Something went wrong reading the file");
    println!("'{}' read as content", contents);
    let _tokens: Vec<Token> = Lexer::new(&contents).collect();
    //    let program = parser::parse(&tokens);
    //println!("{:?}", program);
}


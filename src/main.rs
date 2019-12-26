mod token;
mod lexer;

use token::TokenType;
use lexer::Lexer;

use std::fs;

fn main() {
    let contents = fs::read_to_string("main.ur")
        .expect("Something went wrong reading the file");

    let mut lexer = Lexer::new(&contents);
    while let Some(token) = lexer.next() {
        if token.token_type == TokenType::Error {
            println!("Error: {:?}", token);
            break;
        }
        println!("{:?}", token);
    }
}

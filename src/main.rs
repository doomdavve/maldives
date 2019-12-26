mod token;
mod lexer;

use std::fs;

fn main() {
    let contents = fs::read_to_string("main.ur")
        .expect("Something went wrong reading the file");

    let mut lexer = lexer::Lexer::new(&contents);
    while let Some(token) = lexer.next() {
        println!("{:?}", token);
        if token.token_type == token::TokenType::Error {
            break;
        }
    }
}

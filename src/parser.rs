// Function Symbol ParenLeft ParenRight BraceLeft Expression BraceRight
// Expression -> Symbol Some(< [_] > :: into_vec (box [$ ($ x), *]))ght

//use crate::lexer::Lexer;
//use crate::token::Keyword;
use crate::token::Token;

#[derive(Debug)]
pub struct TreeNode<'a> {
    name: &'a Token,
    children: Vec<TreeNode<'a>>,
}

pub fn parse(_tokens: &Vec<Token>) -> Option<TreeNode> {
    None
    //parse_symbol(tokens, 0)
}

//fn parse_expression(tokens: &Vec<Token>, pos: usize) -> Option<TreeNode> {
//    let (node, _next) = parse_function_call(tokens, pos);
//    node
//}
//
//fn parse_function_call(tokens: &Vec<Token>, pos: usize) -> (Option<TreeNode>, usize) {
//    let (symbol, next) = parse_symbol(tokens, pos);
//    (match symbol {
//        Some(name) => {
//            let (arguments, rest) = parse_arguments(tokens, next);
//            (Some(TreeNode {
//                name: name,
//                children: arguments,
//            }), rest)
//        },
//        _ => (None, next),
//    })
//}
//
//fn accept_token(tokens: &Vec<Token>, pos: usize, token: Token) -> Option<usize> {
//    return tokens.get(pos).and_then(|s| {
//        if *s == token {
//            Some(pos + 1)
//        } else {
//            None
//        }
//    });
//}
//
//fn parse_arguments(tokens: &Vec<Token>, pos: usize) -> (Vec<TreeNode>, usize) {
//    match accept_token(tokens, pos, Token::keyword(Keyword::ParenLeft)) {
//        Some(argument_start) => {
//            loop {
//                let next = accept_token(tokens, pos, Token::keyword(Keyword::ParenRight));
//                if (next.is_some())
//                    ;
//
//                let token = tokens.get(pos).and_then(|s| {
//                    if s.token_type == TokenType::Keyword && s.keyword == Some(Keyword::ParenRight) {
//                        Some(s)
//                    } else {
//                        None
//                    }
//                });
//                if token.is_some() {
//                    break;
//                }
//            }
//        },
//        None => (vec!(), pos)
//    }
////
////    let token = tokens.get(pos).and_then(|s| {
////        if s.token_type == TokenType::Keyword && s.keyword == Some(Keyword::ParenLeft) {
////            Some(s)
////        } else {
////            None
////        }
////    });
////
////    if token.is_none() {
////        return ;
////    }
//}

//fn parse_symbol(lexer: &mut Lexer) -> Option<TreeNode> {
//    accept_token(tokens, pos, Token::symbol())
//    let s = tokens.get(pos).and_then(|s| {
//        if s.token_type == TokenType::Symbol {
//            Some(s)
//        } else {
//            None
//        }
//    });
//
//    match s {
//        Some(s) => ( Some(s), pos + 1 ),
//        None => (None, pos)
//    }
//}

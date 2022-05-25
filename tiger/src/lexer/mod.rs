mod error;
mod lex;
mod token;

pub use error::Error;
pub use token::{Ident, Reserved, Separator, StringLiteral, Token, TokenKind};

// pub use lex::Lexer;

use std::io::Read;

use crate::position::Position;

type Result<T> = std::result::Result<T, Error>;
pub type Spanned<Tok, Loc> = Result<(Loc, Tok, Loc)>;

pub struct Lexer<R>
where
    R: Read,
{
    lexer: lex::LexerInner<R>,
}

impl<R> Lexer<R>
where
    R: Read,
{
    pub fn new(filename: impl Into<String>, r: R) -> Self {
        Self {
            lexer: lex::LexerInner::new(filename, r),
        }
    }
}

impl<R> Iterator for Lexer<R>
where
    R: Read,
{
    type Item = Spanned<TokenKind, Position>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.token() {
            Ok(Token {
                kind: TokenKind::Eof,
                ..
            }) => None,
            x => Some(x.map(|tok| tok.to_lalrpop())),
        }
    }
}

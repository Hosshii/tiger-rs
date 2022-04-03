mod error;
mod lex;
mod position;
mod token;

pub use error::Error;
pub use token::{Token, TokenKind};

// pub use lex::Lexer;

use std::io::Read;

use self::error::ErrorKind;

type Result<T> = std::result::Result<T, Error>;
pub type Spanned<Tok, Loc> = Result<(Loc, Tok, Loc)>;

pub struct Lexer<R>
where
    R: Read,
{
    lexer: lex::Lexer<R>,
}

impl<R> Lexer<R>
where
    R: Read,
{
    pub fn new(filename: impl Into<String>, r: R) -> Self {
        Self {
            lexer: lex::Lexer::new(filename, r),
        }
    }
}

impl<R> Iterator for Lexer<R>
where
    R: Read,
{
    type Item = Spanned<TokenKind, usize>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.token() {
            Err(Error {
                kind: ErrorKind::Eof,
                ..
            }) => None,
            x => Some(x.map(|tok| tok.to_lalrpop())),
        }
    }
}

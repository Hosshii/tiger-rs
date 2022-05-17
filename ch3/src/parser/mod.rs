pub mod ast;

use std::{fmt, io::Read};

use lalrpop_util::lalrpop_mod;
use thiserror::Error;

use crate::{
    lexer::{self, Error as LexError, TokenKind},
    position::Meta,
};

lalrpop_mod!(pub grammar,"/parser/grammar.rs");

type LalrpopError = lalrpop_util::ParseError<usize, TokenKind, LexError>;

#[derive(Debug, Error)]
pub enum ErrorKind {
    #[error("parse error: unexpected end of file")]
    Eof,
    #[error("lexer error: {0}")]
    Lexical(LexError),
    #[error("parse error: extra token {0:?}")]
    ExtraToken(TokenKind),
    #[error("parse error: unrecognized token {0:?}, expected {1}")]
    UnrecognizedToken(TokenKind, String),
}

#[derive(Debug, Error)]
pub struct Error {
    pub(crate) kind: ErrorKind,
    pub(crate) meta: Meta,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "file: {}, line: {}, column: {}",
            self.meta.filename(),
            self.meta.line(),
            self.meta.column()
        )?;
        write!(f, "{}", self.kind)
    }
}

pub fn parse<R>(filename: impl Into<String>, r: R) -> Result<ast::Program, LalrpopError>
where
    R: Read,
{
    let lexer = lexer::Lexer::new(filename, r);
    let parser = grammar::ProgramParser::new();

    parser.parse(lexer)
    // Ok(1)
}

use std::{fmt, io};

use thiserror::Error;

use crate::lexer::position::Meta;

#[derive(Debug, Error)]
pub struct Error {
    pub(crate) kind: ErrorKind,
    pub(crate) meta: Meta,
}

impl Error {
    pub fn new(kind: ErrorKind, meta: Meta) -> Self {
        Self { kind, meta }
    }
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

#[derive(Debug, Error)]
pub enum ErrorKind {
    #[error("reached end of file")]
    Eof,

    // expected, actual
    #[error("unexpected character. expected: `{}`, found: `{}`.",*.0 as char, *.1 as char)]
    UnexpectedCharacter(u8, u8),

    #[error("invalid escape.")]
    InvalidEscape,

    #[error(transparent)]
    Io(#[from] io::Error),

    #[error(transparent)]
    Utf8(#[from] std::str::Utf8Error),

    #[error("unknown error {0}")]
    Other(String),
}

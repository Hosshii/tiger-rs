pub mod ast;

use std::{fmt, io::Read};

use lalrpop_util::lalrpop_mod;
use thiserror::Error;

use crate::{
    common::Position,
    lexer::{self, Error as LexError, TokenKind},
};

lalrpop_mod!(#[allow(unused_imports, clippy::all)] pub grammar,"/parser/grammar.rs");

type LalrpopError = lalrpop_util::ParseError<Position, TokenKind, LexError>;

#[derive(Debug, Error)]
pub enum ErrorKind {
    #[error("parse error: unexpected end of file. expected: `{0:?}`")]
    Eof(Vec<String>),
    #[error("lexer error: {0}")]
    Lexical(LexError),
    #[error("parse error: extra token: `{0:?}`")]
    ExtraToken(TokenKind),
    #[error("parse error: unrecognized token: `{0:?}`, expected: `{1:?}`")]
    UnrecognizedToken(TokenKind, Vec<String>),
    #[error("invalid token")]
    InvalidToken,
}

#[derive(Debug, Error)]
pub struct Error {
    pub(crate) kind: ErrorKind,
    pub(crate) loc: Position,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "line: {}, column: {}",
            self.loc.line + 1,
            self.loc.column + 1
        )?;
        write!(f, "{}", self.kind)
    }
}

impl From<LalrpopError> for Error {
    fn from(e: LalrpopError) -> Self {
        match e {
            lalrpop_util::ParseError::ExtraToken {
                token: (loc, kind, _),
            } => Error {
                kind: ErrorKind::ExtraToken(kind),
                loc,
            },
            lalrpop_util::ParseError::InvalidToken { location } => Error {
                kind: ErrorKind::InvalidToken,
                loc: location,
            },
            lalrpop_util::ParseError::UnrecognizedEOF { location, expected } => Error {
                kind: ErrorKind::Eof(expected),
                loc: location,
            },
            lalrpop_util::ParseError::UnrecognizedToken {
                token: (loc, kind, _),
                expected,
            } => Error {
                kind: ErrorKind::UnrecognizedToken(kind, expected),
                loc,
            },
            lalrpop_util::ParseError::User { error } => {
                let loc = error.meta.cursor;
                Error {
                    kind: ErrorKind::Lexical(error),
                    loc,
                }
            }
        }
    }
}

pub fn parse<R>(filename: impl Into<String>, r: R) -> Result<ast::Program, Error>
where
    R: Read,
{
    let lexer = lexer::Lexer::new(filename, r);
    let parser = grammar::ProgramParser::new();

    parser.parse(lexer).map_err(Into::into)
}

macro_rules! test_file {
    ($name:ident, $path:expr) => {
        #[test]
        fn $name() {
            use std::fs::File;
            let file = File::open($path).unwrap();
            parse($path, file).unwrap();
        }
    };
}

test_file!(test_merge, "./testcases/merge.tig");
test_file!(test_queens, "./testcases/queens.tig");
test_file!(test_test1, "./testcases/test1.tig");
test_file!(test_test2, "./testcases/test2.tig");
test_file!(test_test3, "./testcases/test3.tig");
test_file!(test_test4, "./testcases/test4.tig");
test_file!(test_test5, "./testcases/test5.tig");
test_file!(test_test6, "./testcases/test6.tig");
test_file!(test_test7, "./testcases/test7.tig");
test_file!(test_test8, "./testcases/test8.tig");
test_file!(test_test9, "./testcases/test9.tig");
test_file!(test_test10, "./testcases/test10.tig");
test_file!(test_test11, "./testcases/test11.tig");
test_file!(test_test12, "./testcases/test12.tig");
test_file!(test_test13, "./testcases/test13.tig");
test_file!(test_test14, "./testcases/test14.tig");
test_file!(test_test15, "./testcases/test15.tig");
test_file!(test_test16, "./testcases/test16.tig");
test_file!(test_test17, "./testcases/test17.tig");
test_file!(test_test18, "./testcases/test18.tig");
test_file!(test_test19, "./testcases/test19.tig");
test_file!(test_test20, "./testcases/test20.tig");
test_file!(test_test21, "./testcases/test21.tig");
test_file!(test_test22, "./testcases/test22.tig");
test_file!(test_test23, "./testcases/test23.tig");
test_file!(test_test24, "./testcases/test24.tig");
test_file!(test_test25, "./testcases/test25.tig");
test_file!(test_test26, "./testcases/test26.tig");
test_file!(test_test27, "./testcases/test27.tig");
test_file!(test_test28, "./testcases/test28.tig");
test_file!(test_test29, "./testcases/test29.tig");
test_file!(test_test30, "./testcases/test30.tig");
test_file!(test_test31, "./testcases/test31.tig");
test_file!(test_test32, "./testcases/test32.tig");
test_file!(test_test33, "./testcases/test33.tig");
test_file!(test_test34, "./testcases/test34.tig");
test_file!(test_test35, "./testcases/test35.tig");
test_file!(test_test36, "./testcases/test36.tig");
test_file!(test_test37, "./testcases/test37.tig");
test_file!(test_test38, "./testcases/test38.tig");
test_file!(test_test39, "./testcases/test39.tig");
test_file!(test_test40, "./testcases/test40.tig");
test_file!(test_test41, "./testcases/test41.tig");
test_file!(test_test42, "./testcases/test42.tig");
test_file!(test_test43, "./testcases/test43.tig");
test_file!(test_test44, "./testcases/test44.tig");
test_file!(test_test45, "./testcases/test45.tig");
test_file!(test_test46, "./testcases/test46.tig");
test_file!(test_test47, "./testcases/test47.tig");
test_file!(test_test48, "./testcases/test48.tig");
// cannot parse test49
// test_file!(test_test49, "./testcases/test49.tig");

mod error;
mod position;
mod token;

use std::{
    io::{Bytes, Read},
    iter::Peekable,
    rc::Rc,
    str::{self, FromStr},
};

use crate::lexer::{
    error::{Error, ErrorKind},
    position::{Cursor, Meta},
    token::{Ident, Reserved, Separator, StringLiteral, Token, TokenKind},
};

type Result<T> = std::result::Result<T, Error>;

pub type Spanned<Tok, Loc> = Result<(Loc, Tok, Loc)>;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct State {
    cursor: Cursor,
}

impl State {
    fn next(&mut self) {
        self.cursor.next();
    }

    fn newline(&mut self) {
        self.cursor.newline();
    }
}

pub struct Lexer<R: Read> {
    ipt: Peekable<Bytes<R>>,
    filename: Rc<String>,
    before_state: State,
    state: State,
}

use ErrorKind::*;
impl<R> Lexer<R>
where
    R: Read,
{
    pub fn new(filename: impl Into<String>, ipt: R) -> Self {
        Self {
            ipt: ipt.bytes().peekable(),
            filename: Rc::new(filename.into()),
            before_state: State::default(),
            state: State::default(),
        }
    }

    fn peek(&mut self) -> Result<u8> {
        if let Some(Ok(v)) = self.ipt.peek() {
            return Ok(*v);
        }

        match self.ipt.next() {
            Some(Ok(_)) => unreachable!(),
            Some(Err(e)) => Err(self.make_error(e.into())),
            None => Err(self.make_error(ErrorKind::Eof)),
        }
    }

    fn comment_or_slash(&mut self) -> Result<Option<Token>> {
        if self.peek()? != b'*' {
            Ok(Some(
                self.make_token(TokenKind::Separator(Separator::Slash)),
            ))
        } else {
            self.consume(b'*')?;
            loop {
                self.take_while(|c| c != &b'*' && c != &b'/')?;

                match self.next()? {
                    b'*' => {
                        // end comment
                        if self.next()? == b'/' {
                            break Ok(None);
                        }
                    }
                    b'/' => {
                        // nested comment
                        if self.peek()? == b'*' {
                            self.comment_or_slash()?;
                        }
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    fn consume(&mut self, c: u8) -> Result<()> {
        let _c = self.next()?;
        if _c == c {
            Ok(())
        } else {
            Err(self.make_error(UnexpectedCharacter(c, _c)))
        }
    }

    fn escape_char(&mut self) -> Result<char> {
        let escaped_char = match self.peek()? {
            b'n' => '\n',
            b't' => '\t',
            b'r' => '\r',
            b'\\' => '\\',
            b'"' => '"',
            ch if ch.is_ascii_digit() => self.escape_ascii()? as char,
            _ => return Err(self.make_error(InvalidEscape)),
        };
        Ok(escaped_char)
    }

    fn escape_ascii(&mut self) -> Result<u8> {
        let digit = self.take_while(u8::is_ascii_digit)?;
        if digit.len() == 3 {
            let num: u16 = digit[0] as u16 * 100 + digit[1] as u16 * 10 + digit[2] as u16;
            if num <= u8::MAX as u16 {
                Ok(num as u8)
            } else {
                Err(self.make_error(InvalidEscape))
            }
        } else {
            Err(self.make_error(InvalidEscape))
        }
    }

    fn make_error(&self, kind: ErrorKind) -> Error {
        let meta = Meta::new(self.filename.clone(), self.before_state.cursor, 0);
        Error::new(kind, meta)
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        let len = kind.len();
        Token::new(
            kind,
            Meta::new(Rc::clone(&self.filename), self.before_state.cursor, len),
        )
    }

    fn next(&mut self) -> Result<u8> {
        match self.ipt.next() {
            Some(Ok(c)) => {
                if c == b'\n' {
                    self.state.newline();
                } else {
                    self.state.next();
                }
                Ok(c)
            }
            Some(Err(e)) => {
                self.state.next();
                Err(self.make_error(e.into()))
            }
            None => Err(self.make_error(Eof)),
        }
    }

    fn number(&mut self) -> Result<u64> {
        let a = self.take_while(u8::is_ascii_digit)?;

        let result = a
            .into_iter()
            .rev()
            // this is safe because only contain is_ascii_digit == true
            .map(|c| (c as char).to_digit(10).unwrap())
            .enumerate()
            .fold(0, |acc, (idx, x)| acc + 10u64.pow(idx as u32) * x as u64);

        Ok(result)
    }

    // TODO: 複数回呼び出されたときのことを考える
    fn save_state(&mut self) {
        self.before_state = self.state.clone();
    }

    fn separator(&mut self) -> Result<Separator> {
        let first = self.next()? as char;
        let second = self.peek()? as char;
        let mut s = first.to_string();
        s.push(second);
        let s = s;

        if let Ok(v) = Separator::from_str(s.as_str()) {
            self.next()?;
            Ok(v)
        } else {
            Separator::from_str(&s[..1]).map_err(|e| self.make_error(ErrorKind::Other(e)))
        }
    }

    fn string_literal(&mut self) -> Result<StringLiteral> {
        self.consume(b'"')?;
        let mut st = String::new();
        while self.peek()? != b'"' {
            let cur = self.next()?;
            if cur == b'\\' {
                if self.peek()?.is_ascii_whitespace() {
                    self.take_while(u8::is_ascii_whitespace)?;
                    self.consume(b'\\')?;
                } else {
                    st.push(self.escape_char()?);
                }
            } else {
                st.push(cur as char);
            }
        }
        self.consume(b'"')?;
        Ok(StringLiteral::new(st))
    }

    fn take_while<P>(&mut self, mut predicate: P) -> Result<Vec<u8>>
    where
        P: FnMut(&u8) -> bool,
    {
        let mut buf = Vec::new();
        loop {
            let cur = self.peek()?;
            if predicate(&cur) {
                buf.push(cur);
                self.next()?;
            } else {
                break Ok(buf);
            }
        }
    }

    fn token(&mut self) -> Result<Token> {
        self.trim_whitespace()?;

        match self.peek() {
            Ok(ch) => match ch {
                b'0'..=b'9' => {
                    let n = self.number()?;
                    Ok(self.make_token(TokenKind::Int(n)))
                }
                b'"' => {
                    let s = self.string_literal()?;
                    Ok(self.make_token(TokenKind::Str(s)))
                }
                b'/' => {
                    self.consume(b'/')?;
                    match self.comment_or_slash()? {
                        Some(tok) => Ok(tok),
                        None => self.token(),
                    }
                }
                c if Separator::starts_with(c as char) => {
                    let separator = self.separator()?;
                    Ok(self.make_token(TokenKind::Separator(separator)))
                }
                _ => {
                    let ident = self.take_while(|c| c.is_ascii_alphanumeric() || c == &b'_')?;
                    let ident = str::from_utf8(&ident).map_err(|e| self.make_error(e.into()))?;
                    if let Ok(reserved) = Reserved::from_str(ident) {
                        Ok(self.make_token(TokenKind::Reserved(reserved)))
                    } else {
                        Ok(self.make_token(TokenKind::Ident(Ident(ident.to_string()))))
                    }
                }
            },

            Err(Error {
                kind: ErrorKind::Eof,
                ..
            }) => Ok(self.make_token(TokenKind::Eof)),

            Err(e) => Err(e),
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>> {
        let mut result = Vec::new();

        while self.ipt.peek().is_some() {
            self.save_state();
            let token = match self.token() {
                Err(Error {
                    kind: ErrorKind::Eof,
                    ..
                }) => {
                    result.push(self.make_token(TokenKind::Eof));
                    return Ok(result);
                }
                Err(e) => return Err(e),
                Ok(tok) => tok,
            };

            result.push(token);
        }

        Ok(result)
    }

    fn trim_whitespace(&mut self) -> Result<()> {
        self.take_while(u8::is_ascii_whitespace)?;
        Ok(())
    }
}

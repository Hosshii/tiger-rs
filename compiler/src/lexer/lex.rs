use std::{
    io::{Bytes, Read},
    iter::Peekable,
    str::{self, FromStr},
    sync::Arc,
};

use crate::{
    common::{Meta, Position},
    lexer::{
        error::{Error, ErrorKind},
        token::{Ident, Reserved, Separator, StringLiteral, Token, TokenKind},
        Result,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct State {
    cursor: Position,
}

impl State {
    fn next(&mut self) {
        self.cursor.next();
    }

    fn newline(&mut self) {
        self.cursor.newline();
    }
}

pub(crate) struct LexerInner<R: Read> {
    ipt: Peekable<Bytes<R>>,
    filename: Arc<String>,
    state: State,
}

use ErrorKind::*;

impl<R> LexerInner<R>
where
    R: Read,
{
    pub fn new(filename: impl Into<String>, ipt: R) -> Self {
        Self {
            ipt: ipt.bytes().peekable(),
            filename: Arc::new(filename.into()),
            state: State::default(),
        }
    }

    fn peek_byte(&mut self) -> Option<Result<u8>> {
        if let Some(Ok(v)) = self.ipt.peek() {
            return Some(Ok(*v));
        }

        match self.ipt.next() {
            Some(Ok(_)) => unreachable!(),
            Some(Err(e)) => Some(Err(self.make_error(e.into()))),
            None => None,
        }
    }

    /// if reaches EOF this function returns EOF error instead of None
    fn must_peek_byte(&mut self) -> Result<u8> {
        match self.peek_byte() {
            Some(r) => r,
            None => Err(self.make_error(ErrorKind::Eof)),
        }
    }

    fn comment_or_slash(&mut self) -> Result<Option<Token>> {
        match self.peek_byte() {
            Some(Ok(b'*')) => {
                self.consume(b'*')?;
                loop {
                    self.take_while(|c| c != &b'*' && c != &b'/')?;

                    match self.must_next_byte()? {
                        b'*' => {
                            // end comment
                            if self.must_next_byte()? == b'/' {
                                break Ok(None);
                            }
                        }
                        b'/' => {
                            // nested comment
                            if self.must_peek_byte()? == b'*' {
                                self.comment_or_slash()?;
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            }
            Some(_) | None => Ok(Some(
                self.make_token(TokenKind::Separator(Separator::Slash)),
            )),
        }
    }

    fn consume(&mut self, c: u8) -> Result<()> {
        let _c = self.must_next_byte()?;
        if _c == c {
            Ok(())
        } else {
            Err(self.make_error(UnexpectedCharacter(c, _c)))
        }
    }

    fn escape_char(&mut self) -> Result<char> {
        let escaped_char = match self.must_peek_byte()? {
            x @ (b'n' | b't' | b'r' | b'\\' | b'"') => {
                self.must_next_byte()?;
                match x {
                    b'n' => '\n',
                    b't' => '\t',
                    b'r' => '\r',
                    b'\\' => '\\',
                    b'"' => '"',
                    _ => unreachable!(),
                }
            }
            ch if ch.is_ascii_digit() => self.escape_ascii()? as char,
            _ => return Err(self.make_error(InvalidEscape)),
        };
        Ok(escaped_char)
    }

    fn escape_ascii(&mut self) -> Result<u8> {
        let mut digit = vec![
            self.must_next_byte()?,
            self.must_next_byte()?,
            self.must_next_byte()?,
        ];

        if !digit.iter().all(u8::is_ascii_digit) {
            return Err(self.make_error(InvalidEscape));
        }

        digit.iter_mut().for_each(|v| *v -= b'0');

        let num: u16 = digit[0] as u16 * 100 + digit[1] as u16 * 10 + digit[2] as u16;
        if num <= u8::MAX as u16 {
            Ok(num as u8)
        } else {
            Err(self.make_error(InvalidEscape))
        }
    }

    // use before_state ?
    fn make_error(&self, kind: ErrorKind) -> Error {
        let meta = Meta::new(self.filename.clone(), self.state.cursor, 0);
        Error::new(kind, meta)
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        let len = kind.len();
        Token::new(
            kind,
            Meta::new(Arc::clone(&self.filename), self.state.cursor, len),
        )
    }

    fn next_byte(&mut self) -> Option<Result<u8>> {
        match self.ipt.next() {
            Some(Ok(c)) => {
                if c == b'\n' {
                    self.state.newline();
                } else {
                    self.state.next();
                }
                Some(Ok(c))
            }
            Some(Err(e)) => {
                self.state.next();
                Some(Err(self.make_error(e.into())))
            }
            None => None,
        }
    }

    fn must_next_byte(&mut self) -> Result<u8> {
        match self.next_byte() {
            Some(r) => r,
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

    fn separator(&mut self) -> Result<Separator> {
        let first = self.must_next_byte()? as char;
        let mut s = first.to_string();
        match self.peek_byte() {
            None => {
                Separator::from_str(s.as_str()).map_err(|e| self.make_error(ErrorKind::Other(e)))
            }
            Some(x) => {
                let second = x? as char;
                s.push(second);
                let s = s;

                if let Ok(v) = Separator::from_str(s.as_str()) {
                    self.must_next_byte()?;
                    Ok(v)
                } else {
                    Separator::from_str(&s[..1]).map_err(|e| self.make_error(ErrorKind::Other(e)))
                }
            }
        }
    }

    fn string_literal(&mut self) -> Result<StringLiteral> {
        self.consume(b'"')?;
        let mut st = String::new();
        while self.must_peek_byte()? != b'"' {
            let cur = self.must_next_byte()?;
            if cur == b'\\' {
                if self.must_peek_byte()?.is_ascii_whitespace() {
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
        while let Some(r) = self.peek_byte() {
            let cur = r?;
            if predicate(&cur) {
                buf.push(cur);
                self.must_next_byte()?;
            } else {
                break;
            }
        }
        Ok(buf)
    }

    pub(crate) fn token(&mut self) -> Result<Token> {
        self.trim_whitespace()?;

        match self.peek_byte() {
            Some(Ok(ch)) => match ch {
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
            Some(Err(e)) => Err(e),
            None => Ok(self.make_token(TokenKind::Eof)),
        }
    }

    fn trim_whitespace(&mut self) -> Result<()> {
        self.take_while(u8::is_ascii_whitespace)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use strum::IntoEnumIterator;

    use super::*;
    #[test]
    fn test_tokenize() {
        let test_case: Vec<(&str, Box<dyn Iterator<Item = TokenKind>>)> = vec![
            (
                "type var function break of end in nil let do to for while else then if array",
                Box::new(Reserved::iter().map(TokenKind::Reserved)),
            ),
            (
                ":=|&>=><=<<>=/ *-+.{}[]();:,",
                Box::new(Separator::iter().map(TokenKind::Separator)),
            ),
            (
                r##"
                "hello world"
                "hello\n\t\r\"\\ \          \ \000 \111"
                "##,
                Box::new(
                    vec!["hello world", "hello\n\t\r\"\\  \u{0} o"]
                        .into_iter()
                        .map(|v| TokenKind::Str(StringLiteral(v.to_string()))),
                ),
            ),
            (
                "typeident",
                Box::new(
                    vec!["typeident"]
                        .into_iter()
                        .map(|v| TokenKind::Ident(Ident(v.to_string()))),
                ),
            ),
        ];

        for (ipt, expected) in test_case {
            let actual = Lexer::new("", ipt.as_bytes())
                .map(|v| v.unwrap().1)
                .inspect(|v| println!("{:?}", v));

            assert!(expected.eq(actual));
        }
    }
}

use std::{str::FromStr, string::ToString};

use strum::{EnumIter, IntoEnumIterator};

use crate::position::{Cursor, Meta};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Reserved(Reserved),
    Separator(Separator),
    Str(StringLiteral),
    Int(u64),
    Ident(Ident),
    Eof,
}

impl TokenKind {
    pub fn len(&self) -> usize {
        use TokenKind::*;

        match self {
            Reserved(r) => r.len(),
            Separator(s) => s.len(),
            Str(s) => s.len(),
            Int(num) => num.to_string().len(),
            Ident(ident) => ident.len(),
            Eof => 0,
        }
    }
}

impl ToString for TokenKind {
    fn to_string(&self) -> String {
        use self::TokenKind::*;
        match self {
            Reserved(r) => r.to_string(),
            Separator(s) => s.to_string(),
            Str(s) => format!("\"{}\"", format_escape(s.0.as_str())),
            Int(n) => n.to_string(),
            Ident(s) => s.to_string(),
            Eof => "eof".to_string(),
        }
    }
}

fn format_escape(s: &str) -> String {
    s.chars()
        .map(|v| match v {
            '\n' => "\\n".to_string(),
            '\t' => "\\t".to_string(),
            '\r' => "\\r".to_string(),
            '\\' => "\\\\".to_string(),
            '\"' => "\\\"".to_string(),
            s => s.to_string(),
        })
        .collect()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    meta: Meta,
}

impl Token {
    pub fn new(kind: TokenKind, meta: Meta) -> Self {
        Self { kind, meta }
    }

    pub fn meta(&self) -> &Meta {
        &self.meta
    }

    pub fn to_lalrpop(self) -> (Cursor, TokenKind, Cursor) {
        let start = self.meta.cursor;
        let len = self.meta.length;
        let end = Cursor {
            line: start.line,
            column: start.column + len as u32,
            from_start: start.from_start + len,
        };
        (start, self.kind, end)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringLiteral(String);

impl StringLiteral {
    pub fn len(&self) -> usize {
        self.0.chars().count()
    }

    pub fn new(v: impl Into<String>) -> Self {
        Self(v.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, EnumIter)]
pub enum Reserved {
    Type,
    Var,
    Function,
    Break,
    Of,
    End,
    In,
    Nil,
    Let,
    Do,
    To,
    For,
    While,
    Else,
    Then,
    If,
    Array,
}

impl Reserved {
    pub fn as_str(&self) -> &'static str {
        use self::Reserved::*;
        match self {
            Type => "type",
            Var => "var",
            Function => "function",
            Break => "break",
            Of => "of",
            End => "end",
            In => "in",
            Nil => "nil",
            Let => "let",
            Do => "do",
            To => "to",
            For => "for",
            While => "while",
            Else => "else",
            Then => "then",
            If => "if",
            Array => "array",
        }
    }

    pub fn len(&self) -> usize {
        self.as_str().chars().count()
    }

    pub fn starts_with(c: char) -> bool {
        Self::iter().any(|e| e.as_str().starts_with(c))
    }
}

impl ToString for Reserved {
    fn to_string(&self) -> String {
        self.as_str().to_string()
    }
}

impl FromStr for Reserved {
    type Err = String;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        use self::Reserved::*;
        match s {
            "type" => Ok(Type),
            "var" => Ok(Var),
            "function" => Ok(Function),
            "break" => Ok(Break),
            "of" => Ok(Of),
            "end" => Ok(End),
            "in" => Ok(In),
            "nil" => Ok(Nil),
            "let" => Ok(Let),
            "do" => Ok(Do),
            "to" => Ok(To),
            "for" => Ok(For),
            "while" => Ok(While),
            "else" => Ok(Else),
            "then" => Ok(Then),
            "if" => Ok(If),
            "array" => Ok(Array),
            s => Err(format!("cannot convert {} to Reserved", s)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, EnumIter)]
pub enum Separator {
    ColonEqual,
    Pipe,      // Or
    Ampersand, // And
    GreaterOrEqual,
    Greater,
    LesserOrEqual,
    Lesser,
    NotEqual,
    Equal,
    Slash,  // Div
    Star,   // Times
    Hyphen, // Minus
    Plus,
    Dot,
    LBrace,
    RBrace,
    LBrack,
    RBrack,
    LParen,
    RParen,
    Semicolon,
    Colon,
    Comma,
}

impl Separator {
    pub fn as_str(&self) -> &'static str {
        use self::Separator::*;
        match self {
            ColonEqual => ":=",
            Pipe => "|",
            Ampersand => "&",
            GreaterOrEqual => ">=",
            Greater => ">",
            LesserOrEqual => "<=",
            Lesser => "<",
            NotEqual => "<>",
            Equal => "=",
            Slash => "/",
            Star => "*",
            Hyphen => "-",
            Plus => "+",
            Dot => ".",
            LBrace => "{",
            RBrace => "}",
            LBrack => "[",
            RBrack => "]",
            LParen => "(",
            RParen => ")",
            Semicolon => ";",
            Colon => ":",
            Comma => ",",
        }
    }

    pub fn len(&self) -> usize {
        self.as_str().chars().count()
    }

    pub fn starts_with(c: char) -> bool {
        Self::iter().any(|e| e.as_str().starts_with(c))
    }
}

impl ToString for Separator {
    fn to_string(&self) -> String {
        self.as_str().to_string()
    }
}

impl FromStr for Separator {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use self::Separator::*;
        match s {
            ":=" => Ok(ColonEqual),
            "|" => Ok(Pipe),
            "&" => Ok(Ampersand),
            ">=" => Ok(GreaterOrEqual),
            ">" => Ok(Greater),
            "<=" => Ok(LesserOrEqual),
            "<" => Ok(Lesser),
            "<>" => Ok(NotEqual),
            "=" => Ok(Equal),
            "/" => Ok(Slash),
            "*" => Ok(Star),
            "-" => Ok(Hyphen),
            "+" => Ok(Plus),
            "." => Ok(Dot),
            "{" => Ok(LBrace),
            "}" => Ok(RBrace),
            "[" => Ok(LBrack),
            "]" => Ok(RBrack),
            "(" => Ok(LParen),
            ")" => Ok(RParen),
            ";" => Ok(Semicolon),
            ":" => Ok(Colon),
            "," => Ok(Comma),
            s => Err(format!("cannot convert {} to Separator", s)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(pub String);

impl Ident {
    pub fn len(&self) -> usize {
        self.0.chars().count()
    }
}

impl ToString for Ident {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

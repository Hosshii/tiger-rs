use std::sync::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Position {
    pub(crate) line: u32,
    pub(crate) column: u32,
    pub(crate) from_start: usize,
}

impl Position {
    pub fn next(&mut self) {
        self.column += 1;
        self.from_start += 1;
    }

    pub fn newline(&mut self) {
        self.column = 0;
        self.line += 1;
        self.from_start += 1;
    }
}

pub type Positions = (Position, Position);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Meta {
    pub(crate) filename: Arc<String>,
    pub(crate) cursor: Position,
    pub(crate) length: usize,
}

impl Meta {
    pub fn new(filename: Arc<String>, cursor: Position, length: usize) -> Self {
        Self {
            filename,
            cursor,
            length,
        }
    }

    pub fn filename(&self) -> &str {
        &self.filename
    }

    pub fn line(&self) -> u32 {
        self.cursor.line
    }

    pub fn column(&self) -> u32 {
        self.cursor.column
    }
}

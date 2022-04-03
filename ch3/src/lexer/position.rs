use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Cursor {
    pub(crate) line: u32,
    pub(crate) column: u32,
    pub(crate) from_start: u32,
}

impl Cursor {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Meta {
    filename: Rc<String>,
    cursor: Cursor,
    length: usize,
}

impl Meta {
    pub fn new(filename: Rc<String>, cursor: Cursor, length: usize) -> Self {
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

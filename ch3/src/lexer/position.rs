use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Cursor {
    pub(crate) line: u32,
    pub(crate) column: u32,
}

impl Cursor {
    pub fn next(&mut self) {
        self.column += 1
    }

    pub fn newline(&mut self) {
        self.column = 0;
        self.line += 1;
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Meta {
    filename: Rc<String>,
    cursor: Cursor,
}

impl Meta {
    pub fn new(filename: Rc<String>, cursor: Cursor) -> Self {
        Self { filename, cursor }
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

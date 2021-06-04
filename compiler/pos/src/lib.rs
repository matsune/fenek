mod src_file;

use serde::ser::{Serialize, Serializer};
pub use src_file::SrcFile;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Pos {
    pub col: u32,
    pub row: u32,
}

impl Default for Pos {
    fn default() -> Self {
        Pos { col: 1, row: 1 }
    }
}

impl std::fmt::Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.col, self.row)
    }
}

impl Serialize for Pos {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl Pos {
    pub fn newline(&mut self) {
        self.col += 1;
        self.row = 1;
    }

    pub fn add_row(&mut self, row: u32) {
        self.row += row;
    }
}

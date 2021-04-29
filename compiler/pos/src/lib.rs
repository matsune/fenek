use std::convert::TryInto;
use std::io::prelude::Read;
use std::path::PathBuf;
use std::str::Chars;

pub type Offset = u32;

pub struct SrcFile {
    pub path: PathBuf,
    inner: String,
}

impl SrcFile {
    pub fn open<P: Into<PathBuf>>(path: P) -> std::io::Result<SrcFile> {
        let path = path.into();
        let mut src_file = std::fs::File::open(&path)?;
        let mut buf = String::new();
        src_file.read_to_string(&mut buf)?;
        Ok(SrcFile { path, inner: buf })
    }

    pub fn pos_from_offset(&self, offset: Offset) -> Pos {
        let mut pos = Pos::default();
        for (idx, c) in self.inner.chars().enumerate() {
            if idx == offset as usize {
                break;
            }
            if c == '\n' || c == '\r' {
                pos.newline();
            } else {
                pos.add_row(c.len_utf16().try_into().unwrap());
            }
        }
        pos
    }

    pub fn chars(&self) -> Chars<'_> {
        self.inner.chars()
    }
}

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

impl Pos {
    pub fn newline(&mut self) {
        self.col += 1;
        self.row = 1;
    }

    pub fn add_row(&mut self, row: u32) {
        self.row += row;
    }
}

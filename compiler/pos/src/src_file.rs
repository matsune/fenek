use std::io::prelude::Read;
use std::path::PathBuf;
use std::str::Chars;

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

    pub fn chars(&self) -> Chars<'_> {
        self.inner.chars()
    }
}

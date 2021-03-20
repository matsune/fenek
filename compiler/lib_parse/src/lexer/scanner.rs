use std::str::Chars;

#[cfg(test)]
mod tests;

pub const EOF: char = '\0';

#[derive(Debug)]
pub struct Scanner<'a> {
    source: &'a mut Chars<'a>,
    peek: char,
    utf8_offset: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a mut Chars<'a>) -> Self {
        let peek = source.next().unwrap_or(EOF);
        Scanner {
            source,
            peek,
            utf8_offset: 0,
        }
    }

    pub fn peek(&mut self) -> char {
        self.peek
    }

    pub fn bump(&mut self) -> char {
        let p = self.peek;
        self.utf8_offset += p.len_utf8();
        self.peek = self.source.next().unwrap_or(EOF);
        p
    }

    pub fn is_eof(&self) -> bool {
        self.peek == EOF
    }

    pub fn utf8_offset(&self) -> usize {
        self.utf8_offset
    }
}

use std::str::Chars;

#[cfg(test)]
mod tests;

pub const EOF: char = '\0';

#[derive(Debug)]
pub struct Scanner<'a> {
    source: &'a mut Chars<'a>,
    peek: char,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a mut Chars<'a>) -> Self {
        let peek = source.next().unwrap_or(EOF);
        Scanner { source, peek }
    }

    pub fn peek(&mut self) -> char {
        self.peek
    }

    pub fn bump(&mut self) -> char {
        let p = self.peek;
        self.peek = self.source.next().unwrap_or(EOF);
        p
    }
}

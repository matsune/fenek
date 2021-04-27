use std::str::Chars;

#[cfg(test)]
mod tests;

pub const EOF: char = '\0';

#[derive(Debug)]
pub struct Scanner<'src> {
    source: Chars<'src>,
    peek: char,
}

impl<'src> Scanner<'src> {
    pub fn new(mut source: Chars<'src>) -> Self {
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

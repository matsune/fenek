mod scanner_tests {
    use crate::scanner::{Scanner, EOF};

    #[test]
    fn test_scanner_eof() {
        let mut source = "".chars();
        let s = Scanner::new(&mut source);
        assert_eq!(s.is_eof(), true);
    }

    #[test]
    fn test_scanner_utf8() {
        let mut source = "a©あ😀".chars();
        let mut s = Scanner::new(&mut source);
        assert_eq!(s.is_eof(), false);
        assert_eq!(s.bump(), 'a');
        assert_eq!(s.utf8_offset(), 1);
        assert_eq!(s.bump(), '©');
        assert_eq!(s.utf8_offset(), 3);
        assert_eq!(s.bump(), 'あ');
        assert_eq!(s.utf8_offset(), 6);
        assert_eq!(s.bump(), '😀');
        assert_eq!(s.utf8_offset(), 10);
        assert_eq!(s.is_eof(), true);
    }
}

mod lexer_tests {
    use crate::lexer::{Lexer, LiteralError};
    use crate::token::{Literal, Token, TokenKind};

    #[test]
    fn test_lexer() {
        macro_rules! test_token {
            ($s:expr, $kind:expr) => {
                let tok = Lexer::new(&mut $s.chars()).scan().unwrap();
                assert_eq!(tok, Token::new($kind, $s.into()));
            };
        }
        macro_rules! test_token_literal {
            ($s:expr, $kind:expr, $lit:expr) => {
                let tok = Lexer::new(&mut $s.chars()).scan().unwrap();
                assert_eq!(tok, Token::new($kind, $lit.into()));
            };
        }

        test_token!(" \t", TokenKind::Spaces);
        test_token!("\n\n\n", TokenKind::Newlines);
        test_token!(r"// line comment", TokenKind::LineComment);
        test_token!("12_3_", TokenKind::Literal(Literal::Int));
        test_token!("0b0__101", TokenKind::Literal(Literal::Int));
        test_token!("0o0_17", TokenKind::Literal(Literal::Int));
        test_token!("0xa0f", TokenKind::Literal(Literal::Int));
        test_token!("20.23", TokenKind::Literal(Literal::Float));
        test_token!("0.2__3", TokenKind::Literal(Literal::Float));
        test_token_literal!(
            r#""terminated string literal""#,
            TokenKind::Literal(Literal::String),
            "\"terminated string literal\""
        );
        test_token_literal!(
            r#""string \"escaped\" literal""#,
            TokenKind::Literal(Literal::String),
            "\"string \"escaped\" literal\""
        );
        test_token_literal!(
            r#""string with newline\nliteral""#,
            TokenKind::Literal(Literal::String),
            "\"string with newline\nliteral\""
        );
        // test_token_literal!("'a'", TokenKind::Literal(Literal::Char), "'a'");
        // test_token_literal!("'\\n'", TokenKind::Literal(Literal::Char), "'\n'");
        test_token!("true", TokenKind::Literal(Literal::Bool(true)));
        test_token!("false", TokenKind::Literal(Literal::Bool(false)));
        test_token!("ident", TokenKind::Ident);
        test_token!("/", TokenKind::Slash);
        test_token!(";", TokenKind::Semi);
        test_token!(",", TokenKind::Comma);
        test_token!("(", TokenKind::OpenParen);
        test_token!(")", TokenKind::CloseParen);
        test_token!("{", TokenKind::OpenBrace);
        test_token!("}", TokenKind::CloseBrace);
        test_token!(":", TokenKind::Colon);
        test_token!("=", TokenKind::Eq);
        test_token!("!", TokenKind::Not);
        test_token!("<", TokenKind::Lt);
        test_token!(">", TokenKind::Gt);
        test_token!("-", TokenKind::Minus);
        test_token!("&", TokenKind::And);
        test_token!("|", TokenKind::Or);
        test_token!("+", TokenKind::Plus);
        test_token!("*", TokenKind::Star);
        test_token!("^", TokenKind::Caret);
        test_token!("%", TokenKind::Percent);
    }

    #[test]
    fn test_lexer_error() {
        use LiteralError::*;

        macro_rules! test_literal_error {
            ($s:expr, $error:expr) => {
                let err = Lexer::new(&mut $s.chars()).scan().unwrap_err();
                assert_eq!(err.to_string(), $error.to_string());
            };
        }

        test_literal_error!(
            r#""unterminated string"#,
            "unterminated string literal: `\"unterminated string`"
        );
        // test_literal_error!(
        //     r#"'unterminated char"#,
        //     "unterminated char literal: `'unterminated char`"
        // );
        // test_literal_error!(r#""\m"#, "unknown character escape: `\\m`");
        test_literal_error!("0b", InvalidBinaryLiteral);
        test_literal_error!("0b012", InvalidBinaryLiteral);
        test_literal_error!("0o", InvalidOctalLiteral);
        test_literal_error!("0o0129", InvalidOctalLiteral);
        test_literal_error!("0x", InvalidHexLiteral);
        test_literal_error!("0xz", InvalidHexLiteral);
        test_literal_error!("0.", InvalidFloatLiteral);
    }
}

use super::*;

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
    test_token!("12_3_", TokenKind::Lit(LitKind::Int));
    test_token!("0b0__101", TokenKind::Lit(LitKind::Int));
    test_token!("0o0_17", TokenKind::Lit(LitKind::Int));
    test_token!("0xa0f", TokenKind::Lit(LitKind::Int));
    test_token!("20.23", TokenKind::Lit(LitKind::Float));
    test_token!("0.2__3", TokenKind::Lit(LitKind::Float));
    test_token_literal!(
        r#""terminated string literal""#,
        TokenKind::Lit(LitKind::String),
        "\"terminated string literal\""
    );
    test_token_literal!(
        r#""string \"escaped\" literal""#,
        TokenKind::Lit(LitKind::String),
        "\"string \"escaped\" literal\""
    );
    test_token_literal!(
        r#""string with newline\nliteral""#,
        TokenKind::Lit(LitKind::String),
        "\"string with newline\nliteral\""
    );
    // test_token_literal!("'a'", TokenKind::Lit(LitKind::Char), "'a'");
    // test_token_literal!("'\\n'", TokenKind::Lit(LitKind::Char), "'\n'");
    test_token!("true", TokenKind::Lit(LitKind::Bool(true)));
    test_token!("false", TokenKind::Lit(LitKind::Bool(false)));
    test_token!("ident", TokenKind::Ident);
    test_token!("/", TokenKind::Slash);
    test_token!(";", TokenKind::Semi);
    test_token!(",", TokenKind::Comma);
    test_token!("(", TokenKind::LParen);
    test_token!(")", TokenKind::RParen);
    test_token!("{", TokenKind::LBrace);
    test_token!("}", TokenKind::RBrace);
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
    use LitError::*;

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
    test_literal_error!("0b", InvalidBinaryLit);
    test_literal_error!("0b012", InvalidBinaryLit);
    test_literal_error!("0o", InvalidOctalLit);
    test_literal_error!("0o0129", InvalidOctalLit);
    test_literal_error!("0x", InvalidHexLit);
    test_literal_error!("0xz", InvalidHexLit);
    test_literal_error!("0.", InvalidFloatLit);
}

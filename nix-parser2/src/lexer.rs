//! Utility functions which wrap and extend `nix-lexer`.

use nix_errors::{Errors, Partial};
use nix_lexer::{LiteralKind, Token, TokenKind};

use crate::error::Error;

/// Converts an input string into a sequence of tokens and an optional stack of errors.
pub fn tokenize(input: &str) -> Partial<Vec<Token>, Error> {
    let mut errors = Errors::new();

    #[rustfmt::skip]
    let tokens = nix_lexer::tokenize(input)
        .inspect(|token| match token.kind {
            TokenKind::BlockComment { terminated: false } => {
                errors.push(Error::Unterminated(token.span, token.kind))
            }
            TokenKind::Literal { kind: LiteralKind::Path { trailing_slash: true } } => {
                errors.push(Error::TrailingSlash(token.span, token.kind))
            }
            TokenKind::Literal { kind: LiteralKind::PathTemplate { trailing_slash: true } } => {
                errors.push(Error::TrailingSlash(token.span, token.kind))
            }
            _ => (),
        })
        .collect();

    Partial::with_errors(tokens, errors)
}

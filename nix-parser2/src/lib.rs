//! Parser for the Nix programming language.

pub use crate::error::Error;

use codespan::Span;
use nix_errors::Errors;
use nix_lexer::Token;

use crate::cst::{CstBuilder, SyntaxKind, SyntaxNode};

pub mod ast;
pub mod cst;

mod error;
mod lexer;

/// A trait for converting a value to a `codespan::Span`.
///
/// This is helpful for getting spanned types from external crates to interoperate with `codespan`.
pub trait ToSpan {
    /// Converts the given value to a `Span`.
    fn to_span(&self) -> Span;
}

/// Converts an input string to a concrete syntax tree and a stack of parse errors, if any.
pub fn parse(input: &str) -> (SyntaxNode, Errors<Error>) {
    let (tokens, mut errors) = lexer::tokenize(input);
    let (tree, parse_errors) = Parser::from_tokens(tokens).parse();
    errors.extend(parse_errors);
    (tree, errors)
}

struct Parser {
    tokens: Vec<Token>,
    builder: CstBuilder,
}

impl Parser {
    pub fn from_tokens(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            builder: CstBuilder::new(),
        }
    }

    pub fn parse(mut self) -> (SyntaxNode, Errors<Error>) {
        self.builder.start_node(SyntaxKind::Root);
        // TODO: Implement parser here.
        self.builder.finish_node();
        self.builder.finish()
    }
}

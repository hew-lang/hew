//! Expansion of parsed declarative macro fragments before semantic checking.

mod matcher;
mod tokens;
mod transcribe;

use hew_parser::ast::Span;

pub use matcher::{Capture, Captures, Pattern};
pub use transcribe::{transcribe, Generated, OriginKind, TokenOrigin};

#[derive(Debug, Clone)]
pub struct ExpansionError {
    pub message: String,
    pub span: Span,
}

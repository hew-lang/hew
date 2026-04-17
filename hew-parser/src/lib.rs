//! Hew language parser — recursive descent with Pratt precedence.

pub mod ast;
pub mod ast_eq;
pub mod fmt;
pub mod module;
pub mod parser;
pub mod tail_call;

pub use parser::{parse, ParseError, ParseResult, Severity};

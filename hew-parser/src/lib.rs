//! Hew language parser — recursive descent with Pratt precedence.

pub mod ast;
pub mod ast_eq;
pub mod fmt;
pub mod loop_analysis;
pub mod module;
pub mod parser;
pub mod tail_call;

pub use loop_analysis::loop_body_has_break;
pub use parser::{
    parse, parse_duration_ns, ParseDiagnosticKind, ParseError, ParseResult, Severity,
};

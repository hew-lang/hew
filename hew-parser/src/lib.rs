//! Hew language parser — recursive descent with Pratt precedence.

pub mod ast;
pub mod ast_eq;
pub mod fmt;
pub mod init_analysis;
pub mod loop_analysis;
pub mod macro_syntax;
pub mod module;
pub mod parser;
pub mod tail_call;

pub use loop_analysis::{
    block_leaves_enclosing_loop, expr_leaves_enclosing_loop, loop_body_has_break,
    stmt_leaves_enclosing_loop,
};
pub use parser::{
    is_import_path_segment, parse, parse_duration_ns, parse_fragment, parse_generated,
    parse_generated_fragment, FragmentParseResult, ParseDiagnosticKind, ParseError, ParseResult,
    Severity,
};

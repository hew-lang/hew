//! Fail-closed guard for the retired in-process JIT evaluation path.
//!
//! The C ABI bridge formerly used by `hew eval --jit inprocess` belonged to
//! the retired native codegen substrate.  v0.5 keeps this module as the narrow
//! error surface consumed by dormant REPL/eval helpers, but it never fabricates
//! execution and never reaches a native backend.

/// Errors that can arise during in-process JIT execution.
#[derive(Debug, Clone)]
pub enum JitError {
    /// Evaluation cannot proceed through the retired JIT substrate.
    ExecFailed(String),
}

impl std::fmt::Display for JitError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExecFailed(msg) => write!(f, "JIT execution failed: {msg}"),
        }
    }
}

impl std::error::Error for JitError {}

/// Refuse in-process JIT execution until eval is rebuilt on the Rust codegen
/// substrate.  The argument remains so existing dormant call sites keep their
/// shape while the backend is unavailable.
pub fn run_jit(_msgpack_data: &[u8]) -> Result<i64, JitError> {
    Err(JitError::ExecFailed(
        "in-process JIT evaluation is unavailable during the v0.5 compiler \
         cutover; the retired native codegen bridge has been removed"
            .into(),
    ))
}

//! Fail-closed guard for the unavailable in-process JIT evaluation path.
//!
//! `hew eval --jit inprocess` / `--jit auto` require a Rust-codegen `ORCv2`
//! bridge that is not implemented yet. v0.5 keeps this module as the narrow
//! error surface, but it never fabricates execution and never reaches LLJIT.

/// Errors that can arise during in-process JIT execution.
#[derive(Debug, Clone)]
pub enum JitError {
    /// Evaluation cannot proceed until the `ORCv2` bridge exists.
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

/// Refuse in-process JIT execution until the Rust-codegen `ORCv2` bridge exists.
/// The argument remains so existing call sites keep their shape while the
/// backend is unavailable.
pub fn run_jit(_msgpack_data: &[u8]) -> Result<i64, JitError> {
    Err(JitError::ExecFailed(
        "in-process JIT evaluation is unavailable: the Rust v0.5 `ORCv2`/JIT \
         bridge is not implemented yet (#1227/#1235); use the AOT eval path \
         without `--jit=auto` or `--jit=inprocess`"
            .into(),
    ))
}

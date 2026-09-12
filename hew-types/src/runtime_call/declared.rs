//! Runtime methods generated from their canonical stdlib declarations.

use super::{RuntimeCallFamily, RuntimeOpRow};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeclaredRuntimeResult {
    StatusResult {
        error_type: &'static str,
        error_variant: &'static str,
    },
    DiscardStatus,
}

#[derive(Debug, Clone, Copy)]
pub struct DeclaredRuntimeMethod {
    pub module: &'static str,
    pub declaration: &'static str,
    pub family: RuntimeCallFamily,
    pub data_handler: &'static str,
    pub close_handler: &'static str,
    pub consumes_receiver: bool,
    pub result: DeclaredRuntimeResult,
    pub row: RuntimeOpRow,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeclaredRuntimeTarget {
    Native,
}

/// Logical declaration and native physical contract generated together.
#[derive(Debug, Clone, Copy)]
pub struct DeclaredDirectRuntimeMethod {
    pub signature: super::CanonicalStdlibExternSignature,
    /// Full source parameter sequence, including an explicit receiver.
    pub params: &'static [super::CanonicalExternTy],
    pub target: DeclaredRuntimeTarget,
    pub row: RuntimeOpRow,
}

#[must_use]
pub fn declared_direct_runtime_method(
    family: RuntimeCallFamily,
) -> Option<&'static DeclaredDirectRuntimeMethod> {
    DECLARED_DIRECT_RUNTIME_METHODS
        .iter()
        .find(|method| method.signature.family == Some(family))
}

include!(concat!(env!("OUT_DIR"), "/declared_runtime_methods.rs"));

#[must_use]
pub fn declared_runtime_method(signature_key: &str) -> Option<&'static DeclaredRuntimeMethod> {
    DECLARED_RUNTIME_METHODS
        .iter()
        .find(|method| method.declaration == signature_key)
}

/// Deterministic tooling projection of the same parsed declaration contracts.
pub const DECLARED_RUNTIME_EXPORTS_TOML: &str =
    include_str!(concat!(env!("OUT_DIR"), "/declared_runtime_exports.toml"));

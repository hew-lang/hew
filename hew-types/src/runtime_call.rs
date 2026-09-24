//! Typed contracts for compiler-known runtime operations.
//!
//! The checker selects runtime families and argument/result semantics. HIR and
//! SIR preserve those facts; physical lowering chooses their storage and ABI.
//! Adding an exported symbol does not by itself implement a source operation:
//! its selected family still needs a complete lowering and execution contract.
//!
//! This module lives in `hew-types` so source checking and downstream stages
//! consume the same family definitions without a dependency cycle. Foreign
//! symbol ownership is described separately by `crate::ffi_contracts`.

mod array;
pub use array::ArrayValueOp;

mod supervisor_pool;
pub use supervisor_pool::SupervisorPoolOp;

mod async_io;
pub use async_io::{AsyncIoLoan, AsyncIoOp, AsyncIoResume, IoHandleKind};
mod tcp;
pub use tcp::TcpOp;
mod file_resources;
use crate::{BuiltinType, ResolvedTy};
pub use file_resources::{FileReadHandleKind, FileReadOp};

mod declared;
pub use declared::{
    declared_direct_runtime_method, declared_runtime_method, DeclaredDirectRuntimeMethod,
    DeclaredRuntimeMethod, DeclaredRuntimeResult, DeclaredRuntimeTarget,
    DECLARED_DIRECT_RUNTIME_METHODS, DECLARED_RUNTIME_EXPORTS_TOML,
};

// Value/argument/result kinds and the ownership-verdict vocabulary shared by
// every runtime family.
mod value_kinds;
pub use value_kinds::*;

// The `RuntimeCallFamily` enum, its capability/extern tables and its
// checker-facing constructors.
mod family;
pub use family::*;

// The declarative per-variant operation-row table (`RuntimeCallFamily::row`)
// and the family's symbol/consume/suspension query methods. Both are `impl
// RuntimeCallFamily` blocks with no additional public items to re-export.
mod family_queries;
mod family_row;

// Row-table support types: descriptors, physical form and ABI shape.
mod descriptors;
pub use descriptors::*;

#[cfg(test)]
mod encoding_tests;
#[cfg(test)]
mod family_tests;
#[cfg(test)]
mod map_set_tests;
#[cfg(test)]
mod vector_tests;

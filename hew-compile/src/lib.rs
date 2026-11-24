//! Frontend compilation pipeline, module resolution, and semantic session boundary for Hew.

/// Build identity shared by the compiler driver and its frontend consumers.
pub const VERSION: &str = env!("HEW_VERSION");

/// Stack budget for native threads running the recursive compiler pipeline.
pub const COMPILER_STACK_SIZE: usize = 64 * 1024 * 1024;

mod compile_time;
mod coverage;
mod diagnostics;
mod frontend_options;
mod host;
mod host_admission;
pub mod live;
mod manifest;
mod module_graph;
mod module_loader;
mod pipeline;
mod session;
mod source_paths;
mod source_set;
#[cfg(test)]
mod tests;

pub use coverage::{SourceCoverageCounter, SourceCoverageMap};
pub use diagnostics::{
    hir_diagnostics_to_frontend, line_map_from_source, ownership_diagnostics_to_frontend,
    paths_name_same_file, CheckOutput, FrontendDiagnostic, FrontendDiagnosticKind, FrontendFailure,
    FrontendMessageDiagnostic, FrontendMessageNote, SourceSnapshot,
};
pub use frontend_options::{FrontendOptions, HostAdmission};
pub use manifest::{compiler_supports, COMPILER_VERSION};
pub use module_graph::{attach_prelude_std_modules, build_module_graph, inject_implicit_imports};
pub use module_loader::{
    module_membership, resolve_module_import, ImportResolutionContext, ResolvedImport,
};
pub use pipeline::{
    check_file, check_file_with_state, check_program, parse_source, run_document_frontend,
    run_file_frontend_to_expansion, run_file_frontend_to_typecheck, run_program_frontend,
    run_program_frontend_to_typecheck, run_source_frontend, typecheck_program,
    DocumentFrontendState, FileFrontendState, ProgramFrontendState, SourceAnalysisBatch,
    TypeCheckResult,
};
pub use session::{
    CheckSet, DiagnosticPolicy, Session, SessionError, SessionOutput, SessionTarget,
};
pub use source_paths::{
    display_source_path, register_source_path, source_path_policy, table_source_path,
    SourcePathPolicy, SourcePathScope,
};
pub use source_set::SourceSet;

pub(crate) use diagnostics::{configured_stdlib_roots, display_path, path_is_below};
pub(crate) use module_loader::anchor_for;
pub(crate) use source_set::{buffer_identity, read_source};

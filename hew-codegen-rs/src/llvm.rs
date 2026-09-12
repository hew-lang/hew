//! Shared LLVM target, optimization, diagnostic and artifact mechanics.

use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use std::path::PathBuf;
use std::sync::OnceLock;

#[derive(Debug)]
pub enum CodegenError {
    Llvm(String),
    Unsupported(&'static str),
    FailClosed(String),
    FailClosedAt { msg: String, span: (u32, u32) },
    UnsupportedAt { msg: &'static str, span: (u32, u32) },
    LlvmVerify(String),
    TargetSetup { triple: String, reason: String },
    Link(String),
    Io(std::io::Error),
}
impl CodegenError {
    /// Source byte range, when the emitter can attribute the failure.
    #[must_use]
    pub fn span(&self) -> Option<(u32, u32)> {
        match self {
            Self::FailClosedAt { span, .. } | Self::UnsupportedAt { span, .. } => Some(*span),
            _ => None,
        }
    }
}
impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Llvm(message) => write!(f, "llvm: {message}"),
            Self::Unsupported(message) | Self::UnsupportedAt { msg: message, .. } => {
                write!(f, "unsupported construct: {message}")
            }
            Self::FailClosed(message) | Self::FailClosedAt { msg: message, .. } => {
                write!(f, "fail-closed: {message}")
            }
            Self::LlvmVerify(message) => write!(f, "llvm verify rejected module: {message}"),
            Self::TargetSetup { triple, reason } => {
                write!(f, "target setup for `{triple}` failed: {reason}")
            }
            Self::Link(message) => write!(f, "link: {message}"),
            Self::Io(error) => write!(f, "io: {error}"),
        }
    }
}
impl std::error::Error for CodegenError {}
impl From<std::io::Error> for CodegenError {
    fn from(error: std::io::Error) -> Self {
        Self::Io(error)
    }
}
pub(crate) type CodegenResult<T> = Result<T, CodegenError>;
pub(crate) trait LlvmResultExt<T> {
    fn llvm_ctx(self, context: &'static str) -> CodegenResult<T>;
    fn llvm_ctx_with<F: FnOnce() -> String>(self, context: F) -> CodegenResult<T>;
}
impl<T, E: std::fmt::Debug> LlvmResultExt<T> for Result<T, E> {
    fn llvm_ctx(self, context: &'static str) -> CodegenResult<T> {
        self.map_err(|error| CodegenError::Llvm(format!("{context}: {error:?}")))
    }
    fn llvm_ctx_with<F: FnOnce() -> String>(self, context: F) -> CodegenResult<T> {
        self.map_err(|error| CodegenError::Llvm(format!("{}: {error:?}", context())))
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum OptLevel {
    #[default]
    O0,
    O2,
}
impl OptLevel {
    /// Parse supported optimization levels without silently changing a request.
    #[must_use]
    pub fn from_cli_str(value: &str) -> Option<Self> {
        match value {
            "0" => Some(Self::O0),
            "2" => Some(Self::O2),
            _ => None,
        }
    }
}
#[derive(Debug, Default, Clone)]
pub struct EmitArtefacts {
    pub ll_path: Option<PathBuf>,
    pub native_obj_path: Option<PathBuf>,
    pub wasm_obj_path: Option<PathBuf>,
    pub wasm_path: Option<PathBuf>,
}
pub(crate) fn run_module_pipeline(
    module: &Module<'_>,
    machine: &TargetMachine,
    level: OptLevel,
) -> CodegenResult<()> {
    if level == OptLevel::O2 {
        module
            .run_passes(
                "default<O2>",
                machine,
                inkwell::passes::PassBuilderOptions::create(),
            )
            .map_err(|error| CodegenError::Llvm(format!("O2 pass pipeline failed: {error}")))?;
    }
    module.verify().map_err(|error| {
        CodegenError::LlvmVerify(format!("module rejected after optimization: {error}"))
    })
}
/// The private process-entry body symbol; the platform entry remains an adapter.
#[must_use]
pub fn entry_body_symbol_for_triple(triple: &str) -> &'static str {
    if triple.starts_with("wasm32") {
        "__original_main"
    } else {
        "__hew_main_body"
    }
}
/// Returns the target triple to use when emitting native object files.
///
/// On macOS, `TargetMachine::get_default_triple()` returns the *system* triple
/// (e.g. `aarch64-apple-macosx26.0.0` on a host with the macOS 26 SDK installed),
/// which embeds a higher OS-minimum version into the object than the link step
/// targets.  This causes ld64.lld to emit a "has version X, which is newer than
/// target minimum of 13.0.0" warning for every object in the archive.
///
/// The fix is to construct the triple from the intended deployment target
/// (read from `MACOSX_DEPLOYMENT_TARGET`, defaulting to `"13.0"`) so the
/// emitted object's minimum-OS tag matches the linker's target minimum.
///
/// On non-macOS hosts the system default triple is returned unchanged.
#[cfg(target_os = "macos")]
#[must_use]
pub fn native_emission_triple() -> String {
    let default = TargetMachine::get_default_triple();
    let default_str = default.as_str().to_string_lossy();

    // Determine the arch prefix from the system default triple so we don't
    // hard-code it; fall back to the raw default when the arch is unrecognised.
    let arch = if default_str.starts_with("x86_64") {
        "x86_64"
    } else if default_str.starts_with("aarch64") || default_str.starts_with("arm64") {
        "aarch64"
    } else {
        return default_str.into_owned();
    };

    let deployment = std::env::var("MACOSX_DEPLOYMENT_TARGET")
        .ok()
        .filter(|v| !v.is_empty())
        .unwrap_or_else(|| "13.0".to_string());

    format!("{arch}-apple-macosx{deployment}")
}

#[cfg(not(target_os = "macos"))]
#[must_use]
pub fn native_emission_triple() -> String {
    TargetMachine::get_default_triple()
        .as_str()
        .to_string_lossy()
        .into_owned()
}

pub(crate) fn target_machine_for_triple_with_opt_level(
    triple: &str,
    opt_level: OptLevel,
) -> CodegenResult<TargetMachine> {
    initialise_llvm_targets();
    let target_triple = TargetTriple::create(triple);
    let target = Target::from_triple(&target_triple).map_err(|e| CodegenError::TargetSetup {
        triple: triple.to_string(),
        reason: format!("from_triple: {e:?}"),
    })?;
    target
        .create_target_machine(
            &target_triple,
            "generic",
            "",
            target_machine_optimization_level(opt_level),
            RelocMode::PIC,
            CodeModel::Default,
        )
        .ok_or_else(|| CodegenError::TargetSetup {
            triple: triple.to_string(),
            reason: "create_target_machine returned None".to_string(),
        })
}

fn target_machine_optimization_level(opt_level: OptLevel) -> inkwell::OptimizationLevel {
    match opt_level {
        OptLevel::O0 => inkwell::OptimizationLevel::None,
        OptLevel::O2 => inkwell::OptimizationLevel::Default,
    }
}

pub(crate) fn initialise_llvm_targets() {
    static INIT: OnceLock<()> = OnceLock::new();
    INIT.get_or_init(|| {
        Target::initialize_all(&InitializationConfig::default());
    });
}

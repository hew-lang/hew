//! Semantic compilation session boundary shared across execution targets.

use std::ops::Range;

use crate::frontend_options::FrontendOptions;
use crate::source_paths::{display_source_path, table_source_path};

/// Target facts that must stay coupled while a source is lowered.
///
/// This deliberately contains only facts passed by existing hosts: the HIR
/// architecture, MIR pointer width, and optional backend target triple.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SessionTarget {
    pub hir_arch: hew_hir::TargetArch,
    pub pointer_width: hew_mir::PointerWidth,
    pub codegen_triple: Option<String>,
}

impl SessionTarget {
    #[must_use]
    pub fn native() -> Self {
        Self {
            hir_arch: hew_hir::TargetArch::host(),
            pointer_width: hew_mir::PointerWidth::Bits64,
            codegen_triple: None,
        }
    }

    /// The browser sandbox VM.
    ///
    /// The VM interprets verified semantics with its own deterministic
    /// scheduler and has no machine layout at all, so this is not a codegen
    /// target and carries no triple. It is pinned to a 64-bit architecture
    /// because `isize` and `usize` are 64-bit in the VM, matching the native
    /// execution the parity oracle compares against, and pinned to one exact
    /// architecture so a browser result does not vary with the host that built
    /// the wasm package.
    #[must_use]
    pub fn browser() -> Self {
        Self {
            hir_arch: hew_hir::TargetArch::X86_64,
            pointer_width: hew_mir::PointerWidth::Bits64,
            codegen_triple: None,
        }
    }

    /// The `wasm32-wasi` codegen target.
    #[must_use]
    pub fn wasi() -> Self {
        Self {
            hir_arch: hew_hir::TargetArch::Wasm32,
            pointer_width: hew_mir::PointerWidth::Bits32,
            codegen_triple: None,
        }
    }
}

/// What a session runs after the SIR is lowered and verified.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CheckSet {
    /// Verify, then run the SIR optimization passes and verify their result:
    /// the SIR a build, run or `--explain-cow` report consumes.
    #[default]
    Build,
    /// Verify the lowered SIR only. Nothing in a diagnostics-only check reads
    /// the optimized form.
    Check,
}

/// Policy used when exposing diagnostics produced by a compilation session.
#[derive(Debug, Clone, Default)]
pub struct DiagnosticPolicy {
    pub warnings_as_errors: bool,
    pub lint_levels: hew_types::LintLevels,
}

/// The shared semantic compilation boundary. Every host enters through this
/// type with its target facts and resolved compilation roots explicit.
#[derive(Debug, Clone)]
pub struct Session {
    pub target: SessionTarget,
    pub checks: CheckSet,
    pub diagnostic_policy: DiagnosticPolicy,
}

impl Session {
    #[must_use]
    pub fn new(target: SessionTarget, diagnostic_policy: DiagnosticPolicy) -> Self {
        Self {
            target,
            checks: CheckSet::Build,
            diagnostic_policy,
        }
    }

    #[must_use]
    pub fn from_frontend_options(target: SessionTarget, options: &FrontendOptions) -> Self {
        Self::new(
            target,
            DiagnosticPolicy {
                warnings_as_errors: options.warnings_as_errors,
                lint_levels: options.lint_levels.clone(),
            },
        )
    }

    /// Lower a checked source program through the shared semantic boundary.
    ///
    /// # Errors
    ///
    /// Returns HIR lowering or semantic verification diagnostics.
    pub fn lower_program(
        &self,
        program: &hew_parser::ast::Program,
        tco: &hew_types::TypeCheckOutput,
    ) -> Result<SessionOutput, SessionError> {
        let module = self.lower_hir_program(program, tco)?;
        let mut roots = Self::source_roots(program, tco)?;
        roots.extend(tco.test_entry_plans.iter().map(|plan| plan.entry));
        self.lower_hir_module(&module, tco, &roots)
    }

    /// Lower checked declarations and generated test entries without entering SIR.
    ///
    /// # Errors
    /// Returns HIR lowering or test-entry installation diagnostics.
    pub fn lower_hir_program(
        &self,
        program: &hew_parser::ast::Program,
        tco: &hew_types::TypeCheckOutput,
    ) -> Result<hew_hir::HirModule, SessionError> {
        let mut lowered =
            hew_hir::lower_program(program, tco, &hew_hir::ResolutionCtx, self.target.hir_arch);
        if !lowered.diagnostics.is_empty() {
            return Err(SessionError::Hir(lowered.diagnostics));
        }
        if !tco.test_entry_plans.is_empty() {
            hew_hir::test_entry::install_test_entry_plans(
                &mut lowered.module,
                &tco.test_entry_plans,
            )
            .map_err(|message| SessionError::Lowering {
                callable: None,
                error: hew_sir::SirLoweringError::Internal(message),
                span: None,
            })?;
        }
        Ok(lowered.module)
    }

    /// Select concrete declarations exported by this checked source module.
    ///
    /// Root-source `pub` and `package` monomorphic functions require bodies.
    /// Imported declarations are not roots. Generic exports are templates:
    /// concrete caller demand supplies their specializations. These roots do
    /// not establish a public C calling convention or a stable embedding ABI.
    /// The checker-selected process entry is added by semantic lowering.
    ///
    /// # Errors
    ///
    /// Returns an error if a selected declaration lacks checker identity.
    pub fn source_roots(
        program: &hew_parser::ast::Program,
        tco: &hew_types::TypeCheckOutput,
    ) -> Result<Vec<hew_types::DefId>, SessionError> {
        let program = tco
            .normalized_program
            .as_ref()
            .map_or(program, |normalized| &normalized.program);
        // A file import's public declarations remain imports, with their own
        // source identities, rather than authored root exports.
        let root_items = program
            .module_graph
            .as_ref()
            .and_then(|graph| graph.modules.get(&graph.root))
            .map_or(program.items.as_slice(), |root| root.items.as_slice());
        root_items
            .iter()
            .enumerate()
            .filter_map(|(ordinal, (item, span))| {
                let hew_parser::ast::Item::Function(function) = item else {
                    return None;
                };
                if !function.visibility.is_pub()
                    || function
                        .type_params
                        .as_ref()
                        .is_some_and(|params| !params.is_empty())
                {
                    return None;
                }
                let occurrence = hew_types::DeclarationOccurrence::new_with_synthetic_ordinal(
                    tco.defs.root_module(),
                    span,
                    ordinal,
                    hew_types::DeclarationKind::Function,
                    0,
                );
                Some(
                    tco.defs
                        .declaration(occurrence)
                        .ok_or_else(|| SessionError::Lowering {
                            callable: None,
                            error: hew_sir::SirLoweringError::Internal(format!(
                                "exported source declaration at {span:?} has no checked identity"
                            )),
                            span: Some(span.clone()),
                        }),
                )
            })
            .collect()
    }

    /// Verify and canonicalize ownership semantics shared by every host.
    /// `roots` are resolved concrete declarations, in addition to the selected
    /// process entry. Source consumers use [`Self::source_roots`] to preserve
    /// the same export policy as [`Self::lower_program`].
    ///
    /// # Errors
    ///
    /// Returns the first failing semantic boundary with its diagnostics.
    pub fn lower_hir_module(
        &self,
        module: &hew_hir::HirModule,
        tco: &hew_types::TypeCheckOutput,
        roots: &[hew_types::DefId],
    ) -> Result<SessionOutput, SessionError> {
        self.lower_analyzed_hir(hew_hir::analyze_hir(module), tco, roots)
    }

    /// Reuse structural and source facts for an unchanged HIR module.
    ///
    /// # Errors
    /// Returns HIR diagnostics or a failure of semantic lowering or verification.
    pub fn lower_analyzed_hir(
        &self,
        analysis: hew_hir::HirAnalysis<'_>,
        tco: &hew_types::TypeCheckOutput,
        roots: &[hew_types::DefId],
    ) -> Result<SessionOutput, SessionError> {
        let module = analysis.module();
        if !analysis.diagnostics().is_empty() {
            return Err(SessionError::Hir(analysis.diagnostics().to_vec()));
        }
        let mut roots = roots.to_vec();
        roots.extend(tco.c_abi.exports.keys().copied());
        let sir =
            hew_sir::lower_analyzed_module_with_roots(analysis, tco, &roots).map_err(|errors| {
                let Some(error) = errors.into_iter().next() else {
                    return SessionError::Lowering {
                        callable: None,
                        error: hew_sir::SirLoweringError::Internal(
                            "root selection failed without reporting a cause".into(),
                        ),
                        span: None,
                    };
                };
                SessionError::Lowering {
                    callable: None,
                    error: error.reason.with_context(&format!(
                        "SIR root `{}`",
                        module.defs.path(error.declaration)
                    )),
                    span: None,
                }
            })?;
        let mut sir =
            hew_sir::VerifiedModule::new(sir).map_err(SessionError::from_semantic_diagnostics)?;
        let semantic = &sir.lowered().module;
        require_complete_semantics(sir.lowered(), module.entry_exit_plan.is_some())?;
        if semantic.test_entries.len() != module.test_entry_plans.len()
            || semantic
                .test_entries
                .iter()
                .zip(&module.test_entry_plans)
                .any(|(entry, plan)| entry.declaration != plan.entry)
        {
            return Err(SessionError::Lowering {
                callable: None,
                error: hew_sir::SirLoweringError::Internal(
                    "semantic test dispatcher does not match checked entry plans".into(),
                ),
                span: None,
            });
        }
        let body_index = semantic.function_index();
        let mut compiled_roots = roots
            .iter()
            .map(|declaration| {
                let callable = semantic
                    .callable_for_declaration(declaration)
                    .ok_or_else(|| SessionError::Lowering {
                        callable: None,
                        error: hew_sir::SirLoweringError::Internal(format!(
                            "selected declaration {declaration:?} has no semantic callable"
                        )),
                        span: None,
                    })?
                    .id;
                require_semantic_body(semantic, &body_index, callable)?;
                Ok(callable)
            })
            .collect::<Result<Vec<_>, SessionError>>()?;
        if let Some(entry) = semantic.entry_callable {
            compiled_roots.push(entry);
        }
        for entry in &semantic.test_entries {
            require_semantic_body(semantic, &body_index, entry.callable)?;
            compiled_roots.push(entry.callable);
        }
        compiled_roots.sort_unstable();
        compiled_roots.dedup();
        if self.checks == CheckSet::Build {
            let mut lowered = sir.into_lowered();
            hew_sir::canonicalize_module_constant_cfg(&mut lowered.module)
                .map_err(SessionError::Semantic)?;
            hew_sir::transfer_module_dead_local_reads(&mut lowered.module);
            sir = hew_sir::VerifiedModule::new(lowered)
                .map_err(SessionError::from_semantic_diagnostics)?;
        }
        let sources = source_files(&tco.file_span_indices, &sir.lowered().module);
        Ok(SessionOutput {
            sir,
            compiled_roots,
            sources,
        })
    }
}

/// Measure every source file a lowered body indexes, by file span index. A
/// file that cannot be read (an in-memory root) is left out; its bodies keep
/// their sites but carry no lines.
fn source_files(
    indices: &hew_parser::module::FileSpanIndices,
    module: &hew_sir::SemModule,
) -> std::collections::BTreeMap<u32, hew_mir::physical::PhysicalSourceFile> {
    let files: std::collections::BTreeSet<u32> = module
        .functions
        .iter()
        .filter_map(|function| module.debug.item_file(function.id))
        .collect();
    files
        .into_iter()
        .filter_map(|file| {
            let path = indices.file_path(file)?;
            let text = std::fs::read_to_string(path).ok()?;
            let display = table_source_path(path).to_string_lossy().into_owned();
            let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
            let debug_path = display_source_path(&canonical);
            let mut source = hew_mir::physical::PhysicalSourceFile::new(canonical, display, &text);
            source.debug_path = debug_path;
            Some((file, source))
        })
        .collect()
}

/// Verified semantic compilation result, independent of the execution host.
#[derive(Debug)]
pub struct SessionOutput {
    pub(crate) sir: hew_sir::VerifiedModule,
    compiled_roots: Vec<hew_sir::CallableId>,
    /// Source files the lowered bodies index, for the image's file table.
    pub(crate) sources: std::collections::BTreeMap<u32, hew_mir::physical::PhysicalSourceFile>,
}

impl SessionOutput {
    /// Resolved entry and export roots whose complete call closures were lowered.
    #[must_use]
    pub fn compiled_roots(&self) -> &[hew_sir::CallableId] {
        &self.compiled_roots
    }

    /// Inspect the verified semantic result without invalidating it.
    #[must_use]
    pub fn semantics(&self) -> &hew_sir::LoweredModule {
        self.sir.lowered()
    }

    /// Consume the session result and relinquish its verification guarantee.
    #[must_use]
    pub fn into_semantics(self) -> hew_sir::LoweredModule {
        self.sir.into_lowered()
    }

    /// Prepare one physical type inventory tied to this checked semantic input.
    #[must_use]
    pub fn physical_plan(&self) -> hew_mir::physical::PhysicalPlan<'_> {
        hew_mir::physical::PhysicalPlan::new(self.sir.checked()).with_sources(self.sources.clone())
    }

    /// Realize verified semantics using the backend's measured target layouts.
    ///
    /// # Errors
    ///
    /// Returns a physical lowering or verification diagnostic.
    pub fn lower_physical(
        &self,
        target: hew_mir::PhysicalTarget,
    ) -> Result<hew_mir::VerifiedPhysicalModule, hew_mir::PhysicalError> {
        self.physical_plan().lower(target)
    }
}

/// Diagnostics from the shared semantic compilation boundary.
#[derive(Debug)]
pub enum SessionError {
    Hir(Vec<hew_hir::HirDiagnostic>),
    Semantic(Vec<hew_sir::SirDiagnostic>),
    /// Source ownership obligations diagnosed by the semantic lifetime flow.
    Ownership(Vec<hew_sir::SirDiagnostic>),
    Selection(String),
    Lowering {
        callable: Option<hew_sir::CallableId>,
        error: hew_sir::SirLoweringError,
        /// The provoking construct's span, when SIR could attribute one
        /// (#3384). `None` for a session-level refusal with no declaration
        /// of its own (a missing entry, an unresolved root).
        span: Option<Range<usize>>,
    },
}

impl SessionError {
    fn from_semantic_diagnostics(diagnostics: Vec<hew_sir::SirDiagnostic>) -> Self {
        if diagnostics.iter().all(|diagnostic| {
            matches!(
                diagnostic.kind,
                hew_sir::SirDiagnosticKind::UnconsumedLinear { .. }
                    | hew_sir::SirDiagnosticKind::InvalidGeneratorSend { .. }
            )
        }) {
            Self::Ownership(diagnostics)
        } else {
            Self::Semantic(diagnostics)
        }
    }
}

impl std::fmt::Display for SessionError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Hir(diagnostics) => write!(formatter, "HIR verification failed: {diagnostics:?}"),
            Self::Ownership(diagnostics) => {
                write!(formatter, "unconsumed linear values: {diagnostics:?}")
            }
            Self::Semantic(diagnostics) => {
                write!(formatter, "SIR verification failed: {diagnostics:?}")
            }
            Self::Lowering { error, .. } => error.fmt(formatter),
            Self::Selection(message) => formatter.write_str(message),
        }
    }
}

impl std::error::Error for SessionError {}

/// Headers without bodies are valid SIR, but a selected executable must close
/// every demanded body. Join by resolved identity, never by emitted names.
fn require_complete_semantics(
    sir: &hew_sir::LoweredModule,
    entry_required: bool,
) -> Result<(), SessionError> {
    if entry_required && sir.module.entry_callable.is_none() {
        return Err(SessionError::Lowering {
            callable: None,
            error: hew_sir::SirLoweringError::Internal(
                "the selected process entry has no semantic callable".into(),
            ),
            span: None,
        });
    }
    for (callable, status) in &sir.callable_statuses {
        if let hew_sir::SirLoweringStatus::Failed { reason, span } = status {
            let name = sir
                .module
                .callable(*callable)
                .map_or("<unknown>", |item| item.symbol.as_str());
            return Err(SessionError::Lowering {
                callable: Some(*callable),
                error: reason
                    .clone()
                    .with_context(&format!("semantic lowering of `{name}`")),
                span: span.clone(),
            });
        }
    }
    let body_index = sir.module.function_index();
    for callable in sir
        .module
        .structural_display
        .values()
        .filter_map(|render| render.display)
    {
        require_semantic_body(&sir.module, &body_index, callable)?;
    }
    for plan in sir.module.value_capabilities.values() {
        if let Some(callable) = plan.callable {
            require_semantic_body(&sir.module, &body_index, callable)?;
        }
    }
    if let Some(entry) = sir.module.entry_callable {
        require_semantic_body(&sir.module, &body_index, entry)?;
    }
    for function in &sir.module.functions {
        for block in &function.blocks {
            if let hew_sir::SemTerminator::Call { callee, .. } = &block.terminator {
                require_semantic_body(&sir.module, &body_index, *callee)?;
            }
        }
    }
    Ok(())
}

fn require_semantic_body(
    module: &hew_sir::SemModule,
    body_index: &hew_sir::SemFunctionIndex<'_>,
    callable: hew_sir::CallableId,
) -> Result<(), SessionError> {
    if body_index.function(callable).is_none() {
        let name = module
            .callable(callable)
            .map_or("<unknown>", |item| item.symbol.as_str());
        return Err(SessionError::Lowering {
            callable: Some(callable),
            error: hew_sir::SirLoweringError::Internal(format!(
                "required semantic callable `{name}` has no body"
            )),
            span: None,
        });
    }
    Ok(())
}

#[cfg(test)]
mod session_completion_tests {
    use super::*;
    use crate::pipeline::{parse_source, run_file_frontend_to_typecheck};

    fn complete_module() -> hew_sir::LoweredModule {
        let program = parse_source(
            "fn leaf() -> i64 { 7 } fn unused() -> i64 { 9 } fn main() -> i64 { leaf() }",
            "completion.hew",
        )
        .unwrap();
        let mut checker =
            hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let tco = checker.check_program(&program);
        assert!(tco.errors.is_empty(), "{:?}", tco.errors);
        Session::new(SessionTarget::native(), DiagnosticPolicy::default())
            .lower_program(&program, &tco)
            .unwrap()
            .into_semantics()
    }

    /// `unused` is declared and never called. Demand reaches neither its
    /// header nor its body, and that absence is not an incomplete lowering:
    /// the gate asks for the entry, the callees the bodies name and the
    /// selected capability plans, not for every declaration in the source.
    #[test]
    fn a_declaration_the_entry_never_reaches_does_not_make_a_module_incomplete() {
        let sir = complete_module();
        assert!(
            sir.module
                .callables
                .iter()
                .all(|callable| !callable.symbol.contains("unused")),
            "an uncalled declaration must not be admitted a callable: {:?}",
            sir.module.callables
        );
        require_complete_semantics(&sir, true).unwrap();
    }

    #[test]
    fn selected_entry_must_have_a_callable_and_body() {
        let mut sir = complete_module();
        let entry = sir.module.entry_callable.unwrap();
        sir.module
            .functions
            .retain(|function| function.callable != entry);
        assert!(matches!(
            require_complete_semantics(&sir, true),
            Err(SessionError::Lowering { callable: Some(id), error: hew_sir::SirLoweringError::Internal(_), .. }) if id == entry
        ));
        sir.module.entry_callable = None;
        assert!(matches!(
            require_complete_semantics(&sir, true),
            Err(SessionError::Lowering {
                callable: None,
                error: hew_sir::SirLoweringError::Internal(_),
                ..
            })
        ));
    }

    #[test]
    fn demanded_callee_must_have_a_body() {
        let mut sir = complete_module();
        let callee = sir
            .module
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .find_map(|block| match block.terminator {
                hew_sir::SemTerminator::Call { callee, .. } => Some(callee),
                _ => None,
            })
            .expect("the source contains a direct call");
        sir.module
            .functions
            .retain(|function| function.callable != callee);
        assert!(matches!(
            require_complete_semantics(&sir, true),
            Err(SessionError::Lowering { callable: Some(id), error: hew_sir::SirLoweringError::Internal(_), .. }) if id == callee
        ));
    }

    #[test]
    fn lowering_refusal_cannot_be_hidden_by_a_verified_body() {
        let mut sir = complete_module();
        let entry = sir.module.entry_callable.unwrap();
        let (_, status) = sir
            .callable_statuses
            .iter_mut()
            .find(|(callable, _)| *callable == entry)
            .unwrap();
        *status = hew_sir::SirLoweringStatus::Failed {
            reason: hew_sir::SirLoweringError::Ownership {
                kind: hew_sir::SirOwnershipError::ConsumeBorrowed,
                message: "deliberate ownership refusal".into(),
            },
            span: None,
        };
        assert!(matches!(
            require_complete_semantics(&sir, true),
            Err(SessionError::Lowering { callable: Some(id), error, .. })
                if id == entry && matches!(error, hew_sir::SirLoweringError::Ownership { kind: hew_sir::SirOwnershipError::ConsumeBorrowed, .. }) && error.message().contains("deliberate ownership refusal")
        ));
    }

    #[test]
    fn source_exports_select_only_concrete_root_declarations() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(
            dir.path().join("library.hew"),
            "pub fn imported() -> i64 { 99 }",
        )
        .unwrap();
        let source = dir.path().join("exports.hew");
        std::fs::write(&source,
            "import library; pub fn answer() -> i64 { helper() } package fn neighbour() -> i64 { 8 } fn helper() -> i64 { 42 } pub fn generic<T>(value: T) -> T { value }").unwrap();
        let state =
            run_file_frontend_to_typecheck(source.to_str().unwrap(), &FrontendOptions::default())
                .unwrap();
        let tco = state.typecheck_result.tco.as_ref().unwrap();
        let output = Session::new(SessionTarget::native(), DiagnosticPolicy::default())
            .lower_program(&state.program, tco)
            .unwrap();
        let module = &output.semantics().module;
        assert!(module.entry_callable.is_none());
        assert_eq!(output.compiled_roots().len(), 2);
        // `SemFunction::name` carries the emitted symbol (`__hew_fn_answer`);
        // the declaration is the source-name authority, and selection is about
        // which declarations compile.
        let mut bodies = module
            .callables
            .iter()
            .filter(|callable| callable.source_origin == hew_sir::FunctionSourceOrigin::RootUnit)
            .map(|callable| module.defs.path(callable.declaration))
            .collect::<Vec<_>>();
        bodies.sort_unstable();
        assert_eq!(
            bodies,
            ["exports.answer", "exports.helper", "exports.neighbour"]
        );
        for root in output.compiled_roots() {
            assert!(module.function_index().function(*root).is_some());
        }
    }
}

//! Pretty error rendering with source spans and ANSI colours.
//!
//! Produces Rust/Elm-style diagnostics with `^^^` underlines pointing at the
//! relevant source location.

use std::cell::RefCell;
use std::collections::HashMap;
use std::io::IsTerminal;
use std::ops::Range;

// ---------------------------------------------------------------------------
// Thread-local diagnostic capture
// ---------------------------------------------------------------------------
//
// When active, all `diag_println` calls append to a string buffer instead of
// writing to stderr.  Used by `hew eval --json` to collect diagnostic text
// into the JSON run contract without altering the normal (non-JSON) path.

thread_local! {
    static DIAG_CAPTURE: RefCell<Option<String>> = const { RefCell::new(None) };
}

/// Activate per-thread diagnostic capture.
///
/// All subsequent [`diag_println`] calls on this thread append to an internal
/// buffer instead of writing to stderr.  Call [`finish_diagnostic_capture`] to
/// retrieve the accumulated text and deactivate capture.
///
/// Capture is not re-entrant: calling this while capture is already active
/// resets the buffer.
pub(crate) fn start_diagnostic_capture() {
    DIAG_CAPTURE.with(|c| *c.borrow_mut() = Some(String::new()));
}

/// Deactivate per-thread diagnostic capture and return the accumulated text.
///
/// Returns an empty string if capture was not active.
pub(crate) fn finish_diagnostic_capture() -> String {
    DIAG_CAPTURE.with(|c| c.borrow_mut().take().unwrap_or_default())
}

/// Write a diagnostic line to the active capture buffer, or to stderr if no
/// capture is active.
fn diag_println(s: &str) {
    DIAG_CAPTURE.with(|c| {
        if let Some(ref mut buf) = *c.borrow_mut() {
            buf.push_str(s);
            buf.push('\n');
        } else {
            eprintln!("{s}");
        }
    });
}

/// Emit a plain diagnostic line through the capture-aware sink.
pub(crate) fn emit_plain_diagnostic_line(s: &str) {
    diag_println(s);
}

const ROOT_SOURCE_CONTEXT_UNAVAILABLE: &str =
    "source context unavailable: root source not attached to frontend diagnostic";

/// Map an HIR diagnostic kind to a user-visible prefix string.
pub(crate) fn hir_diagnostic_prefix(kind: &hew_hir::HirDiagnosticKind) -> &'static str {
    match kind {
        hew_hir::HirDiagnosticKind::NotYetImplemented { .. } => "E_NOT_YET_IMPLEMENTED",
        _ => "E_HIR",
    }
}

/// Stable, user-facing string identifier for an HIR diagnostic kind.
///
/// Returns the variant name (e.g. `NotYetImplemented`, `UnresolvedSymbol`)
/// without any of the Rust `{:?}` struct payload. Used as the `code` field in
/// JSON diagnostics and matches the LSP's `hir_diagnostic_kind_string`.
pub(crate) fn hir_diagnostic_kind_string(kind: &hew_hir::HirDiagnosticKind) -> String {
    let debug = format!("{kind:?}");
    let end = debug.find([' ', '{', '(']).unwrap_or(debug.len());
    debug[..end].to_string()
}

/// Build the user-facing message for an HIR diagnostic.
///
/// For a `NotYetImplemented` gap, the message frames the gap as a current
/// compiler limitation — not a problem with the user's code — and never leaks
/// the Rust `{:?}` field names (`construct`, `owning_pass`). Other kinds fall
/// back to the stable prefix plus any attached note.
pub(crate) fn hir_diagnostic_user_message(diagnostic: &hew_hir::HirDiagnostic) -> String {
    if let hew_hir::HirDiagnosticKind::NotYetImplemented { construct, .. } = &diagnostic.kind {
        let mut message = format!(
            "`{construct}` is not yet supported by the Hew compiler \
             (this is a current Hew limitation, not your code)"
        );
        if !diagnostic.note.is_empty() {
            message.push_str(": ");
            message.push_str(&diagnostic.note);
        }
        return message;
    }

    let prefix = hir_diagnostic_prefix(&diagnostic.kind);
    if diagnostic.note.is_empty() {
        prefix.to_string()
    } else {
        format!("{prefix}: {}", diagnostic.note)
    }
}

/// Map a MIR diagnostic kind to the stable CLI diagnostic family.
pub(crate) fn mir_diagnostic_prefix(kind: &hew_mir::MirDiagnosticKind) -> &'static str {
    match kind {
        hew_mir::MirDiagnosticKind::UseAfterConsume { .. }
        | hew_mir::MirDiagnosticKind::InitialisedBeforeUse { .. }
        | hew_mir::MirDiagnosticKind::DecisionMapTotal { .. }
        | hew_mir::MirDiagnosticKind::MustConsume { .. }
        | hew_mir::MirDiagnosticKind::DropPlanUndetermined { .. }
        | hew_mir::MirDiagnosticKind::ContextBoundaryViolation { .. }
        | hew_mir::MirDiagnosticKind::ContextBindingEscapes { .. }
        | hew_mir::MirDiagnosticKind::ClosurePairBorrowedStore { .. } => "E_MIR_CHECK",
        hew_mir::MirDiagnosticKind::NotYetImplemented { .. }
        | hew_mir::MirDiagnosticKind::OwnedHandleAggregateExtractionUnsupported { .. } => {
            "E_NOT_YET_IMPLEMENTED"
        }
        hew_mir::MirDiagnosticKind::RemotePayloadUnsupported { .. } => {
            "E_REMOTE_PAYLOAD_UNSUPPORTED"
        }
        hew_mir::MirDiagnosticKind::InvalidActorSpawnArgument { .. } => "E_INVALID_SPAWN_ARGUMENT",
        hew_mir::MirDiagnosticKind::UnknownType { .. }
        | hew_mir::MirDiagnosticKind::UnsupportedUserRecordValueClass { .. }
        | hew_mir::MirDiagnosticKind::UnsupportedNode { .. }
        | hew_mir::MirDiagnosticKind::UnresolvedPlace { .. }
        | hew_mir::MirDiagnosticKind::CannotMaterializeClosureCapture { .. }
        | hew_mir::MirDiagnosticKind::UnknownActorStateField { .. }
        | hew_mir::MirDiagnosticKind::ActorHandlerSymbolCollision { .. }
        | hew_mir::MirDiagnosticKind::ActorStateCloneClassificationFailed { .. }
        | hew_mir::MirDiagnosticKind::SelectArmNotImplemented { .. }
        | hew_mir::MirDiagnosticKind::UnresolvedStaticDispatchSubstitution { .. }
        | hew_mir::MirDiagnosticKind::StaticDispatchImplNotFound { .. }
        | hew_mir::MirDiagnosticKind::StaticDispatchMonomorphisationMissing { .. }
        | hew_mir::MirDiagnosticKind::TraitObjectStorageUndetermined { .. }
        | hew_mir::MirDiagnosticKind::CallTraitMethodSignatureUnresolved { .. }
        | hew_mir::MirDiagnosticKind::ClosureCapturesDuplexHandle { .. } => "E_MIR",
    }
}

pub(crate) fn render_codegen_front_diagnostic(error: &hew_codegen_rs::CodegenError) {
    emit_plain_diagnostic_line(&format!(
        "E_CODEGEN_FRONT: codegen-front validation failed: {error}"
    ));
}

// ANSI colour helpers
const RED: &str = "\x1b[1;31m";
const YELLOW: &str = "\x1b[1;33m";
const BLUE: &str = "\x1b[1;34m";
const CYAN: &str = "\x1b[1;36m";
const BOLD: &str = "\x1b[1m";
const RESET: &str = "\x1b[0m";

struct DiagnosticPalette {
    red: &'static str,
    yellow: &'static str,
    blue: &'static str,
    cyan: &'static str,
    bold: &'static str,
    reset: &'static str,
}

fn diagnostic_capture_active() -> bool {
    DIAG_CAPTURE.with(|c| c.borrow().is_some())
}

fn should_use_ansi_output(
    stderr_is_terminal: bool,
    no_color_set: bool,
    diagnostic_capture_active: bool,
) -> bool {
    !diagnostic_capture_active && !no_color_set && stderr_is_terminal
}

pub(crate) fn use_ansi_diagnostics() -> bool {
    should_use_ansi_output(
        std::io::stderr().is_terminal(),
        std::env::var_os("NO_COLOR").is_some(),
        diagnostic_capture_active(),
    )
}

fn diagnostic_palette() -> DiagnosticPalette {
    if use_ansi_diagnostics() {
        DiagnosticPalette {
            red: RED,
            yellow: YELLOW,
            blue: BLUE,
            cyan: CYAN,
            bold: BOLD,
            reset: RESET,
        }
    } else {
        DiagnosticPalette {
            red: "",
            yellow: "",
            blue: "",
            cyan: "",
            bold: "",
            reset: "",
        }
    }
}

/// A secondary note attached to a diagnostic.
#[derive(Debug)]
pub struct DiagnosticNote<'a> {
    pub span: &'a Range<usize>,
    pub message: &'a str,
}

pub(crate) type ModuleSourceMap = HashMap<String, (String, String)>;

/// Render a diagnostic message with source context and span underline.
///
/// Prints to stderr:
/// ```text
/// filename:line:col: error: message
///   line_num | source line
///            | ^^^^^^^^^^^
///   = note: ...
///   = help: ...
/// ```
pub fn render_diagnostic(
    source: &str,
    filename: &str,
    span: &Range<usize>,
    message: &str,
    notes: &[DiagnosticNote<'_>],
    suggestions: &[String],
) {
    let palette = diagnostic_palette();
    let (line, col) = offset_to_line_col(source, span.start);

    // Header: filename:line:col: error: message
    diag_println(&format!(
        "{bold}{filename}:{line}:{col}:{reset} {red}error{reset}{bold}: {message}{reset}",
        bold = palette.bold,
        red = palette.red,
        reset = palette.reset,
    ));

    render_source_underline(source, span, line, &palette);

    // Secondary notes with their own spans
    for note in notes {
        let (note_line, note_col) = offset_to_line_col(source, note.span.start);
        diag_println(&format!(
            "{bold}{filename}:{note_line}:{note_col}:{reset} {cyan}note{reset}{bold}: {message}{reset}",
            bold = palette.bold,
            cyan = palette.cyan,
            message = note.message,
            reset = palette.reset,
        ));
        render_source_underline(source, note.span, note_line, &palette);
    }

    // Suggestions
    for suggestion in suggestions {
        diag_println(&format!(
            "  {cyan}= help{reset}: {suggestion}",
            cyan = palette.cyan,
            reset = palette.reset,
        ));
    }
}

/// Render a warning message with source context and span underline.
///
/// Same layout as [`render_diagnostic`] but prints `warning` in yellow.
pub fn render_warning(
    source: &str,
    filename: &str,
    span: &Range<usize>,
    message: &str,
    notes: &[DiagnosticNote<'_>],
    suggestions: &[String],
) {
    let palette = diagnostic_palette();
    let (line, col) = offset_to_line_col(source, span.start);

    diag_println(&format!(
        "{bold}{filename}:{line}:{col}:{reset} {yellow}warning{reset}{bold}: {message}{reset}",
        bold = palette.bold,
        reset = palette.reset,
        yellow = palette.yellow,
    ));

    render_source_underline(source, span, line, &palette);

    for note in notes {
        let (note_line, note_col) = offset_to_line_col(source, note.span.start);
        diag_println(&format!(
            "{bold}{filename}:{note_line}:{note_col}:{reset} {cyan}note{reset}{bold}: {message}{reset}",
            bold = palette.bold,
            cyan = palette.cyan,
            message = note.message,
            reset = palette.reset,
        ));
        render_source_underline(source, note.span, note_line, &palette);
    }

    for suggestion in suggestions {
        diag_println(&format!(
            "  {cyan}= help{reset}: {suggestion}",
            cyan = palette.cyan,
            reset = palette.reset,
        ));
    }
}

/// Print diagnostic-only `info[HEW-PERF-001]` stack-allocation hints to stderr.
///
/// One line per [`hew_types::check::StackHint`], formatted as:
///
/// ```text
/// <file>:<line>:<col>: info[HEW-PERF-001]: binding `<name>` (<class>) could be stack-allocated
/// ```
///
/// Severity is `info`. This function never affects exit code or stdout. It is
/// invoked only when the user passes `--show-stack-hints` to `hew check` or
/// `hew run`. The diagnostic code (`HEW-PERF-001`) and the `file:line:col`
/// prefix are stable UX surface; the trailing message text is best-effort.
pub fn print_stack_hints(source: &str, filename: &str, hints: &[hew_types::check::StackHint]) {
    let palette = diagnostic_palette();
    for hint in hints {
        let (line, col) = offset_to_line_col(source, hint.span_key.start);
        let class_label = stack_hint_alloc_class_label(&hint.alloc_class);
        let name = if hint.binding_name.is_empty() {
            // Bindings produced by destructuring patterns have no single name;
            // render a placeholder so the line stays well-formed.
            "<destructured>"
        } else {
            hint.binding_name.as_str()
        };
        diag_println(&format!(
            "{bold}{filename}:{line}:{col}:{reset} {blue}info[HEW-PERF-001]{reset}{bold}: \
             binding `{name}` ({class_label}) could be stack-allocated{reset}",
            bold = palette.bold,
            blue = palette.blue,
            reset = palette.reset,
        ));
    }
}

fn stack_hint_alloc_class_label(class: &hew_types::check::AllocationClass) -> &'static str {
    use hew_types::check::AllocationClass;
    // Stable display labels matching the AllocationClass discriminant names
    // documented on the type. `Stack` and `Indeterminate` should never reach
    // this function (the walker filters them out before recording a hint),
    // but we render them defensively rather than panicking.
    match class {
        AllocationClass::Vec => "Vec",
        AllocationClass::String => "string",
        AllocationClass::HashMap => "HashMap",
        AllocationClass::HashSet => "HashSet",
        AllocationClass::Rc => "Rc",
        AllocationClass::ClosureEnv => "ClosureEnv",
        AllocationClass::Stack => "Stack",
        AllocationClass::Indeterminate => "Indeterminate",
    }
}

/// Render an error diagnostic where notes are provided as `(span, message)` pairs.
///
/// Convenience wrapper around [`render_diagnostic`] for callers that hold notes as
/// raw `(Range<usize>, String)` tuples rather than [`DiagnosticNote`] slices.
pub fn render_diagnostic_with_raw_notes(
    source: &str,
    filename: &str,
    span: &Range<usize>,
    message: &str,
    raw_notes: &[(Range<usize>, String)],
    suggestions: &[String],
) {
    let notes: Vec<DiagnosticNote<'_>> = raw_notes
        .iter()
        .map(|(s, msg)| DiagnosticNote {
            span: s,
            message: msg.as_str(),
        })
        .collect();
    render_diagnostic(source, filename, span, message, &notes, suggestions);
}

/// Render a warning diagnostic where notes are provided as `(span, message)` pairs.
///
/// Convenience wrapper around [`render_warning`] for callers that hold notes as
/// raw `(Range<usize>, String)` tuples rather than [`DiagnosticNote`] slices.
pub fn render_warning_with_raw_notes(
    source: &str,
    filename: &str,
    span: &Range<usize>,
    message: &str,
    raw_notes: &[(Range<usize>, String)],
    suggestions: &[String],
) {
    let notes: Vec<DiagnosticNote<'_>> = raw_notes
        .iter()
        .map(|(s, msg)| DiagnosticNote {
            span: s,
            message: msg.as_str(),
        })
        .collect();
    render_warning(source, filename, span, message, &notes, suggestions);
}

/// Build a map from dotted module path to `(source_text, display_filename)` for
/// every non-root module in the program that has an on-disk source file.
///
/// // WASM-TODO(#1451): `std::fs` is unavailable in WASM / no-fs contexts, so this
/// // map is empty there and non-root diagnostics fall back to root-source
/// // rendering until the WASM diagnostic pass grows a source-provider hook.
pub(crate) fn build_module_source_map(program: &hew_parser::ast::Program) -> ModuleSourceMap {
    let Some(ref module_graph) = program.module_graph else {
        return ModuleSourceMap::new();
    };

    let mut map = ModuleSourceMap::new();
    for mod_id in &module_graph.topo_order {
        if *mod_id == module_graph.root {
            continue;
        }
        let Some(module) = module_graph.modules.get(mod_id) else {
            continue;
        };
        let Some(path) = module.source_paths.first() else {
            continue;
        };
        if let Ok(text) = std::fs::read_to_string(path) {
            map.insert(mod_id.path.join("."), (text, path.display().to_string()));
        }
    }
    map
}

fn mir_kind_name(kind: &hew_mir::MirDiagnosticKind) -> &'static str {
    match kind {
        hew_mir::MirDiagnosticKind::UseAfterConsume { .. } => "UseAfterConsume",
        hew_mir::MirDiagnosticKind::InitialisedBeforeUse { .. } => "InitialisedBeforeUse",
        hew_mir::MirDiagnosticKind::DecisionMapTotal { .. } => "DecisionMapTotal",
        hew_mir::MirDiagnosticKind::MustConsume { .. } => "MustConsume",
        hew_mir::MirDiagnosticKind::UnknownType { .. } => "UnknownType",
        hew_mir::MirDiagnosticKind::UnsupportedUserRecordValueClass { .. } => {
            "UnsupportedUserRecordValueClass"
        }
        hew_mir::MirDiagnosticKind::UnsupportedNode { .. } => "UnsupportedNode",
        hew_mir::MirDiagnosticKind::SelectArmNotImplemented { .. } => "SelectArmNotImplemented",
        hew_mir::MirDiagnosticKind::NotYetImplemented { .. } => "NotYetImplemented",
        hew_mir::MirDiagnosticKind::UnresolvedPlace { .. } => "UnresolvedPlace",
        hew_mir::MirDiagnosticKind::CannotMaterializeClosureCapture { .. } => {
            "CannotMaterializeClosureCapture"
        }
        hew_mir::MirDiagnosticKind::RemotePayloadUnsupported { .. } => "RemotePayloadUnsupported",
        hew_mir::MirDiagnosticKind::DropPlanUndetermined { .. } => "DropPlanUndetermined",
        hew_mir::MirDiagnosticKind::ContextBoundaryViolation { .. } => "ContextBoundaryViolation",
        hew_mir::MirDiagnosticKind::ContextBindingEscapes { .. } => "ContextBindingEscapes",
        hew_mir::MirDiagnosticKind::UnknownActorStateField { .. } => "UnknownActorStateField",
        hew_mir::MirDiagnosticKind::InvalidActorSpawnArgument { .. } => "InvalidActorSpawnArgument",
        hew_mir::MirDiagnosticKind::ActorHandlerSymbolCollision { .. } => {
            "ActorHandlerSymbolCollision"
        }
        hew_mir::MirDiagnosticKind::ActorStateCloneClassificationFailed { .. } => {
            "ActorStateCloneClassificationFailed"
        }
        hew_mir::MirDiagnosticKind::UnresolvedStaticDispatchSubstitution { .. } => {
            "UnresolvedStaticDispatchSubstitution"
        }
        hew_mir::MirDiagnosticKind::StaticDispatchImplNotFound { .. } => {
            "StaticDispatchImplNotFound"
        }
        hew_mir::MirDiagnosticKind::StaticDispatchMonomorphisationMissing { .. } => {
            "StaticDispatchMonomorphisationMissing"
        }
        hew_mir::MirDiagnosticKind::TraitObjectStorageUndetermined { .. } => {
            "TraitObjectStorageUndetermined"
        }
        hew_mir::MirDiagnosticKind::CallTraitMethodSignatureUnresolved { .. } => {
            "CallTraitMethodSignatureUnresolved"
        }
        hew_mir::MirDiagnosticKind::OwnedHandleAggregateExtractionUnsupported { .. } => {
            "OwnedHandleAggregateExtractionUnsupported"
        }
        hew_mir::MirDiagnosticKind::ClosurePairBorrowedStore { .. } => "ClosurePairBorrowedStore",
        hew_mir::MirDiagnosticKind::ClosureCapturesDuplexHandle { .. } => {
            "ClosureCapturesDuplexHandle"
        }
    }
}

fn mir_place_label(place: &hew_mir::Place) -> String {
    match place {
        hew_mir::Place::Local(local) => format!("local {local}"),
        hew_mir::Place::ReturnSlot => "return slot".to_string(),
        hew_mir::Place::DuplexHandle(local) => format!("duplex handle local {local}"),
        hew_mir::Place::LambdaActorHandle(local) => format!("lambda actor handle local {local}"),
        hew_mir::Place::ActorHandle(local) => format!("actor handle local {local}"),
        hew_mir::Place::SendHalf(local) => format!("duplex send-half local {local}"),
        hew_mir::Place::RecvHalf(local) => format!("duplex recv-half local {local}"),
        hew_mir::Place::MachineTag(local) => format!("machine tag local {local}"),
        hew_mir::Place::MachineVariant {
            local,
            variant_idx,
            field_idx,
        } => format!("machine local {local} variant {variant_idx} field {field_idx}"),
        hew_mir::Place::EnumTag(local) => format!("enum tag local {local}"),
        hew_mir::Place::EnumVariant {
            local,
            variant_idx,
            field_idx,
        } => format!("enum local {local} variant {variant_idx} field {field_idx}"),
    }
}

fn mir_primary_site(kind: &hew_mir::MirDiagnosticKind) -> Option<hew_hir::SiteId> {
    match kind {
        hew_mir::MirDiagnosticKind::UseAfterConsume { used_at, .. } => Some(*used_at),
        hew_mir::MirDiagnosticKind::InitialisedBeforeUse { use_site, .. } => Some(*use_site),
        hew_mir::MirDiagnosticKind::DecisionMapTotal { offending_sites } => {
            offending_sites.first().copied()
        }
        hew_mir::MirDiagnosticKind::MustConsume { bind_site, .. } => Some(*bind_site),
        hew_mir::MirDiagnosticKind::SelectArmNotImplemented { site, .. }
        | hew_mir::MirDiagnosticKind::NotYetImplemented { site, .. }
        | hew_mir::MirDiagnosticKind::InvalidActorSpawnArgument { site, .. }
        | hew_mir::MirDiagnosticKind::UnresolvedPlace { site, .. }
        | hew_mir::MirDiagnosticKind::CannotMaterializeClosureCapture { site, .. }
        | hew_mir::MirDiagnosticKind::RemotePayloadUnsupported { site, .. }
        | hew_mir::MirDiagnosticKind::UnresolvedStaticDispatchSubstitution { site, .. }
        | hew_mir::MirDiagnosticKind::StaticDispatchImplNotFound { site, .. }
        | hew_mir::MirDiagnosticKind::StaticDispatchMonomorphisationMissing { site, .. }
        | hew_mir::MirDiagnosticKind::TraitObjectStorageUndetermined { site, .. }
        | hew_mir::MirDiagnosticKind::CallTraitMethodSignatureUnresolved { site, .. }
        | hew_mir::MirDiagnosticKind::ClosurePairBorrowedStore { site, .. }
        | hew_mir::MirDiagnosticKind::ClosureCapturesDuplexHandle { site, .. } => Some(*site),
        hew_mir::MirDiagnosticKind::UnknownType { .. }
        | hew_mir::MirDiagnosticKind::UnsupportedUserRecordValueClass { .. }
        | hew_mir::MirDiagnosticKind::UnsupportedNode { .. }
        | hew_mir::MirDiagnosticKind::DropPlanUndetermined { .. }
        | hew_mir::MirDiagnosticKind::ContextBoundaryViolation { .. }
        | hew_mir::MirDiagnosticKind::ContextBindingEscapes { .. }
        | hew_mir::MirDiagnosticKind::UnknownActorStateField { .. }
        | hew_mir::MirDiagnosticKind::ActorHandlerSymbolCollision { .. }
        | hew_mir::MirDiagnosticKind::ActorStateCloneClassificationFailed { .. }
        | hew_mir::MirDiagnosticKind::OwnedHandleAggregateExtractionUnsupported { .. } => None,
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "exhaustive match over all MIR diagnostic variants"
)]
fn mir_diagnostic_message(diagnostic: &hew_mir::MirDiagnostic) -> String {
    let message = match &diagnostic.kind {
        hew_mir::MirDiagnosticKind::UseAfterConsume { name, .. } => {
            format!("binding `{name}` is used after it was consumed")
        }
        hew_mir::MirDiagnosticKind::InitialisedBeforeUse { name, .. } => {
            format!("binding `{name}` may be read before it is initialized")
        }
        hew_mir::MirDiagnosticKind::DecisionMapTotal { offending_sites } => format!(
            "MIR decision map contains {} unresolved site decision(s)",
            offending_sites.len()
        ),
        hew_mir::MirDiagnosticKind::MustConsume { name, ty, .. } => format!(
            "linear binding `{name}` of type `{}` must be consumed before this exit",
            ty.user_facing()
        ),
        hew_mir::MirDiagnosticKind::UnknownType { name } => {
            format!("unknown type `{name}` at the MIR boundary")
        }
        hew_mir::MirDiagnosticKind::UnsupportedUserRecordValueClass { name, .. } => format!(
            "record type `{name}` has a value class that MIR cannot lower yet"
        ),
        hew_mir::MirDiagnosticKind::UnsupportedNode { reason } => {
            format!("unsupported HIR node reached MIR lowering: {reason}")
        }
        hew_mir::MirDiagnosticKind::SelectArmNotImplemented {
            arm_kind,
            deferred_by,
            ..
        } => format!("select arm `{arm_kind}` is not implemented in MIR yet ({deferred_by})"),
        hew_mir::MirDiagnosticKind::NotYetImplemented { construct, .. } => {
            format!("MIR lowering for {construct} is not implemented yet")
        }
        hew_mir::MirDiagnosticKind::UnresolvedPlace { name, .. } => {
            format!("could not resolve binding `{name}` to an MIR place")
        }
        hew_mir::MirDiagnosticKind::CannotMaterializeClosureCapture { name, .. } => {
            format!("could not materialize closure capture `{name}` in MIR")
        }
        hew_mir::MirDiagnosticKind::RemotePayloadUnsupported { actor, handler, .. } => format!(
            "remote dispatch to multi-parameter receive fn `{handler}` on actor `{actor}` \
             is not supported: the cross-node codec carries single-value payloads only"
        ),
        hew_mir::MirDiagnosticKind::DropPlanUndetermined { block, reason } => {
            format!("drop plan for MIR block {block} could not be determined: {reason}")
        }
        hew_mir::MirDiagnosticKind::ContextBoundaryViolation {
            function,
            kind,
            reason,
            ..
        } => {
            format!("context boundary violation in `{function}` ({kind}): {reason}")
        }
        hew_mir::MirDiagnosticKind::ContextBindingEscapes { place, block } => format!(
            "context-bound place `{}` escapes from MIR block {block}",
            mir_place_label(place)
        ),
        hew_mir::MirDiagnosticKind::UnknownActorStateField { actor, field } => {
            format!("actor `{actor}` has no state field `{field}`")
        }
        hew_mir::MirDiagnosticKind::InvalidActorSpawnArgument {
            actor, argument, ..
        } => {
            format!("invalid spawn argument `{argument}` for actor `{actor}`")
        }
        hew_mir::MirDiagnosticKind::ActorHandlerSymbolCollision {
            symbol,
            existing,
            duplicate,
        } => format!(
            "actor handler symbol `{symbol}` is produced by both {existing} and {duplicate}"
        ),
        hew_mir::MirDiagnosticKind::ActorStateCloneClassificationFailed {
            actor,
            field_name,
            reason,
            ..
        } => format!(
            "could not classify actor `{actor}` state field `{field_name}` for clone lowering: {reason}"
        ),
        hew_mir::MirDiagnosticKind::UnresolvedStaticDispatchSubstitution {
            receiver_type_param,
            declaring_trait,
            method_name,
            ..
        } => format!(
            "static dispatch for `{declaring_trait}.{method_name}` still references unresolved type parameter `{receiver_type_param}`"
        ),
        hew_mir::MirDiagnosticKind::StaticDispatchImplNotFound {
            declaring_trait,
            self_type_name,
            method_name,
            ..
        } => format!(
            "no implementation found for static dispatch `{declaring_trait}.{method_name}` on `{self_type_name}`"
        ),
        hew_mir::MirDiagnosticKind::StaticDispatchMonomorphisationMissing {
            method_symbol,
            mangled,
            ..
        } => format!(
            "static dispatch target `{method_symbol}` was not monomorphized as `{mangled}`"
        ),
        hew_mir::MirDiagnosticKind::TraitObjectStorageUndetermined { name, reason, .. } => {
            format!(
                "cannot determine TraitObjectStorage for dyn Trait binding `{name}` ({reason})"
            )
        }
        hew_mir::MirDiagnosticKind::CallTraitMethodSignatureUnresolved {
            trait_name,
            method_name,
            reason,
            ..
        } => format!(
            "dyn-trait method dispatch `{trait_name}::{method_name}` reached MIR with an \
             unresolved caller-side signature ({reason})"
        ),
        hew_mir::MirDiagnosticKind::OwnedHandleAggregateExtractionUnsupported {
            name,
            handle_ty,
        } => format!(
            "extracting an owned handle (`{name}`: {handle_ty}) out of an aggregate in this \
             shape is not yet supported in v0.5 — iterate or consume the handle directly, or \
             return it without re-aggregating (full support lands in v0.5.1)"
        ),
        hew_mir::MirDiagnosticKind::ClosurePairBorrowedStore { name, .. } => {
            let what = name.as_ref().map_or_else(
                || "this function value".to_string(),
                |n| format!("the function value `{n}`"),
            );
            format!(
                "cannot store {what} here: it borrows a closure environment owned \
                 elsewhere (a parameter, collection element, or record-field read), \
                 and storing it would create a second owner of that environment — \
                 store a closure literal, a fresh call result, or a binding that \
                 owns its closure instead"
            )
        }
        hew_mir::MirDiagnosticKind::ClosureCapturesDuplexHandle { name, .. } => format!(
            "defence-in-depth: fn-closure capture env contains a Duplex handle `{name}` — \
             no env-materialization protocol exists; checker gate in `check_call` \
             must have been bypassed by a new source form \
             (E_CLOSURE_CAPTURES_LAMBDA_HANDLE)"
        ),
    };
    format!("{}: {message}", mir_diagnostic_prefix(&diagnostic.kind))
}

fn mir_context_notes(diagnostic: &hew_mir::MirDiagnostic) -> Vec<String> {
    let mut notes = Vec::new();
    notes.push(format!("MIR kind: {}", mir_kind_name(&diagnostic.kind)));
    match &diagnostic.kind {
        hew_mir::MirDiagnosticKind::UseAfterConsume {
            binding,
            consumed_at,
            used_at,
            ..
        } => {
            notes.push(format!("binding id: {binding}"));
            notes.push(format!("consumed at site: {consumed_at}"));
            notes.push(format!("used at site: {used_at}"));
        }
        hew_mir::MirDiagnosticKind::InitialisedBeforeUse {
            binding, use_site, ..
        } => {
            notes.push(format!("binding id: {binding}"));
            notes.push(format!("use site: {use_site}"));
        }
        hew_mir::MirDiagnosticKind::DecisionMapTotal { offending_sites } => {
            let sites = offending_sites
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ");
            notes.push(format!("offending sites: {sites}"));
        }
        hew_mir::MirDiagnosticKind::MustConsume {
            binding, exit_site, ..
        } => {
            notes.push(format!("binding id: {binding}"));
            notes.push(format!("exit site: {exit_site}"));
        }
        hew_mir::MirDiagnosticKind::UnsupportedUserRecordValueClass { reason, .. } => {
            notes.push(format!("reason: {reason}"));
        }
        hew_mir::MirDiagnosticKind::SelectArmNotImplemented { site, .. }
        | hew_mir::MirDiagnosticKind::NotYetImplemented { site, .. }
        | hew_mir::MirDiagnosticKind::InvalidActorSpawnArgument { site, .. }
        | hew_mir::MirDiagnosticKind::UnresolvedPlace { site, .. }
        | hew_mir::MirDiagnosticKind::CannotMaterializeClosureCapture { site, .. }
        | hew_mir::MirDiagnosticKind::RemotePayloadUnsupported { site, .. }
        | hew_mir::MirDiagnosticKind::UnresolvedStaticDispatchSubstitution { site, .. }
        | hew_mir::MirDiagnosticKind::StaticDispatchImplNotFound { site, .. }
        | hew_mir::MirDiagnosticKind::StaticDispatchMonomorphisationMissing { site, .. }
        | hew_mir::MirDiagnosticKind::ClosurePairBorrowedStore { site, .. }
        | hew_mir::MirDiagnosticKind::ClosureCapturesDuplexHandle { site, .. } => {
            notes.push(format!("site: {site}"));
        }
        hew_mir::MirDiagnosticKind::DropPlanUndetermined { block, .. }
        | hew_mir::MirDiagnosticKind::ContextBoundaryViolation { block, .. }
        | hew_mir::MirDiagnosticKind::ContextBindingEscapes { block, .. } => {
            notes.push(format!("block: {block}"));
        }
        hew_mir::MirDiagnosticKind::ActorStateCloneClassificationFailed { field_index, .. } => {
            notes.push(format!("field index: {field_index}"));
        }
        hew_mir::MirDiagnosticKind::UnknownType { .. }
        | hew_mir::MirDiagnosticKind::UnsupportedNode { .. }
        | hew_mir::MirDiagnosticKind::UnknownActorStateField { .. }
        | hew_mir::MirDiagnosticKind::ActorHandlerSymbolCollision { .. }
        | hew_mir::MirDiagnosticKind::TraitObjectStorageUndetermined { .. }
        | hew_mir::MirDiagnosticKind::CallTraitMethodSignatureUnresolved { .. } => {}
        hew_mir::MirDiagnosticKind::OwnedHandleAggregateExtractionUnsupported {
            handle_ty, ..
        } => {
            notes.push(format!("handle type: {handle_ty}"));
        }
    }
    if !diagnostic.note.is_empty() {
        notes.push(diagnostic.note.clone());
    }
    notes
}

fn site_source<'a>(
    root_source: &'a str,
    root_filename: &'a str,
    module_source_map: &'a ModuleSourceMap,
    site: &hew_hir::HirSiteSource,
) -> Option<(&'a str, &'a str)> {
    match site.source_module.as_deref() {
        None => Some((root_source, root_filename)),
        Some(module) => module_source_map
            .get(module)
            .map(|(source, filename)| (source.as_str(), filename.as_str())),
    }
}

fn mir_source_context_unavailable_note(site: &hew_hir::HirSiteSource) -> String {
    site.source_module.as_ref().map_or_else(
        || ROOT_SOURCE_CONTEXT_UNAVAILABLE.to_string(),
        |module| format!("source context unavailable: module '{module}' not in module_source_map"),
    )
}

fn mir_secondary_spans(
    diagnostic: &hew_mir::MirDiagnostic,
    primary_site: &hew_hir::HirSiteSource,
    site_spans: &HashMap<hew_hir::SiteId, hew_hir::HirSiteSource>,
) -> Vec<(Range<usize>, String)> {
    let mut spans = Vec::new();
    match &diagnostic.kind {
        hew_mir::MirDiagnosticKind::UseAfterConsume { consumed_at, .. } => {
            if let Some(site) = site_spans.get(consumed_at) {
                if site.source_module == primary_site.source_module {
                    spans.push((site.span.clone(), "binding consumed here".to_string()));
                }
            }
        }
        hew_mir::MirDiagnosticKind::DecisionMapTotal { offending_sites } => {
            for site_id in offending_sites.iter().skip(1) {
                if let Some(site) = site_spans.get(site_id) {
                    if site.source_module == primary_site.source_module {
                        spans.push((site.span.clone(), format!("offending MIR site {site_id}")));
                    }
                }
            }
        }
        _ => {}
    }
    spans
}

fn render_mir_diagnostic_without_source(
    diagnostic: &hew_mir::MirDiagnostic,
    site: Option<&hew_hir::HirSiteSource>,
) {
    emit_plain_diagnostic_line(&format!("error: {}", mir_diagnostic_message(diagnostic)));
    for note in mir_context_notes(diagnostic) {
        emit_plain_diagnostic_line(&format!("  = note: {note}"));
    }
    if let Some(site) = site {
        emit_plain_diagnostic_line(&format!(
            "  = note: {}",
            mir_source_context_unavailable_note(site)
        ));
    }
}

/// Render MIR diagnostics for the deep gates without exposing Rust `Debug`
/// payloads.
///
/// Routes through the JSON sink when `--format=json` is active so MIR
/// diagnostics participate in machine-readable output identically to frontend
/// diagnostics. The user-facing message is always the `mir_diagnostic_message`
/// rendering — never the raw `MirDiagnostic` `{:?}` payload.
pub(crate) fn render_mir_diagnostics(
    root_source: &str,
    root_filename: &str,
    module_source_map: &ModuleSourceMap,
    site_spans: &HashMap<hew_hir::SiteId, hew_hir::HirSiteSource>,
    diagnostics: &[hew_mir::MirDiagnostic],
) {
    let json = crate::diagnostic_json::json_output_active();
    for diagnostic in diagnostics {
        let primary_site = mir_primary_site(&diagnostic.kind)
            .and_then(|site| site_spans.get(&site).map(|source| (site, source)));
        let Some((_, site)) = primary_site else {
            if json {
                push_mir_json_diagnostic(diagnostic, None, None, &[]);
            } else {
                render_mir_diagnostic_without_source(diagnostic, None);
            }
            continue;
        };
        let Some((source, filename)) =
            site_source(root_source, root_filename, module_source_map, site)
        else {
            if json {
                push_mir_json_diagnostic(diagnostic, None, None, &[]);
            } else {
                render_mir_diagnostic_without_source(diagnostic, Some(site));
            }
            continue;
        };
        let secondary_spans = mir_secondary_spans(diagnostic, site, site_spans);
        let suggestions = mir_context_notes(diagnostic);
        if json {
            push_mir_json_diagnostic(
                diagnostic,
                Some((source, filename)),
                Some(&site.span),
                &secondary_spans,
            );
        } else {
            render_diagnostic_with_raw_notes(
                source,
                filename,
                &site.span,
                &mir_diagnostic_message(diagnostic),
                &secondary_spans,
                &suggestions,
            );
        }
    }
}

/// Push a MIR diagnostic into the JSON sink, stripping the `E_*` prefix from
/// the message (the prefix lives in the `code`/`source` fields in JSON).
fn push_mir_json_diagnostic(
    diagnostic: &hew_mir::MirDiagnostic,
    source_and_filename: Option<(&str, &str)>,
    span: Option<&Range<usize>>,
    secondary_spans: &[(Range<usize>, String)],
) {
    let prefixed = mir_diagnostic_message(diagnostic);
    // `mir_diagnostic_message` prepends `E_MIR: ` / `E_NOT_YET_IMPLEMENTED: `;
    // strip it so the JSON `message` is clean prose and the family lives in
    // `code`.
    let message = prefixed
        .split_once(": ")
        .map_or(prefixed.as_str(), |(_, rest)| rest);
    let (source, filename) = match source_and_filename {
        Some((source, filename)) => (Some(source), Some(filename)),
        None => (None, None),
    };
    crate::diagnostic_json::push_json_diagnostic(crate::diagnostic_json::from_mir_diagnostic(
        source,
        filename,
        span,
        mir_kind_name(&diagnostic.kind),
        message,
        secondary_spans,
        &mir_context_notes(diagnostic),
    ));
}

fn type_diagnostic_source<'a>(
    root_source: &'a str,
    root_filename: &'a str,
    diagnostic: &hew_types::TypeError,
    module_source_map: &'a ModuleSourceMap,
) -> (&'a str, &'a str) {
    if let Some(ref mod_name) = diagnostic.source_module {
        if let Some((mod_src, mod_file)) = module_source_map.get(mod_name.as_str()) {
            return (mod_src.as_str(), mod_file.as_str());
        }
    }
    (root_source, root_filename)
}

/// Render parser diagnostics using the shared CLI diagnostic layout.
pub fn render_parse_diagnostics(source: &str, filename: &str, errors: &[hew_parser::ParseError]) {
    for err in errors {
        let hints: Vec<String> = err.hint.iter().cloned().collect();
        match err.severity {
            hew_parser::Severity::Warning => {
                render_warning(source, filename, &err.span, &err.message, &[], &hints);
            }
            hew_parser::Severity::Error => {
                render_diagnostic(source, filename, &err.span, &err.message, &[], &hints);
            }
        }
    }
}

/// Render type-check diagnostics using the shared CLI diagnostic layout.
pub fn render_type_diagnostics(source: &str, filename: &str, diagnostics: &[hew_types::TypeError]) {
    let module_source_map = ModuleSourceMap::new();
    render_type_diagnostics_with_sources(source, filename, diagnostics, &module_source_map);
}

/// Render type-check diagnostics, routing non-root diagnostics to their source
/// modules when `source_module` attribution is available.
pub(crate) fn render_type_diagnostics_with_sources(
    root_source: &str,
    root_filename: &str,
    diagnostics: &[hew_types::TypeError],
    module_source_map: &ModuleSourceMap,
) {
    for diagnostic in diagnostics {
        let (source, filename) =
            type_diagnostic_source(root_source, root_filename, diagnostic, module_source_map);
        match diagnostic.severity {
            hew_types::error::Severity::Warning => render_warning_with_raw_notes(
                source,
                filename,
                &diagnostic.span,
                &diagnostic.message,
                &diagnostic.notes,
                &diagnostic.suggestions,
            ),
            hew_types::error::Severity::Error => render_diagnostic_with_raw_notes(
                source,
                filename,
                &diagnostic.span,
                &diagnostic.message,
                &diagnostic.notes,
                &diagnostic.suggestions,
            ),
        }
    }
}

fn hir_source_context_unavailable_note(diagnostic: &hew_hir::HirDiagnostic) -> String {
    diagnostic.source_module.as_ref().map_or_else(
        || ROOT_SOURCE_CONTEXT_UNAVAILABLE.to_string(),
        |module| format!("source context unavailable: module '{module}' not in module_source_map"),
    )
}

fn hir_diagnostic_message(diagnostic: &hew_hir::HirDiagnostic) -> String {
    // For a `NotYetImplemented` gap, keep the stable `E_NOT_YET_IMPLEMENTED`
    // family code (grep-able by tooling) but use the limitation-framed body and
    // never leak the Rust `{:?}` field names. Other kinds fall back to the
    // stable prefix plus any attached note.
    if matches!(
        diagnostic.kind,
        hew_hir::HirDiagnosticKind::NotYetImplemented { .. }
    ) {
        return format!(
            "{}: {}",
            hir_diagnostic_prefix(&diagnostic.kind),
            hir_diagnostic_user_message(diagnostic)
        );
    }
    let prefix = hir_diagnostic_prefix(&diagnostic.kind);
    if diagnostic.note.is_empty() {
        prefix.to_string()
    } else {
        format!("{prefix}: {}", diagnostic.note)
    }
}

/// Render a HIR diagnostic using source context when the frontend was able to
/// resolve the diagnostic's source module. Non-root source-map misses are
/// rendered explicitly rather than falling back to the root file.
///
/// Routes through the JSON sink when `--format=json` is active. The `HIR kind`
/// note uses the stable kind string (never the Rust `{:?}` struct payload), so
/// no Debug payload reaches user output on either path.
pub(crate) fn render_hir_diagnostic(
    source: Option<&str>,
    filename: Option<&str>,
    diagnostic: &hew_hir::HirDiagnostic,
) {
    if crate::diagnostic_json::json_output_active() {
        crate::diagnostic_json::push_json_diagnostic(crate::diagnostic_json::from_hir_diagnostic(
            source, filename, diagnostic,
        ));
        return;
    }

    let message = hir_diagnostic_message(diagnostic);
    let kind_note = format!("HIR kind: {}", hir_diagnostic_kind_string(&diagnostic.kind));
    if let (Some(source), Some(filename)) = (source, filename) {
        let suggestions = [kind_note];
        render_diagnostic_with_raw_notes(
            source,
            filename,
            &diagnostic.span,
            &message,
            &diagnostic.secondary_spans,
            &suggestions,
        );
        return;
    }

    emit_plain_diagnostic_line(&format!("error: {message}"));
    emit_plain_diagnostic_line(&format!("  = note: {kind_note}"));
    emit_plain_diagnostic_line(&format!(
        "  = note: {}",
        hir_source_context_unavailable_note(diagnostic)
    ));
    for (_, label) in &diagnostic.secondary_spans {
        emit_plain_diagnostic_line(&format!("  = note: {label}"));
    }
}

/// Render the source line and `^^^` underline for a span.
fn render_source_underline(
    source: &str,
    span: &Range<usize>,
    line: usize,
    palette: &DiagnosticPalette,
) {
    let lines: Vec<&str> = source.lines().collect();

    if line == 0 {
        return;
    }

    // Handle EOF / empty file — show a marker line.
    if line > lines.len() {
        let line_num = line.to_string();
        let padding = " ".repeat(line_num.len());
        diag_println(&format!(
            " {blue}{line_num} |{reset}",
            blue = palette.blue,
            reset = palette.reset,
        ));
        diag_println(&format!(
            " {padding} {blue}|{reset} {red}^{reset}",
            blue = palette.blue,
            red = palette.red,
            reset = palette.reset,
        ));
        return;
    }

    let source_line = lines[line - 1];
    // Strip trailing \r for CRLF files so underline widths align with display.
    let display_line = source_line.strip_suffix('\r').unwrap_or(source_line);
    let line_num = line.to_string();
    let padding = " ".repeat(line_num.len());

    // Print the source line
    diag_println(&format!(
        " {blue}{line_num} |{reset} {display_line}",
        blue = palette.blue,
        reset = palette.reset,
    ));

    // Compute underline position within the line using character counts,
    // not byte offsets, so multi-byte UTF-8 characters align correctly.
    let line_start = line_start_offset(source, line);
    let start_byte = span
        .start
        .saturating_sub(line_start)
        .min(display_line.len());
    let start_chars = display_line
        .get(..start_byte)
        .map_or(0, |s| s.chars().count());

    // For multi-line spans, underline to end of first line; for empty spans, show one caret.
    let end_byte = if span.start == span.end {
        start_byte + 1
    } else {
        let line_end = line_start + display_line.len();
        span.end.min(line_end).saturating_sub(line_start)
    };
    let end_chars = display_line
        .get(..end_byte.min(display_line.len()))
        .map_or(start_chars + 1, |s| s.chars().count());

    let underline_len = end_chars.saturating_sub(start_chars).max(1);

    diag_println(&format!(
        " {padding} {blue}|{reset} {}{red}{}{reset}",
        " ".repeat(start_chars),
        "^".repeat(underline_len),
        blue = palette.blue,
        red = palette.red,
        reset = palette.reset,
    ));
}

/// Convert a byte offset to a 1-based (line, column) pair.
/// Handles both `\n` and `\r\n` line endings; `\r` before `\n` is not counted
/// as a column so that the column number matches the displayed line content.
pub(crate) fn offset_to_line_col(source: &str, offset: usize) -> (usize, usize) {
    let offset = offset.min(source.len());
    let mut line = 1;
    let mut col = 1;
    let bytes = source.as_bytes();

    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else if ch == '\r' && bytes.get(i + 1) == Some(&b'\n') {
            // Skip \r in a \r\n pair — the \n will advance the line.
        } else {
            col += 1;
        }
    }

    (line, col)
}

/// Return the byte offset of the start of a 1-based line number.
fn line_start_offset(source: &str, line: usize) -> usize {
    let mut current_line = 1;
    for (i, ch) in source.char_indices() {
        if current_line == line {
            return i;
        }
        if ch == '\n' {
            current_line += 1;
        }
    }
    // Past end — return source length
    source.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_type_error() -> hew_types::TypeError {
        hew_types::TypeError::new(
            hew_types::error::TypeErrorKind::UndefinedFunction,
            0..4,
            "cannot find function `oops` in this scope",
        )
    }

    #[test]
    fn type_diagnostic_source_prefers_attributed_module_source() {
        let mut diagnostic = sample_type_error();
        diagnostic.source_module = Some("dep".to_string());

        let mut module_source_map = ModuleSourceMap::new();
        module_source_map.insert(
            "dep".to_string(),
            ("pub fn oops() {}\n".to_string(), "dep.hew".to_string()),
        );

        let (source, filename) = type_diagnostic_source(
            "fn main() {}\n",
            "main.hew",
            &diagnostic,
            &module_source_map,
        );

        assert_eq!(source, "pub fn oops() {}\n");
        assert_eq!(filename, "dep.hew");
    }

    #[test]
    fn type_diagnostic_source_falls_back_to_root_when_module_missing() {
        let mut diagnostic = sample_type_error();
        diagnostic.source_module = Some("dep".to_string());
        let module_source_map = ModuleSourceMap::new();

        let (source, filename) = type_diagnostic_source(
            "fn main() {}\n",
            "main.hew",
            &diagnostic,
            &module_source_map,
        );

        assert_eq!(source, "fn main() {}\n");
        assert_eq!(filename, "main.hew");
    }

    #[test]
    fn captured_diagnostics_strip_ansi_sequences() {
        start_diagnostic_capture();
        render_diagnostic("oops()\n", "main.hew", &(0..4), "bad call", &[], &[]);
        let captured = finish_diagnostic_capture();

        assert!(
            !captured.contains("\u{1b}["),
            "captured diagnostics must not contain ANSI escapes: {captured:?}"
        );
        assert!(captured.contains("main.hew:1:1: error: bad call"));
    }

    #[test]
    fn hir_source_map_miss_reports_unavailable_note() {
        let diagnostic = hew_hir::HirDiagnostic::new(
            hew_hir::HirDiagnosticKind::UnresolvedInferenceVar,
            0..1,
            "probe",
        )
        .with_source_module(Some("dep".to_string()));

        start_diagnostic_capture();
        render_hir_diagnostic(None, None, &diagnostic);
        let captured = finish_diagnostic_capture();

        assert!(captured.contains("error: E_HIR: probe"));
        assert!(captured.contains("HIR kind: UnresolvedInferenceVar"));
        assert!(
            captured.contains("source context unavailable: module 'dep' not in module_source_map")
        );
    }

    #[test]
    fn hir_secondary_spans_use_primary_source_context() {
        let diagnostic = hew_hir::HirDiagnostic::new(
            hew_hir::HirDiagnosticKind::UnresolvedInferenceVar,
            0..4,
            "primary",
        )
        .with_secondary_spans(vec![(
            5..9,
            "secondary uses primary source module".to_string(),
        )]);

        start_diagnostic_capture();
        render_hir_diagnostic(Some("abcd\nefgh\n"), Some("dep.hew"), &diagnostic);
        let captured = finish_diagnostic_capture();

        assert!(captured.contains("dep.hew:1:1: error: E_HIR: primary"));
        assert!(captured.contains("dep.hew:2:1: note: secondary uses primary source module"));
        assert!(captured.contains("efgh"));
    }

    #[test]
    fn ansi_output_requires_tty_without_no_color_or_capture() {
        assert!(should_use_ansi_output(true, false, false));
        assert!(!should_use_ansi_output(false, false, false));
        assert!(!should_use_ansi_output(true, true, false));
        assert!(!should_use_ansi_output(true, false, true));
    }
}

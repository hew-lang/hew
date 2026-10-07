//! The checker-driven failure-edge pass of `hew fmt --migrate` (D578).
//!
//! `?` and `return error` leave a callable only through its declared failure
//! edge, `-> T fails E`. A `-> Result<T, E>` return is a value: its body
//! produces the `Result` itself. A callable written to the retired rules — a
//! `-> Result` body that uses `?` on a `Result` or `return error`, or whose
//! tail relied on the compiler wrapping a success value — moves to the edge
//! form:
//!
//! | value body                              | edge body          |
//! |-----------------------------------------|--------------------|
//! | `-> Result<T, E>` / `-> Result<(), E>`  | `-> T fails E` / `fails E` |
//! | `.Ok(v)` at a tail or `return`          | `v`                |
//! | `.Err(e)` at a tail or `return`         | `return error e`   |
//! | another `Result` at a tail or `return`  | `r?`               |
//!
//! A closure with no declared return whose body uses `?` on a `Result` or
//! `return error` takes its edge from them, so only its exits are rewritten;
//! a `?` on an `Option` is no failure exit. A `-> Result` callable with
//! neither is a value return and is left alone. Types come from the checker;
//! a file keeps its rewrite only when the re-check reports no diagnostic it
//! did not already have and no failure exit still lacking its edge.

use std::collections::HashMap;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;

use hew_compile::{DocumentSet, FrontendDiagnosticKind, FrontendOptions};
use hew_parser::ast::{
    Block, CallArg, Expr, Item, Span, Spanned, Stmt, TraitItem, TypeBodyItem, TypeExpr,
};
use hew_types::check::scope::Resolution;
use hew_types::check::TypeCheckOutput;
use hew_types::error::TypeErrorKind;
use hew_types::{ActorMethodKind, BuiltinType, NodeVisitor, SpanKey, Ty};

/// A file the pass left unconverted, or a converted handler whose callers
/// need review, at its first location.
pub(crate) struct EdgeReport {
    pub file: PathBuf,
    pub line: usize,
    pub column: usize,
    pub message: String,
    /// For a converted handler, every caller that may match its old reply.
    pub callers: Option<Vec<Location>>,
}

/// Convert the failure edges of every file in `files` (path, migrated text),
/// in place. Returns the refusals and the converted handlers to review.
///
/// Converting a callable keeps its type, except a `receive fn`'s reply: its
/// callers are reported, never rewritten. So each check unit — a directory
/// module, or a file of its own — is checked against the other units'
/// unconverted text, and units convert in parallel.
pub(crate) fn convert_failure_edges(
    files: &mut [(PathBuf, String)],
) -> (Vec<EdgeReport>, Vec<EdgeReport>) {
    let mut documents = DocumentSet::new();
    let mut units: Vec<(PathBuf, Vec<usize>)> = Vec::new();
    for (index, (path, text)) in files.iter().enumerate() {
        documents.insert(path.clone(), text.clone());
        let root = hew_compile::directory_module_entry(path).unwrap_or_else(|| path.clone());
        match units.iter_mut().find(|(known, _)| *known == root) {
            Some((_, members)) => members.push(index),
            None => units.push((root, vec![index])),
        }
    }
    let next = AtomicUsize::new(0);
    let outcomes: Vec<Mutex<UnitOutcome>> = units
        .iter()
        .map(|_| Mutex::new(UnitOutcome::default()))
        .collect();
    let workers = std::thread::available_parallelism()
        .map_or(1, std::num::NonZeroUsize::get)
        .min(units.len());
    std::thread::scope(|scope| {
        for _ in 0..workers {
            scope.spawn(|| loop {
                let unit = next.fetch_add(1, Ordering::Relaxed);
                let Some((root, members)) = units.get(unit) else {
                    break;
                };
                let members: Vec<_> = members
                    .iter()
                    .map(|&index| (index, files[index].0.as_path(), files[index].1.as_str()))
                    .collect();
                let outcome = convert_unit(root, &members, &documents);
                *outcomes[unit]
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner) = outcome;
            });
        }
    });
    let mut refusals = Vec::new();
    let mut handlers = Vec::new();
    let mut notes = Vec::new();
    let mut asks: HashMap<HandlerSite, Vec<Location>> = HashMap::new();
    for outcome in outcomes {
        let outcome = outcome
            .into_inner()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        for (index, converted) in outcome.converted {
            files[index].1 = converted;
        }
        refusals.extend(outcome.refusals);
        notes.extend(outcome.notes);
        handlers.extend(outcome.handlers);
        for (site, callers) in outcome.asks {
            asks.entry(site).or_default().extend(callers);
        }
    }
    notes.extend(handlers.into_iter().map(|handler| {
        let callers = asks.get(&handler.site).map_or(&[][..], Vec::as_slice);
        handler.notice(callers)
    }));
    (refusals, notes)
}

/// What converting one check unit produced.
#[derive(Default)]
struct UnitOutcome {
    /// Members that converted cleanly, by index in the caller's files.
    converted: Vec<(usize, String)>,
    refusals: Vec<EdgeReport>,
    /// Converted `receive fn`s.
    handlers: Vec<ConvertedHandler>,
    /// Every ask the unit makes of a `receive fn`, by the handler's site.
    asks: HashMap<HandlerSite, Vec<Location>>,
    /// Members the pass could not check.
    notes: Vec<EdgeReport>,
}

/// A `receive fn` by where it is declared: its file, where its actor's item
/// starts and its index among the actor's handlers. One site names one
/// handler in every unit's check.
#[derive(Clone, PartialEq, Eq, Hash)]
struct HandlerSite {
    file: PathBuf,
    actor: usize,
    index: u32,
}

/// A source location, one-based.
#[derive(Clone)]
pub(crate) struct Location {
    pub file: PathBuf,
    pub line: usize,
    pub column: usize,
}

/// A `receive fn` the pass converted: its reply moves from `.Ok(.Err(e))` to
/// `.Err(.Failed(e))`, so every caller that matched the old envelope needs
/// review.
struct ConvertedHandler {
    site: HandlerSite,
    name: String,
    at: Location,
}

impl ConvertedHandler {
    fn notice(self, callers: &[Location]) -> EdgeReport {
        EdgeReport {
            file: self.at.file,
            line: self.at.line,
            column: self.at.column,
            message: format!(
                "`receive fn {}` now fails through its edge: a caller that matched \
                 `.Ok(.Err(e))` receives `.Err(.Failed(e))`",
                self.name
            ),
            callers: Some(callers.to_vec()),
        }
    }
}

/// Convert the members of one check unit. A member keeps its conversion only
/// when the re-check adds no diagnostic to it and leaves no failure exit
/// without its edge.
fn convert_unit(
    root: &Path,
    members: &[(usize, &Path, &str)],
    documents: &DocumentSet,
) -> UnitOutcome {
    let mut documents = documents.clone();
    let mut outcome = UnitOutcome::default();
    let before = check_unit(root, &documents);
    outcome.asks = before.asks(&documents);
    let mut candidates = Vec::new();
    for &(index, path, text) in members {
        let Some(file) = before.file_index(path, root) else {
            // A module that did not load names none of its files; the
            // syntax migration still applies to them.
            outcome.notes.push(EdgeReport {
                file: path.to_path_buf(),
                line: 1,
                column: 1,
                message: "its failure edges were not converted: its module did not load"
                    .to_string(),
                callers: None,
            });
            continue;
        };
        let plan = plan_file(text, file, &before);
        if plan.edits.is_empty() {
            continue;
        }
        let rewrite = match render(text, plan.edits) {
            Ok(rewrite) => rewrite,
            Err(offset) => {
                let (line, column) = crate::diagnostic::offset_to_line_col(text, offset);
                outcome.refusals.push(EdgeReport {
                    file: path.to_path_buf(),
                    line,
                    column,
                    message: "its failure-edge rewrites overlap here; convert this callable \
                              by hand"
                        .to_string(),
                    callers: None,
                });
                continue;
            }
        };
        documents.insert(path.to_path_buf(), rewrite.text.clone());
        let canonical = std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf());
        let handlers = plan
            .handlers
            .into_iter()
            .map(|handler| {
                let (line, column) = crate::diagnostic::offset_to_line_col(text, handler.offset);
                ConvertedHandler {
                    site: HandlerSite {
                        file: canonical.clone(),
                        actor: handler.actor,
                        index: handler.index,
                    },
                    name: handler.name,
                    at: Location {
                        file: path.to_path_buf(),
                        line,
                        column,
                    },
                }
            })
            .collect::<Vec<_>>();
        candidates.push((index, path, rewrite, handlers));
    }
    if candidates.is_empty() {
        return outcome;
    }
    let after = check_unit(root, &documents);
    for (index, path, rewrite, handlers) in candidates {
        if let Some(diagnostic) = first_new_diagnostic(
            &before.reported_in(path),
            &after.reported_in(path),
            &rewrite,
        ) {
            let text = members
                .iter()
                .find(|(member, _, _)| *member == index)
                .map_or("", |(_, _, text)| text);
            let (line, column) = crate::diagnostic::offset_to_line_col(
                text,
                rewrite.original_offset(diagnostic.span.start),
            );
            outcome.refusals.push(EdgeReport {
                file: path.to_path_buf(),
                line,
                column,
                message: format!(
                    "converting its failure edges would leave `{}`: {}",
                    diagnostic.code, diagnostic.message
                ),
                callers: None,
            });
        } else {
            outcome.handlers.extend(handlers);
            outcome.converted.push((index, rewrite.text));
        }
    }
    outcome
}

/// One checked unit: its diagnostics and the checker facts the plan reads.
struct UnitCheck {
    diagnostics: Vec<(Option<PathBuf>, FileDiagnostic)>,
    output: Option<TypeCheckOutput>,
    indices: hew_parser::module::FileSpanIndices,
    /// Each checked source file by its span-key index.
    sources: HashMap<u32, PathBuf>,
}

#[derive(Clone)]
struct FileDiagnostic {
    /// The stage and kind that reported it, as the diagnostic code spells it.
    code: String,
    message: String,
    span: Range<usize>,
}

impl FileDiagnostic {
    fn is_missing_edge(&self) -> bool {
        self.code == TypeErrorKind::NoFailureEdge.as_kind_str()
    }
}

fn check_unit(root: &Path, documents: &DocumentSet) -> UnitCheck {
    let options = FrontendOptions {
        documents: documents.clone(),
        ..FrontendOptions::default()
    };
    let state = hew_compile::run_document_frontend(&root.display().to_string(), &options);
    let indices = state
        .program
        .module_graph
        .as_ref()
        .map(hew_parser::module::ModuleGraph::file_span_indices)
        .unwrap_or_default();
    let sources = state
        .program
        .module_graph
        .iter()
        .flat_map(|graph| graph.modules.values())
        .flat_map(|module| module.source_paths.iter())
        .filter_map(|path| Some((indices.path_index(path)?, path.clone())))
        .collect::<HashMap<_, _>>();
    // The root compilation unit is index 0 whether or not its graph module
    // lists its source.
    let mut sources = sources;
    if let Ok(root) = std::fs::canonicalize(root) {
        sources.entry(0).or_insert(root);
    }
    let canonical = |file: Option<String>| file.and_then(|file| std::fs::canonicalize(file).ok());
    let mut diagnostics: Vec<(Option<PathBuf>, FileDiagnostic)> = state
        .diagnostics
        .into_iter()
        .filter_map(|diagnostic| {
            let file = canonical(diagnostic.filename);
            let reported = match diagnostic.kind {
                FrontendDiagnosticKind::Type(error) => FileDiagnostic {
                    code: error.kind.as_kind_str().to_string(),
                    message: error.message,
                    span: error.span,
                },
                FrontendDiagnosticKind::Parse(error) => FileDiagnostic {
                    code: format!("parse {:?}", error.kind),
                    message: error.message,
                    span: error.span,
                },
                FrontendDiagnosticKind::Message(message) => FileDiagnostic {
                    code: message.code,
                    message: message.message,
                    span: message.span?,
                },
                FrontendDiagnosticKind::Hir(error) => FileDiagnostic {
                    code: format!("hir {:?}", error.kind),
                    message: error.note,
                    span: error.span,
                },
            };
            Some((file, reported))
        })
        .collect();
    let output = state.typecheck_result.and_then(|result| result.tco);
    // A checked program is lowered too, so a rewrite that only HIR refuses
    // is still caught.
    if let Some(output) = output.as_ref().filter(|output| output.errors.is_empty()) {
        let modules = state.program.module_graph.as_ref();
        let lowered =
            hew_hir::lower_program_host_target(&state.program, output, &hew_hir::ResolutionCtx);
        for error in lowered.diagnostics {
            let file = match &error.source_module {
                None => std::fs::canonicalize(root).ok(),
                Some(module) => modules
                    .and_then(|graph| graph.modules.iter().find(|(id, _)| id.dotted() == *module))
                    .and_then(|(_, module)| module.source_paths.first())
                    .and_then(|path| std::fs::canonicalize(path).ok()),
            };
            diagnostics.push((
                file,
                FileDiagnostic {
                    code: format!("hir {:?}", error.kind),
                    message: error.note,
                    span: error.span,
                },
            ));
        }
    }
    UnitCheck {
        diagnostics,
        output,
        indices,
        sources,
    }
}

impl UnitCheck {
    /// The span-key index of `path` in this unit's module graph. A file the
    /// graph does not list is the unit's own root only when it is that root.
    fn file_index(&self, path: &Path, root: &Path) -> Option<u32> {
        let canonical = std::fs::canonicalize(path).ok();
        canonical
            .as_deref()
            .and_then(|path| self.indices.path_index(path))
            .or_else(|| self.indices.path_index(path))
            .or_else(|| {
                let root = std::fs::canonicalize(root).ok();
                (canonical.is_some() && canonical == root).then_some(0)
            })
    }

    fn reported_in(&self, path: &Path) -> Vec<FileDiagnostic> {
        let path = std::fs::canonicalize(path).ok();
        self.diagnostics
            .iter()
            .filter(|(file, _)| path.is_some() && *file == path)
            .map(|(_, diagnostic)| diagnostic.clone())
            .collect()
    }

    fn expr_type(&self, span: &Span, file: u32) -> Option<&Ty> {
        self.output
            .as_ref()?
            .expr_types
            .get(&SpanKey::in_module(span, file))
    }

    fn annotation_type(&self, span: &Span, file: u32) -> Option<Ty> {
        self.output
            .as_ref()?
            .resolved_annotation_types
            .get(&SpanKey::in_module(span, file))
            .map(hew_types::ResolvedTy::to_ty)
    }

    /// Whether `call` is `Result.Ok(..)`/`Result.Err(..)`: a `Result`-typed
    /// call on a receiver that names the builtin `Result` rather than a value.
    ///
    /// Deviation: the checker publishes no resolution for the `Result`
    /// receiver of the method-call spelling, so a receiver with no value type
    /// under a `Result`-typed call stands in for it. It can go when the
    /// checker records that receiver's `Resolution::Builtin` like any other
    /// type path; the published resolution is the real authority.
    fn names_result(&self, call: &Span, receiver: &Span, file: u32) -> bool {
        let Some(output) = &self.output else {
            return false;
        };
        let key = SpanKey::in_module(receiver, file);
        let resolved = matches!(
            output.resolutions.get(&key),
            Some(Resolution::Builtin(BuiltinType::Result))
        );
        let type_receiver = !output.expr_types.contains_key(&key)
            && self
                .expr_type(call, file)
                .is_some_and(|ty| ty.as_result().is_some());
        resolved || type_receiver
    }

    /// Every ask of a source `receive fn`, by the handler's declaration site,
    /// at the caller's location.
    fn asks(&self, documents: &DocumentSet) -> HashMap<HandlerSite, Vec<Location>> {
        let mut asks: HashMap<HandlerSite, Vec<Location>> = HashMap::new();
        let Some(output) = &self.output else {
            return asks;
        };
        for (key, kind) in &output.actor_method_dispatch {
            let ActorMethodKind::Ask { method, .. } = kind else {
                continue;
            };
            let Some(site) = output.defs.site(*method) else {
                continue;
            };
            if site.kind() != hew_types::DeclarationKind::ActorReceive {
                continue;
            }
            let Some(file) = site
                .module()
                .and_then(|module| output.defs.module_source(module))
            else {
                continue;
            };
            let Some(caller) = self.sources.get(&key.module_idx) else {
                continue;
            };
            let Some(text) = documents.get(caller) else {
                continue;
            };
            let (line, column) = crate::diagnostic::offset_to_line_col(text, key.start);
            asks.entry(HandlerSite {
                file: file.to_path_buf(),
                actor: site.span().start,
                index: site.ordinal(),
            })
            .or_default()
            .push(Location {
                file: caller.clone(),
                line,
                column,
            });
        }
        for callers in asks.values_mut() {
            callers.sort_by(|a, b| (&a.file, a.line, a.column).cmp(&(&b.file, b.line, b.column)));
        }
        asks
    }
}

/// The first diagnostic the conversion leaves in its file: one the file did
/// not have before, matched by kind, message and the original position it
/// maps back to, or a failure exit that still has no edge.
fn first_new_diagnostic<'a>(
    before: &[FileDiagnostic],
    after: &'a [FileDiagnostic],
    rewrite: &Rewrite,
) -> Option<&'a FileDiagnostic> {
    let mut unmatched: Vec<&FileDiagnostic> = before.iter().collect();
    after.iter().find(|diagnostic| {
        if diagnostic.is_missing_edge() {
            return true;
        }
        let start = rewrite.original_offset(diagnostic.span.start);
        match unmatched.iter().position(|old| {
            old.code == diagnostic.code
                && old.message == diagnostic.message
                && old.span.start == start
        }) {
            Some(found) => {
                unmatched.swap_remove(found);
                false
            }
            None => true,
        }
    })
}

/// One rewrite: the source range it replaces and what replaces it.
struct Edit {
    range: Range<usize>,
    pieces: Vec<Piece>,
}

impl Edit {
    fn text(range: Range<usize>, text: impl Into<String>) -> Self {
        Self {
            range,
            pieces: vec![Piece::Text(text.into())],
        }
    }
}

/// Part of a replacement: new text, or a range of the source carried over,
/// which an edit nested inside it rewrites in turn.
enum Piece {
    Text(String),
    Source(Range<usize>),
}

/// A rewritten file and how its offsets map back to the source.
struct Rewrite {
    text: String,
    /// Rewritten ranges by the source range they came from; a copied range
    /// maps offset for offset, a written one to its edit's start.
    segments: Vec<(Range<usize>, Range<usize>, bool)>,
}

impl Rewrite {
    fn original_offset(&self, offset: usize) -> usize {
        self.segments
            .iter()
            .find(|(rewritten, _, _)| rewritten.contains(&offset))
            .map_or(offset, |(rewritten, original, copied)| {
                if *copied {
                    original.start + (offset - rewritten.start)
                } else {
                    original.start
                }
            })
    }
}

/// Apply `edits` to `source`. An edit inside another applies to the source
/// that one carries over; an edit that crosses another's boundary, or lies
/// in text the outer edit replaces, cannot compose and reports its offset.
fn render(source: &str, mut edits: Vec<Edit>) -> Result<Rewrite, usize> {
    edits.sort_by_key(|edit| (edit.range.start, std::cmp::Reverse(edit.range.end)));
    edits.dedup_by(|a, b| a.range == b.range);
    let mut rewrite = Rewrite {
        text: String::new(),
        segments: Vec::new(),
    };
    let mut used = vec![false; edits.len()];
    render_range(source, 0..source.len(), &edits, &mut used, &mut rewrite)?;
    match used.iter().position(|used| !used) {
        Some(stray) => Err(edits[stray].range.start),
        None => Ok(rewrite),
    }
}

fn render_range(
    source: &str,
    range: Range<usize>,
    edits: &[Edit],
    used: &mut [bool],
    rewrite: &mut Rewrite,
) -> Result<(), usize> {
    let mut cursor = range.start;
    for (index, edit) in edits.iter().enumerate() {
        if used[index] || edit.range.start < cursor || edit.range.end > range.end {
            if !used[index] && edit.range.start < range.end && edit.range.end > range.end {
                return Err(edit.range.start);
            }
            continue;
        }
        if edit.range.start >= range.end {
            break;
        }
        copy(source, cursor..edit.range.start, rewrite);
        used[index] = true;
        for piece in &edit.pieces {
            match piece {
                Piece::Text(text) => {
                    let start = rewrite.text.len();
                    rewrite.text.push_str(text);
                    rewrite
                        .segments
                        .push((start..rewrite.text.len(), edit.range.clone(), false));
                }
                Piece::Source(inner) => render_range(source, inner.clone(), edits, used, rewrite)?,
            }
        }
        cursor = edit.range.end;
    }
    copy(source, cursor..range.end, rewrite);
    Ok(())
}

fn copy(source: &str, range: Range<usize>, rewrite: &mut Rewrite) {
    if range.is_empty() {
        return;
    }
    let start = rewrite.text.len();
    rewrite.text.push_str(&source[range.clone()]);
    rewrite
        .segments
        .push((start..rewrite.text.len(), range, true));
}

/// The rewrite of one file.
#[derive(Default)]
struct Plan {
    edits: Vec<Edit>,
    /// Converted `receive fn`s.
    handlers: Vec<PlannedHandler>,
}

/// A `receive fn` the plan converts: its name's offset and name, where its
/// actor's item starts and its index among the actor's handlers.
struct PlannedHandler {
    offset: usize,
    name: String,
    actor: usize,
    index: u32,
}

/// Which body a callable has.
enum Body<'a> {
    Block(&'a Block),
    Expr(&'a Spanned<Expr>),
}

fn plan_file(source: &str, file: u32, unit: &UnitCheck) -> Plan {
    let parsed = hew_parser::parse(source);
    let mut plan = Plan::default();
    if !parsed.errors.is_empty() {
        return plan;
    }
    let planner = Planner { source, file, unit };
    let mut lambdas = LambdaFinder::default();
    for (item, item_span) in &parsed.program.items {
        let mut callable = |annotation: Option<&Spanned<TypeExpr>>,
                            body: &Block,
                            handler: Option<PlannedHandler>,
                            plan: &mut Plan| {
            hew_types::walk_block(body, &mut lambdas);
            if planner.convert_declared(annotation, &Body::Block(body), plan) {
                if let Some(handler) = handler {
                    plan.handlers.push(handler);
                }
            }
        };
        match item {
            Item::Function(function) if !function.is_generator => {
                callable(
                    function.return_type.as_ref(),
                    &function.body,
                    None,
                    &mut plan,
                );
            }
            Item::Impl(implementation) => {
                for method in implementation.methods.iter().filter(|m| !m.is_generator) {
                    callable(method.return_type.as_ref(), &method.body, None, &mut plan);
                }
            }
            Item::Trait(declaration) => {
                for trait_item in &declaration.items {
                    if let TraitItem::Method(method) = trait_item {
                        if let Some(body) = &method.body {
                            callable(method.return_type.as_ref(), body, None, &mut plan);
                        }
                    }
                }
            }
            Item::TypeDecl(declaration) => {
                for body_item in &declaration.body {
                    if let TypeBodyItem::Method(method) = body_item {
                        if !method.is_generator {
                            callable(method.return_type.as_ref(), &method.body, None, &mut plan);
                        }
                    }
                }
            }
            Item::Actor(actor) => {
                for method in actor.methods.iter().filter(|m| !m.is_generator) {
                    callable(method.return_type.as_ref(), &method.body, None, &mut plan);
                }
                for (index, handler) in actor.receive_fns.iter().enumerate() {
                    if handler.is_generator {
                        continue;
                    }
                    let handler_plan = PlannedHandler {
                        offset: handler.span.start,
                        name: handler.name.to_string(),
                        actor: item_span.start,
                        index: u32::try_from(index).unwrap_or(u32::MAX),
                    };
                    callable(
                        handler.return_type.as_ref(),
                        &handler.body,
                        Some(handler_plan),
                        &mut plan,
                    );
                }
            }
            _ => {}
        }
    }
    for (span, return_type, body) in std::mem::take(&mut lambdas.found) {
        let (return_type, body) = (return_type.as_ref(), &body);
        if return_type.is_some() {
            planner.convert_declared(return_type, &Body::Expr(body), &mut plan);
        } else {
            planner.convert_closure(&span, &Body::Expr(body), &mut plan);
        }
    }
    plan
}

/// Every closure literal, with its declared return and body, cloned so the
/// planner can visit them after the items.
#[derive(Default)]
struct LambdaFinder {
    found: Vec<(Span, Option<Spanned<TypeExpr>>, Spanned<Expr>)>,
}

impl NodeVisitor for LambdaFinder {
    fn visit_expr(&mut self, expr: &Expr, span: &Span) {
        if let Expr::Lambda {
            return_type, body, ..
        } = expr
        {
            self.found
                .push((span.clone(), return_type.clone(), (**body).clone()));
        }
    }
}

/// The exits a body spells outside any nested callable.
#[derive(Default)]
struct Exits {
    return_error: bool,
    /// The operand of every postfix `?`; only one on a `Result` fails.
    tries: Vec<Span>,
    returns: Vec<(Span, Spanned<Expr>)>,
}

impl NodeVisitor for Exits {
    fn visit_stmt(&mut self, stmt: &Stmt, span: &Span) {
        if let Stmt::Return(Some(value)) = stmt {
            self.returns.push((span.clone(), value.clone()));
        }
    }

    fn visit_expr(&mut self, expr: &Expr, span: &Span) {
        match expr {
            Expr::ReturnError(_) => self.return_error = true,
            Expr::PostfixTry(operand) => self.tries.push(operand.1.clone()),
            Expr::Return(Some(value)) => self.returns.push((span.clone(), (**value).clone())),
            _ => {}
        }
    }

    fn enters_nested_callables(&self) -> bool {
        false
    }
}

struct Planner<'a> {
    source: &'a str,
    file: u32,
    unit: &'a UnitCheck,
}

impl Planner<'_> {
    /// Convert a callable declared `-> Result<T, E>` whose body needs an edge.
    /// Returns whether it was converted.
    fn convert_declared(
        &self,
        annotation: Option<&Spanned<TypeExpr>>,
        body: &Body<'_>,
        plan: &mut Plan,
    ) -> bool {
        let Some(annotation) = annotation else {
            return false;
        };
        if matches!(annotation.0, TypeExpr::Fallible { .. }) {
            return false;
        }
        let Some(declared) = self.unit.annotation_type(&annotation.1, self.file) else {
            return false;
        };
        let Some((success, _)) = declared.as_result() else {
            return false;
        };
        let exits = Self::exits(body);
        let leaves = Self::tail_leaves(body);
        let wraps_tail = leaves.iter().map(|leaf| &leaf.expr).any(|leaf| {
            !Self::is_exit(leaf)
                && self.variant_payload(leaf).is_none()
                && self
                    .unit
                    .expr_type(&leaf.1, self.file)
                    .is_some_and(|ty| *ty != declared && ty == success)
        });
        if !self.fails(&exits) && !wraps_tail {
            return false;
        }
        let Some(edit) = self.annotation_edit(annotation) else {
            return false;
        };
        plan.edits.push(edit);
        self.rewrite_exits(&declared, &exits, &leaves, plan);
        true
    }

    /// Rewrite the exits of a closure without a declared return whose body
    /// fails: such a closure takes a failure edge, so its `Ok`/`Err` exits
    /// become the value and `return error`. A `Result`-valued exit gains `?`
    /// when the checker typed the closure's `Result`.
    fn convert_closure(&self, span: &Span, body: &Body<'_>, plan: &mut Plan) {
        let exits = Self::exits(body);
        if !self.fails(&exits) {
            return;
        }
        let declared = match self.unit.expr_type(span, self.file) {
            Some(Ty::Function { ret, .. } | Ty::Closure { ret, .. })
                if ret.as_result().is_some() =>
            {
                (**ret).clone()
            }
            _ => Ty::Error,
        };
        let leaves = Self::tail_leaves(body);
        self.rewrite_exits(&declared, &exits, &leaves, plan);
    }

    /// Whether a body leaves through a failure edge: `return error`, or a
    /// `?` on an operand the checker did not type as an `Option`.
    fn fails(&self, exits: &Exits) -> bool {
        exits.return_error
            || exits.tries.iter().any(|operand| {
                self.unit
                    .expr_type(operand, self.file)
                    .is_none_or(|ty| ty.as_option().is_none())
            })
    }

    fn exits(body: &Body<'_>) -> Exits {
        let mut exits = Exits::default();
        match body {
            Body::Block(block) => hew_types::walk_block(block, &mut exits),
            Body::Expr(expr) => hew_types::walk_expr(&expr.0, &expr.1, &mut exits),
        }
        exits
    }

    fn rewrite_exits(&self, declared: &Ty, exits: &Exits, leaves: &[Leaf], plan: &mut Plan) {
        for (span, value) in &exits.returns {
            if let Some(edit) = self.return_edit(span, value, declared) {
                plan.edits.push(edit);
            }
        }
        for leaf in leaves {
            if let Some(edit) = self.leaf_edit(leaf, declared) {
                plan.edits.push(edit);
            }
        }
    }

    /// `Result<T, E>` becomes `T fails E`; with a unit `T`, `-> Result<(), E>`
    /// becomes `fails E`. A function-typed `T` is parenthesized, since
    /// `fails` would otherwise bind to its own return.
    fn annotation_edit(&self, annotation: &Spanned<TypeExpr>) -> Option<Edit> {
        let (ok, err) = match &annotation.0 {
            TypeExpr::Result { ok, err } => (ok.as_ref(), err.as_ref()),
            TypeExpr::Named {
                type_args: Some(args),
                ..
            } if args.len() == 2 => (&args[0], &args[1]),
            _ => return None,
        };
        let error = self.text(&err.1)?;
        let unit = matches!(&ok.0, TypeExpr::Tuple(elements) if elements.is_empty());
        if unit {
            // Drop the arrow with the unit success type.
            let before = self.source.get(..annotation.1.start)?;
            let arrow = before.trim_end().strip_suffix("->")?.len();
            return Some(Edit::text(
                arrow..self.trimmed(&annotation.1).end,
                format!("fails {error}"),
            ));
        }
        let success = self.text(&ok.1)?;
        let success = if matches!(ok.0, TypeExpr::Function { .. } | TypeExpr::ActorFn { .. }) {
            format!("({success})")
        } else {
            success.to_string()
        };
        Some(Edit::text(
            self.trimmed(&annotation.1),
            format!("{success} fails {error}"),
        ))
    }

    fn return_edit(&self, span: &Span, value: &Spanned<Expr>, declared: &Ty) -> Option<Edit> {
        match self.variant_payload(value) {
            Some((Variant::Ok, payload)) if Self::is_unit(payload) => {
                // `return .Ok(())` is a bare `return`.
                let keyword_end = span.start + self.source.get(span.clone())?.find("return")? + 6;
                Some(Edit::text(keyword_end..self.trimmed(&value.1).end, ""))
            }
            Some((Variant::Ok, payload)) => Some(self.carry(&value.1, &payload.1, "", "")),
            Some((Variant::Err, payload)) => Some(self.carry(&value.1, &payload.1, "error ", "")),
            None => self.propagate(value, declared),
        }
    }

    fn leaf_edit(&self, leaf: &Leaf, declared: &Ty) -> Option<Edit> {
        let expr = &leaf.expr;
        if Self::is_exit(expr) {
            return None;
        }
        match self.variant_payload(expr) {
            Some((Variant::Ok, payload)) if Self::is_unit(payload) && leaf.block_tail => {
                // A unit success at the end of a block is the block falling
                // off its end; an `else` holding nothing else goes with it.
                if let Some(range) = self.empty_else(&expr.1) {
                    return Some(Edit::text(range, ""));
                }
                let end = self.trimmed(&expr.1).end;
                let start = self.source.get(..expr.1.start)?.trim_end().len();
                Some(Edit::text(start..end, ""))
            }
            Some((Variant::Ok, payload)) if Self::is_unit(payload) => {
                Some(Edit::text(self.trimmed(&expr.1), "()"))
            }
            Some((Variant::Ok, payload)) => Some(self.carry(&expr.1, &payload.1, "", "")),
            Some((Variant::Err, payload)) => {
                // A block's own tail becomes a statement.
                let close = if leaf.block_tail { ";" } else { "" };
                Some(self.carry(&expr.1, &payload.1, "return error ", close))
            }
            None => self.propagate(expr, declared),
        }
    }

    /// Replace `whole` with `payload`'s source between `before` and `after`;
    /// an edit inside the payload still applies.
    fn carry(&self, whole: &Span, payload: &Span, before: &str, after: &str) -> Edit {
        let mut pieces = Vec::with_capacity(3);
        if !before.is_empty() {
            pieces.push(Piece::Text(before.to_string()));
        }
        pieces.push(Piece::Source(self.trimmed(payload)));
        if !after.is_empty() {
            pieces.push(Piece::Text(after.to_string()));
        }
        Edit {
            range: self.trimmed(whole),
            pieces,
        }
    }

    /// The ` else { <value> }` around `value` when that `else` holds nothing
    /// else: from the end of the branch before it through its closing brace.
    fn empty_else(&self, value: &Span) -> Option<Range<usize>> {
        let before = self.source.get(..value.start)?.trim_end();
        let before = before.strip_suffix('{')?.trim_end();
        let before = before.strip_suffix("else")?;
        if !before.ends_with(|c: char| c.is_whitespace() || c == '}') {
            return None;
        }
        let start = before.trim_end().len();
        let end = self.trimmed(value).end;
        let rest = self.source.get(end..)?;
        let close = rest.len() - rest.trim_start().len();
        rest.trim_start()
            .starts_with('}')
            .then_some(start..end + close + 1)
    }

    /// A `Result`-valued exit leaves through the edge with `?`.
    fn propagate(&self, value: &Spanned<Expr>, declared: &Ty) -> Option<Edit> {
        let ty = self.unit.expr_type(&value.1, self.file)?;
        if ty != declared || ty.as_result().is_none() {
            return None;
        }
        let postfix = matches!(
            value.0,
            Expr::Call { .. }
                | Expr::MethodCall { .. }
                | Expr::Ident(_)
                | Expr::FieldAccess { .. }
                | Expr::Index { .. }
                | Expr::PostfixTry(_)
        );
        let range = self.trimmed(&value.1);
        Some(if postfix {
            Edit {
                range: range.clone(),
                pieces: vec![Piece::Source(range), Piece::Text("?".to_string())],
            }
        } else {
            Edit {
                range: range.clone(),
                pieces: vec![
                    Piece::Text("(".to_string()),
                    Piece::Source(range),
                    Piece::Text(")?".to_string()),
                ],
            }
        })
    }

    /// The `Result` constructor `expr` spells, `.Ok(v)` or `Result.Ok(v)`,
    /// and its one payload.
    fn variant_payload<'e>(&self, expr: &'e Spanned<Expr>) -> Option<(Variant, &'e Spanned<Expr>)> {
        let (name, args) = match &expr.0 {
            Expr::Call { function, args, .. } => match &function.0 {
                Expr::ContextVariant(variant) if variant.record.is_none() => (variant.name, args),
                _ => return None,
            },
            Expr::MethodCall {
                receiver,
                method,
                args,
            } if matches!(receiver.0, Expr::Ident(_))
                && self.unit.names_result(&expr.1, &receiver.1, self.file) =>
            {
                (method.0, args)
            }
            _ => return None,
        };
        let variant = match name.name.as_str() {
            "Ok" => Variant::Ok,
            "Err" => Variant::Err,
            _ => return None,
        };
        match args.as_slice() {
            [CallArg::Positional(payload)] => Some((variant, payload)),
            _ => None,
        }
    }

    fn is_unit(expr: &Spanned<Expr>) -> bool {
        matches!(&expr.0, Expr::Tuple(elements) if elements.is_empty())
    }

    fn is_exit(expr: &Spanned<Expr>) -> bool {
        matches!(expr.0, Expr::Return(_) | Expr::ReturnError(_))
    }

    /// The expressions whose value is the callable's value: the tail, through
    /// blocks, `if`/`if let` branches and `match` arms.
    fn tail_leaves(body: &Body<'_>) -> Vec<Leaf> {
        let mut leaves = Vec::new();
        match body {
            Body::Block(block) => Self::block_leaves(block, &mut leaves),
            Body::Expr(expr) => Self::expr_leaves(expr, &mut leaves),
        }
        leaves
    }

    fn block_leaves(block: &Block, leaves: &mut Vec<Leaf>) {
        if let Some(tail) = &block.trailing_expr {
            if matches!(
                tail.0,
                Expr::Block(_)
                    | Expr::UnsafeBlock(_)
                    | Expr::If { .. }
                    | Expr::IfLet { .. }
                    | Expr::Match { .. }
            ) {
                Self::expr_leaves(tail, leaves);
            } else {
                leaves.push(Leaf {
                    expr: (**tail).clone(),
                    block_tail: true,
                });
            }
            return;
        }
        match block.stmts.last().map(|(stmt, _)| stmt) {
            Some(Stmt::If {
                then_block,
                else_block: Some(else_block),
                ..
            }) => {
                Self::block_leaves(then_block, leaves);
                if let Some(block) = &else_block.block {
                    Self::block_leaves(block, leaves);
                }
                if let Some(nested) = &else_block.if_stmt {
                    let nested = Block {
                        stmts: vec![(*nested.clone())],
                        trailing_expr: None,
                    };
                    Self::block_leaves(&nested, leaves);
                }
            }
            Some(Stmt::Match { arms, .. }) => {
                for arm in arms {
                    Self::expr_leaves(&arm.body, leaves);
                }
            }
            Some(Stmt::IfLet {
                body,
                else_body: Some(else_body),
                ..
            }) => {
                Self::block_leaves(body, leaves);
                Self::expr_leaves(else_body, leaves);
            }
            _ => {}
        }
    }

    fn expr_leaves(expr: &Spanned<Expr>, leaves: &mut Vec<Leaf>) {
        match &expr.0 {
            Expr::Block(block) => Self::block_leaves(block, leaves),
            Expr::UnsafeBlock(block) => Self::block_leaves(block, leaves),
            Expr::If {
                then_block,
                else_block: Some(else_block),
                ..
            } => {
                Self::expr_leaves(then_block, leaves);
                Self::expr_leaves(else_block, leaves);
            }
            Expr::IfLet {
                body,
                else_body: Some(else_body),
                ..
            } => {
                Self::block_leaves(body, leaves);
                Self::expr_leaves(else_body, leaves);
            }
            Expr::Match { arms, .. } => {
                for arm in arms {
                    Self::expr_leaves(&arm.body, leaves);
                }
            }
            _ => leaves.push(Leaf {
                expr: expr.clone(),
                block_tail: false,
            }),
        }
    }

    /// The source text of `span`, which can run on over trailing trivia.
    fn text(&self, span: &Span) -> Option<&str> {
        self.source.get(span.clone()).map(str::trim_end)
    }

    /// `span` without the trailing trivia a parsed span can carry.
    fn trimmed(&self, span: &Span) -> Range<usize> {
        let text = self.source.get(span.clone()).unwrap_or_default();
        span.start..span.start + text.trim_end().len()
    }
}

/// A value-producing tail expression, and whether it is a block's own tail
/// rather than an arm's value.
struct Leaf {
    expr: Spanned<Expr>,
    block_tail: bool,
}

#[derive(Clone, Copy)]
enum Variant {
    Ok,
    Err,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn edit(range: Range<usize>, pieces: Vec<Piece>) -> Edit {
        Edit { range, pieces }
    }

    #[test]
    fn a_nested_edit_applies_inside_the_source_its_outer_edit_carries() {
        // `.Ok(f(.Err(1)))`: the outer edit keeps its payload, the inner one
        // rewrites inside it.
        let source = ".Ok(f(.Err(1)))";
        let outer = edit(0..15, vec![Piece::Source(4..14)]);
        let inner = edit(
            6..13,
            vec![Piece::Text("error ".into()), Piece::Source(11..12)],
        );
        let rewrite = render(source, vec![inner, outer]).expect("nested edits compose");
        assert_eq!(rewrite.text, "f(error 1)");
        assert_eq!(rewrite.original_offset(0), 4);
        assert_eq!(rewrite.original_offset(8), 11);
    }

    #[test]
    fn an_edit_inside_replaced_text_cannot_compose() {
        let source = "abcdef";
        let outer = edit(0..6, vec![Piece::Text("x".into())]);
        let inner = edit(2..3, vec![Piece::Text("y".into())]);
        assert_eq!(render(source, vec![outer, inner]).err(), Some(2));
    }

    fn reported(code: &str, message: &str, span: Range<usize>) -> FileDiagnostic {
        FileDiagnostic {
            code: code.to_string(),
            message: message.to_string(),
            span,
        }
    }

    #[test]
    fn a_diagnostic_only_the_rewrite_has_is_new_whatever_its_stage() {
        // `ab` became `xab`: the old diagnostic at 1 moved to 2.
        let rewrite = render("ab", vec![edit(0..0, vec![Piece::Text("x".into())])]).unwrap();
        let before = [reported("E_MISMATCH", "kept", 1..2)];
        let kept = [reported("E_MISMATCH", "kept", 2..3)];
        assert!(first_new_diagnostic(&before, &kept, &rewrite).is_none());
        for stage in ["parse ExpectedToken", "hir CheckerBoundaryViolation"] {
            let after = [
                reported("E_MISMATCH", "kept", 2..3),
                reported(stage, "only after", 0..1),
            ];
            let new = first_new_diagnostic(&before, &after, &rewrite).expect("a new diagnostic");
            assert_eq!(new.code, stage);
        }
        // A failure exit still missing its edge refuses even when it was
        // there before.
        let missing = [reported(
            TypeErrorKind::NoFailureEdge.as_kind_str(),
            "edge",
            1..2,
        )];
        let still = [reported(
            TypeErrorKind::NoFailureEdge.as_kind_str(),
            "edge",
            2..3,
        )];
        assert!(first_new_diagnostic(&missing, &still, &rewrite).is_some());
    }

    #[test]
    fn crossing_edits_cannot_compose() {
        let source = "abcdef";
        let left = edit(0..4, vec![Piece::Source(1..4)]);
        let right = edit(2..6, vec![Piece::Text("y".into())]);
        assert!(render(source, vec![left, right]).is_err());
    }
}

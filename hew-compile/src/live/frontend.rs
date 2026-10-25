//! Immutable declaration snapshots and typed inputs for native live execution.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{self, Write};
use std::ops::Range;
use std::path::PathBuf;
use std::sync::Arc;

use hew_parser::ast::{Item, Program, Visibility};
use hew_types::{session::SessionContents, DefTable, TypeCheckOutput};

use super::LiveSubmission;
use crate::{FrontendFailure, FrontendOptions, Session, SessionError};

mod diagnostics;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InputKind {
    Declarations,
    Statements,
    Expression,
}

/// Original input and its location in the compiler-owned source wrapper.
#[derive(Debug, Clone)]
pub struct SourceView {
    pub source: String,
    pub input_span: Range<usize>,
    pub kind: InputKind,
}

#[derive(Debug)]
pub enum Error {
    Frontend(FrontendFailure),
    Lowering(SessionError),
    State(&'static str),
}

impl fmt::Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Frontend(error) => error.fmt(formatter),
            Self::Lowering(error) => error.fmt(formatter),
            Self::State(message) => formatter.write_str(message),
        }
    }
}

impl std::error::Error for Error {}

impl From<FrontendFailure> for Error {
    fn from(value: FrontendFailure) -> Self {
        Self::Frontend(value)
    }
}

impl From<SessionError> for Error {
    fn from(value: SessionError) -> Self {
        Self::Lowering(value)
    }
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub slot: u32,
    pub name: String,
    pub ty: hew_types::ResolvedTy,
    pub mutable: bool,
    pub contents: SessionContents,
}

#[derive(Debug, Clone)]
struct State {
    options: FrontendOptions,
    definitions: Arc<DefTable>,
    visible: BTreeMap<String, String>,
    snapshots: Vec<String>,
    imports: BTreeSet<String>,
    bindings: Vec<Binding>,
    next_slot: u32,
    views: BTreeMap<PathBuf, SourceView>,
}

impl State {
    fn new(options: FrontendOptions) -> Self {
        Self {
            options,
            definitions: Arc::new(DefTable::default()),
            visible: BTreeMap::new(),
            snapshots: Vec::new(),
            imports: BTreeSet::new(),
            bindings: Vec::new(),
            next_slot: 0,
            views: BTreeMap::new(),
        }
    }
}

/// A checked candidate. Preparation neither changes the active frontend nor
/// executes initializers. Publish it only with the worker's actual occupancy.
#[derive(Debug)]
pub struct PreparedInput {
    pub live: LiveSubmission,
    pub diagnostics: Vec<crate::FrontendDiagnostic>,
    pub program: Program,
    pub checked: TypeCheckOutput,
    pub source: String,
    pub path: PathBuf,
    base_revision: u64,
    next: State,
}

impl PreparedInput {
    #[must_use]
    pub const fn slot_count(&self) -> u32 {
        self.next.next_slot
    }
}

/// Persistent frontend state, separate from the live worker's owning values.
#[derive(Debug)]
pub struct LiveFrontend {
    directory: PathBuf,
    revision: u64,
    next_input: u64,
    initial_options: FrontendOptions,
    state: State,
}

impl LiveFrontend {
    /// Anchor virtual submission documents beside the user's source so ordinary
    /// module and quoted-file imports retain their normal relative resolution.
    #[must_use]
    pub fn new(directory: PathBuf, mut options: FrontendOptions) -> Self {
        options.repl_fragment = true;
        Self {
            directory,
            revision: 0,
            next_input: 0,
            initial_options: options.clone(),
            state: State::new(options),
        }
    }

    #[must_use]
    pub fn bindings(&self) -> &[Binding] {
        &self.state.bindings
    }

    #[must_use]
    pub fn source_views(&self) -> &BTreeMap<PathBuf, SourceView> {
        &self.state.views
    }

    /// Forget the authoring environment after the worker has released its
    /// places. Input identities stay monotonic and old candidates become stale.
    ///
    /// # Errors
    /// Refuses an exhausted session revision counter.
    pub fn clear(&mut self) -> Result<(), Error> {
        self.revision = self
            .revision
            .checked_add(1)
            .ok_or(Error::State("live revisions exhausted"))?;
        self.state = State::new(self.initial_options.clone());
        Ok(())
    }

    /// Check and lower a candidate using original declaration identities and
    /// current live bindings. Only this input contributes executable statements.
    ///
    /// # Errors
    /// Returns ordinary frontend/lowering diagnostics or an exhausted input ID.
    pub fn prepare(
        &mut self,
        compiler: &Session,
        input: &str,
        kind: InputKind,
    ) -> Result<PreparedInput, Error> {
        let id = self.next_input;
        self.next_input = id
            .checked_add(1)
            .ok_or(Error::State("live input IDs exhausted"))?;
        let mut next = self.state.clone();
        // Source snapshots persist; input observations belong to this candidate.
        next.options.documents.reads = Arc::default();
        next.options.documents.dependency_roots = Arc::default();
        let body = if kind == InputKind::Declarations {
            self.prepare_declarations(id, input, &mut next)?;
            ""
        } else {
            input
        };
        let path = self.directory.join(format!("__hew_live_input_{id}.hew"));
        if next.options.documents.is_file(&path) {
            return Err(Error::State("live input path is already in use"));
        }
        let (source, input_span) = input_source(&next, body, kind);
        next.views.insert(
            path.clone(),
            SourceView {
                source: input.to_owned(),
                input_span,
                kind,
            },
        );
        let mut program = crate::parse_source(&source, &path.to_string_lossy())
            .map_err(|failure| diagnostics::remap(failure, &next.views))?;
        let Some((Item::Function(entry), _)) = program.items.last_mut() else {
            return Err(Error::State("live input has no generated submission entry"));
        };
        let mut annotations = BTreeMap::new();
        for (parameter, binding) in entry.params.iter_mut().zip(&next.bindings) {
            parameter.is_mutable = binding.mutable;
            parameter.is_consume = true;
            annotations.insert(
                (parameter.ty.1.start, parameter.ty.1.end),
                binding.ty.clone(),
            );
        }
        next.options.session_type_seed = Some(Arc::new(hew_types::session::SessionTypeSeed {
            definitions: next.definitions.clone(),
            annotations,
        }));
        next.options
            .documents
            .insert_prepared(path.clone(), source.clone(), program.clone());
        let front = crate::run_program_frontend_to_typecheck(
            program,
            &source,
            &path.to_string_lossy(),
            &next.options,
        )
        .map_err(|failure| diagnostics::remap(failure, &next.views))?;
        let checked = front
            .typecheck_result
            .tco
            .ok_or(Error::State("live input requires type checking"))?;
        let roots = Session::source_roots(&front.program, &checked)?;
        let [entry] = roots.as_slice() else {
            return Err(Error::State("live input requires one submission entry"));
        };
        let inputs: Vec<_> = next
            .bindings
            .iter()
            .map(|binding| (binding.slot, binding.contents.clone()))
            .collect();
        let live = compiler
            .lower_live_program_with_contents(
                &front.program,
                &checked,
                *entry,
                &inputs,
                next.next_slot,
            )
            .map_err(|error| diagnostics::lowering(error, &front.program, &source, &path, &next))?;
        next.definitions = checked.defs.clone();
        next.next_slot = live.next_slot;
        next.bindings = live
            .bindings
            .iter()
            .map(|binding| Binding {
                slot: binding.slot,
                name: binding.binding.name.clone(),
                ty: binding.binding.ty.clone(),
                mutable: binding.binding.mutable,
                contents: binding.contents.clone(),
            })
            .collect();
        Ok(PreparedInput {
            live,
            diagnostics: diagnostics::remap_all(front.diagnostics, &next.views),
            program: front.program,
            checked,
            source,
            path,
            base_revision: self.revision,
            next,
        })
    }

    /// Publish the worker's actual initialized places, including fault outcomes.
    /// Returns slots hidden by a newer same-name binding for semantic release.
    ///
    /// # Errors
    /// Refuses stale candidates and occupancy from a different slot inventory.
    pub fn commit(
        &mut self,
        prepared: PreparedInput,
        contents: &[SessionContents],
    ) -> Result<Vec<u32>, Error> {
        if prepared.base_revision != self.revision {
            return Err(Error::State(
                "live candidate no longer matches the active frontend",
            ));
        }
        if contents.len() != prepared.next.next_slot as usize {
            return Err(Error::State(
                "live occupancy differs from the prepared slot inventory",
            ));
        }
        let revision = self
            .revision
            .checked_add(1)
            .ok_or(Error::State("live revisions exhausted"))?;
        let mut next = prepared.next;
        let mut latest = BTreeMap::new();
        let mut retired = Vec::new();
        for mut binding in next.bindings.drain(..) {
            binding.contents = contents[binding.slot as usize].clone();
            if binding.contents != SessionContents::Empty {
                if let Some(previous) = latest.insert(binding.name.clone(), binding) {
                    retired.push(previous.slot);
                }
            }
        }
        next.bindings = latest.into_values().collect();
        self.state = next;
        self.revision = revision;
        Ok(retired)
    }

    fn prepare_declarations(&self, id: u64, input: &str, next: &mut State) -> Result<(), Error> {
        let name = format!("__hew_live_declarations_{id}");
        let path = self.directory.join(format!("{name}.hew"));
        if next.options.documents.is_file(&path) {
            return Err(Error::State("live declaration path is already in use"));
        }
        let prefix = imports(next);
        let source = format!("{prefix}{input}");
        next.views.insert(
            path.clone(),
            SourceView {
                source: input.to_owned(),
                input_span: prefix.len()..prefix.len() + input.len(),
                kind: InputKind::Declarations,
            },
        );
        let mut program = crate::parse_source(&source, &path.to_string_lossy())
            .map_err(|failure| diagnostics::remap(failure, &next.views))?;
        publish(&mut program);
        next.options
            .documents
            .insert_prepared(path.clone(), source.clone(), program);
        let mut expanded =
            crate::run_file_frontend_to_expansion(&path.to_string_lossy(), &next.options)
                .map_err(|failure| diagnostics::remap(failure, &next.views))?
                .program;
        expanded.module_graph = None;
        for (item, span) in &expanded.items {
            if span.start >= prefix.len() && matches!(item, Item::Import(_)) {
                next.imports
                    .insert(hew_parser::fmt::format_expanded_item(item, span));
            }
        }
        for exported in publish(&mut expanded) {
            next.visible.insert(exported, name.clone());
        }
        next.snapshots.push(name);
        next.options
            .documents
            .insert_prepared(path.clone(), source, expanded);
        Ok(())
    }
}

fn input_source(state: &State, input: &str, kind: InputKind) -> (String, Range<usize>) {
    let mut source = imports(state);
    source.push_str("pub fn __hew_live_submission(");
    for (index, binding) in state.bindings.iter().enumerate() {
        if index != 0 {
            source.push_str(", ");
        }
        write!(source, "{}: _", binding.name).expect("writing to a String cannot fail");
    }
    source.push_str(") {\n");
    if kind == InputKind::Expression {
        source.push_str("println(");
    }
    let start = source.len();
    source.push_str(input);
    let input_span = start..source.len();
    if kind == InputKind::Expression {
        source.push_str(");");
    }
    source.push_str("\n}\n");
    (source, input_span)
}

fn imports(state: &State) -> String {
    let mut source = state.imports.iter().cloned().collect::<String>();
    for module in &state.snapshots {
        writeln!(source, "import {module};").expect("writing to a String cannot fail");
    }
    let mut selected: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
    for (name, module) in &state.visible {
        selected.entry(module).or_default().push(name);
    }
    for (module, names) in selected {
        writeln!(source, "import {module}.{{{}}};", names.join(", "))
            .expect("writing to a String cannot fail");
    }
    source
}

fn publish(program: &mut Program) -> Vec<String> {
    let mut names = Vec::new();
    for (item, _) in &mut program.items {
        let (name, visibility) = match item {
            Item::Macro(value) => (&value.name, &mut value.visibility),
            Item::Const(value) => (&value.name, &mut value.visibility),
            Item::TypeDecl(value) => (&value.name, &mut value.visibility),
            Item::TypeAlias(value) => (&value.name, &mut value.visibility),
            Item::Trait(value) => (&value.name, &mut value.visibility),
            Item::Function(value) => (&value.name, &mut value.visibility),
            Item::Actor(value) => (&value.name, &mut value.visibility),
            Item::Supervisor(value) => (&value.name, &mut value.visibility),
            Item::Machine(value) => (&value.name, &mut value.visibility),
            Item::Record(value) => (&value.name, &mut value.visibility),
            Item::Impl(value) => {
                for method in &mut value.methods {
                    method.visibility = Visibility::Pub;
                }
                continue;
            }
            Item::Import(_) | Item::MacroCall(_) | Item::ExternBlock(_) => continue,
        };
        *visibility = Visibility::Pub;
        if name.ctx == hew_parser::ast::SyntaxContext::ROOT {
            names.push(name.to_string());
        }
    }
    names
}

#[cfg(test)]
mod tests {
    use super::*;

    fn compiler() -> Session {
        Session::new(
            crate::SessionTarget::native(),
            crate::DiagnosticPolicy::default(),
        )
    }

    #[test]
    fn live_frontend_publishes_only_reported_owners() {
        let directory = tempfile::tempdir().unwrap();
        let mut frontend =
            LiveFrontend::new(directory.path().to_path_buf(), FrontendOptions::default());
        let first = frontend
            .prepare(&compiler(), "var count: i64 = 40;", InputKind::Statements)
            .unwrap();
        assert!(frontend.bindings().is_empty());
        assert_eq!(first.slot_count(), 1);
        frontend.commit(first, &[SessionContents::Full]).unwrap();
        let next = frontend
            .prepare(&compiler(), "count += 2;", InputKind::Statements)
            .unwrap();
        assert_eq!(next.live.bindings[0].slot, 0);
        assert_eq!(next.slot_count(), 1);
        frontend.commit(next, &[SessionContents::Empty]).unwrap();
        assert!(frontend.bindings().is_empty());
        assert!(frontend
            .prepare(&compiler(), "count", InputKind::Expression)
            .is_err());
    }

    #[test]
    fn live_frontend_retains_old_nominals_when_a_new_declaration_shadows_them() {
        let directory = tempfile::tempdir().unwrap();
        let mut frontend =
            LiveFrontend::new(directory.path().to_path_buf(), FrontendOptions::default());
        let first = frontend
            .prepare(
                &compiler(),
                "type Token { value: i64; }",
                InputKind::Declarations,
            )
            .unwrap();
        frontend.commit(first, &[]).unwrap();
        let old = frontend
            .prepare(
                &compiler(),
                "let old = Token { value: 7 };",
                InputKind::Statements,
            )
            .unwrap();
        frontend.commit(old, &[SessionContents::Full]).unwrap();
        let old_ty = frontend.bindings()[0].ty.clone();
        let replacement = frontend
            .prepare(
                &compiler(),
                "type Token { value: string; }",
                InputKind::Declarations,
            )
            .unwrap();
        frontend
            .commit(replacement, &[SessionContents::Full])
            .unwrap();
        assert_eq!(frontend.bindings()[0].ty, old_ty);
        let next = frontend
            .prepare(
                &compiler(),
                "let latest = Token { value: \"new\" }; let value: i64 = old.value;",
                InputKind::Statements,
            )
            .unwrap();
        assert_eq!(next.live.bindings[0].binding.ty, old_ty);
        assert_ne!(next.live.bindings[1].binding.ty, old_ty);
    }

    #[test]
    fn live_frontend_clear_invalidates_prepared_inputs() {
        let directory = tempfile::tempdir().unwrap();
        let mut frontend =
            LiveFrontend::new(directory.path().to_path_buf(), FrontendOptions::default());
        let pending = frontend
            .prepare(&compiler(), "let before = 1;", InputKind::Statements)
            .unwrap();
        frontend.clear().unwrap();
        assert!(frontend.commit(pending, &[SessionContents::Full]).is_err());
        assert!(frontend
            .prepare(&compiler(), "before", InputKind::Expression)
            .is_err());
        let fresh = frontend
            .prepare(&compiler(), "let after = 2;", InputKind::Statements)
            .unwrap();
        frontend.commit(fresh, &[SessionContents::Full]).unwrap();
        assert_eq!(frontend.bindings()[0].name, "after");
    }

    #[test]
    fn live_frontend_diagnostics_refer_to_original_input() {
        let directory = tempfile::tempdir().unwrap();
        let mut frontend =
            LiveFrontend::new(directory.path().to_path_buf(), FrontendOptions::default());
        let first = frontend
            .prepare(
                &compiler(),
                "fn answer() -> i64 { 42 }",
                InputKind::Declarations,
            )
            .unwrap();
        frontend.commit(first, &[]).unwrap();
        for (source, kind) in [
            ("unknown_name", InputKind::Expression),
            ("let value: i64 = \"wrong\";", InputKind::Statements),
            (
                "fn broken() -> i64 { unknown_name }",
                InputKind::Declarations,
            ),
            ("let value = ;", InputKind::Statements),
        ] {
            let Error::Frontend(failure) = frontend.prepare(&compiler(), source, kind).unwrap_err()
            else {
                panic!("expected a source diagnostic");
            };
            let diagnostic = failure.diagnostics.first().unwrap();
            assert_eq!(diagnostic.source.as_deref(), Some(source));
            let location = match &diagnostic.kind {
                crate::FrontendDiagnosticKind::Parse(error) => &error.span,
                crate::FrontendDiagnosticKind::Type(error) => &error.span,
                other => panic!("unexpected diagnostic: {other:?}"),
            };
            assert!(location.start <= location.end && location.end <= source.len());
            assert!(source.is_char_boundary(location.start));
            assert!(source.is_char_boundary(location.end));
            if source.contains("unknown_name") {
                assert_eq!(&source[location.clone()], "unknown_name");
            }
        }
    }
}

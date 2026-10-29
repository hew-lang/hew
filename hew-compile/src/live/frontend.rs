//! Immutable declaration snapshots and typed inputs for native live execution.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::{self, Write};
use std::ops::Range;
use std::path::PathBuf;
use std::sync::Arc;

use hew_parser::ast::{Item, Program, Visibility};
use hew_types::{session::SessionContents, DefTable, ModuleId, ResolvedTy, TypeCheckOutput};

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
    declaration_modules: HashSet<ModuleId>,
}

struct DeclarationInput {
    name: String,
    path: PathBuf,
    prefix_end: usize,
}

#[derive(Debug, Clone)]
struct State {
    options: FrontendOptions,
    definitions: Arc<DefTable>,
    visible: BTreeMap<String, String>,
    snapshots: HashMap<ModuleId, String>,
    impl_snapshots: HashSet<ModuleId>,
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
            snapshots: HashMap::new(),
            impl_snapshots: HashSet::new(),
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
        let declarations = (kind == InputKind::Declarations)
            .then(|| self.prepare_declarations(id, input, &mut next))
            .transpose()?;
        let body = if declarations.is_some() { "" } else { input };
        let path = self.directory.join(format!("__hew_live_input_{id}.hew"));
        if next.options.documents.is_file(&path) {
            return Err(Error::State("live input path is already in use"));
        }
        let (source, input_span) = input_source(
            &next,
            body,
            kind,
            declarations.as_ref().map(|input| input.name.as_str()),
        );
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
        if let Some(declarations) = declarations {
            next.admit_declarations(declarations, &checked)?;
        }
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
            .map_err(|error| {
                diagnostics::lowering(error, &front.typecheck_result.sources, &next)
            })?;
        next.admit_bindings(&live, &checked, &front.program);
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

    fn prepare_declarations(
        &self,
        id: u64,
        input: &str,
        next: &mut State,
    ) -> Result<DeclarationInput, Error> {
        let name = format!("__hew_live_declarations_{id}");
        let path = self.directory.join(format!("{name}.hew"));
        if next.options.documents.is_file(&path) {
            return Err(Error::State("live declaration path is already in use"));
        }
        let prefix = imports(next, None);
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
        next.options
            .documents
            .insert_prepared(path.clone(), source, expanded);
        Ok(DeclarationInput {
            name,
            path,
            prefix_end: prefix.len(),
        })
    }
}

impl State {
    fn admit_bindings(
        &mut self,
        live: &LiveSubmission,
        checked: &TypeCheckOutput,
        program: &Program,
    ) {
        self.definitions = checked.defs.clone();
        self.next_slot = live.next_slot;
        let mut prior_modules: BTreeMap<_, _> = self
            .bindings
            .drain(..)
            .map(|binding| (binding.slot, binding.declaration_modules))
            .collect();
        self.bindings = live
            .bindings
            .iter()
            .map(|binding| Binding {
                slot: binding.slot,
                name: binding.binding.name.clone(),
                ty: binding.binding.ty.clone(),
                mutable: binding.binding.mutable,
                contents: binding.contents.clone(),
                declaration_modules: prior_modules.remove(&binding.slot).unwrap_or_else(|| {
                    retained_declaration_modules(
                        &binding.binding.ty,
                        checked,
                        program,
                        &self.snapshots,
                    )
                }),
            })
            .collect();
    }

    fn admit_declarations(
        &mut self,
        input: DeclarationInput,
        checked: &TypeCheckOutput,
    ) -> Result<(), Error> {
        let module = checked
            .defs
            .module_for_source(&input.path)
            .ok_or(Error::State(
                "checked live declarations have no source module",
            ))?;
        let source = self
            .options
            .documents
            .get(&input.path)
            .ok_or(Error::State("live declaration source disappeared"))?
            .to_owned();
        let mut program = self
            .options
            .documents
            .parse_document(&input.path, &source)
            .program;
        if program
            .items
            .iter()
            .any(|(item, _)| matches!(item, Item::Impl(_)))
        {
            self.impl_snapshots.insert(module);
        }
        // Unexpanded macro tokens still own their original lexical scope.
        // Other declarations carry the checker's completed import dependencies.
        if !program
            .items
            .iter()
            .any(|(item, _)| matches!(item, Item::Macro(_)))
        {
            program.items.retain(|(item, span)| {
                !matches!(item, Item::Import(_))
                    || span.start >= input.prefix_end
                    || checked
                        .used_imports
                        .contains(&hew_types::check::scope::ImportSite::new(module, span))
            });
            self.options
                .documents
                .insert_prepared(input.path, source, program);
        }
        self.snapshots.insert(module, input.name);
        Ok(())
    }
}

fn input_source(
    state: &State,
    input: &str,
    kind: InputKind,
    declarations: Option<&str>,
) -> (String, Range<usize>) {
    let mut source = imports(state, declarations);
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

fn imports(state: &State, declarations: Option<&str>) -> String {
    let mut source = state.imports.iter().cloned().collect::<String>();
    let mut modules: BTreeSet<_> = state
        .bindings
        .iter()
        .flat_map(|binding| &binding.declaration_modules)
        .chain(&state.impl_snapshots)
        .filter_map(|module| state.snapshots.get(module).map(String::as_str))
        .collect();
    modules.extend(declarations);
    for module in modules {
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

/// Keep declaration owners with the value that needs them. Anonymous producers
/// retain their admitted source closure; their scoped span keys cannot be
/// interpreted through a later compilation's graph.
fn retained_declaration_modules(
    ty: &ResolvedTy,
    checked: &TypeCheckOutput,
    program: &Program,
    snapshots: &HashMap<ModuleId, String>,
) -> HashSet<ModuleId> {
    let mut modules = HashSet::new();
    let mut pending = vec![ty.clone()];
    let mut seen = BTreeSet::new();
    let mut anonymous_producer = false;
    while let Some(ty) = pending.pop() {
        if !seen.insert(ty.clone()) {
            continue;
        }
        let mut declarations = Vec::new();
        if let Some(declaration) = ty.head().and_then(|head| head.declaration(&checked.defs)) {
            declarations.push(declaration.declaration());
        }
        match &ty {
            ResolvedTy::TypeParam { name } => declarations.push(name.id.owner),
            ResolvedTy::TraitObject { traits } => {
                for bound in traits {
                    if let Some(declaration) = bound.trait_id {
                        declarations.push(declaration);
                    }
                }
            }
            ResolvedTy::GenericCallable {
                parameters,
                callable,
            } => {
                pending.push((**callable).clone());
                for parameter in parameters {
                    declarations.push(parameter.parameter.id.owner);
                    for bound in &parameter.bounds {
                        if let Some(declaration) = bound.trait_id {
                            declarations.push(declaration);
                        }
                        pending.extend(bound.args.iter().cloned());
                        pending.extend(bound.assoc_bindings.iter().map(|(_, ty)| ty.clone()));
                    }
                }
            }
            ResolvedTy::Generator { producers, .. } => {
                for producer in producers {
                    match &producer.body {
                        hew_types::ty::EffectBody::Declaration(declaration)
                        | hew_types::ty::EffectBody::Generator(declaration) => {
                            declarations.push(*declaration);
                        }
                        hew_types::ty::EffectBody::GeneratorBlock(_)
                        | hew_types::ty::EffectBody::Closure(_) => anonymous_producer = true,
                    }
                }
            }
            _ => {}
        }
        modules.extend(
            declarations
                .into_iter()
                .filter_map(|declaration| checked.defs.module(declaration)),
        );
        hew_types::push_type_components(&ty, &mut pending);
    }
    if anonymous_producer {
        if let Some(graph) = &program.module_graph {
            modules.extend(
                graph
                    .modules
                    .values()
                    .flat_map(|module| &module.source_paths)
                    .filter_map(|path| checked.defs.module_for_source(path))
                    .filter(|module| snapshots.contains_key(module)),
            );
        }
    }
    modules
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
    fn live_frontend_replacing_a_function_does_not_import_its_history() {
        let directory = tempfile::tempdir().unwrap();
        let mut frontend =
            LiveFrontend::new(directory.path().to_path_buf(), FrontendOptions::default());
        let mut first_graph_size = None;
        for value in 0..3 {
            let declaration = frontend
                .prepare(
                    &compiler(),
                    &format!("fn answer() -> i64 {{ {value} }}"),
                    InputKind::Declarations,
                )
                .unwrap();
            frontend.commit(declaration, &[]).unwrap();
            let invocation = frontend
                .prepare(&compiler(), "answer()", InputKind::Expression)
                .unwrap();
            let graph_size = invocation
                .program
                .module_graph
                .as_ref()
                .unwrap()
                .modules
                .len();
            assert_eq!(graph_size, *first_graph_size.get_or_insert(graph_size));
            frontend.commit(invocation, &[]).unwrap();
        }
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

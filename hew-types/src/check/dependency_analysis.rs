//! Request-local dependency checkpoints for independent real-root checks.
//!
//! A checkpoint contains no request root declarations or root body facts. Each
//! consumer forks all mutable checker tables, reanchors the actual source root,
//! registers its own lexical imports/functions and finalizes its own output.
//! Uncertain registration order/scope cases use the ordinary complete checker.

use std::path::PathBuf;

use hew_parser::ast::{
    FnDecl, ImportSpec, Item, Param, Program, RecordKind, Spanned, TraitBound, TraitItem,
    TypeBodyItem, TypeExpr, TypeParam, VariantKind, WhereClause,
};
use hew_parser::module::{ModuleGraph, ModulePath};

use super::{Checker, LintId, LintLevel, TypeCheckOutput};

const MAX_CHECKPOINTS: usize = 4;

#[derive(Debug, PartialEq)]
struct DependencyKey {
    program: Program,
    sources: Vec<(String, String, String)>,
    registry: (Vec<PathBuf>, Option<PathBuf>),
    lint_levels: Vec<LintLevel>,
}

#[derive(Debug)]
struct DependencySeed {
    key: DependencyKey,
    checker: Checker,
    lookup_footprint: std::collections::HashSet<String>,
}

/// Request-local semantic dependency reuse. Own one cache per immutable source
/// snapshot; never update its source epoch or share it across requests.
///
/// Distinct dependency closures (including physical paths and resolver-selected
/// provenance) have distinct keys. At most four sealed checkers are retained;
/// eviction affects performance only. No root analysis is retained here.
#[derive(Debug, Default)]
pub struct DependencyAnalysisCache {
    seeds: Vec<DependencySeed>,
    reused_roots: usize,
    bootstraps: usize,
    cache_hits: usize,
}

impl DependencyAnalysisCache {
    /// Number of roots checked from a dependency checkpoint in this batch.
    #[must_use]
    pub fn reused_roots(&self) -> usize {
        self.reused_roots
    }

    /// Dependency-only checker pipelines run by this request-local cache.
    #[must_use]
    pub fn bootstraps(&self) -> usize {
        self.bootstraps
    }

    /// Real roots forked from an already retained checkpoint (excluding its
    /// first root, which paid for the dependency bootstrap).
    #[must_use]
    pub fn cache_hits(&self) -> usize {
        self.cache_hits
    }

    /// Check with the same configured, fresh checker the ordinary frontend
    /// would use. `sources` is its exact non-root diagnostic/lint source map,
    /// including raw comments; AST equality alone cannot key lint directives.
    pub fn check_program(
        &mut self,
        checker: &mut Checker,
        program: &Program,
        mut sources: Vec<(String, String, String)>,
    ) -> TypeCheckOutput {
        let Some(dependencies) = dependency_program(program) else {
            return checker.check_program(program);
        };
        let Some(registry) = checker.module_registry.dependency_analysis_context() else {
            return checker.check_program(program);
        };
        if checker.has_checked_program
            || checker.wasm_target
            || checker.repl_fragment
            || checker.entry_selection.is_some()
            || checker.test_entry_selections.is_some()
            || checker.checking_embedded_builtins
        {
            return checker.check_program(program);
        }
        sources.sort();
        let key = DependencyKey {
            program: dependencies,
            sources,
            registry,
            lint_levels: LintId::ALL
                .iter()
                .map(|id| checker.lint_levels.level(*id))
                .collect(),
        };
        let seed_index = self.seeds.iter().position(|seed| seed.key == key);
        let index = if let Some(index) = seed_index {
            index
        } else {
            let mut dependency_checker = checker.fork_dependency_state();
            // The root is explicitly empty and source-less. This bootstrap is
            // never published, and is not direct shipped-stdlib compilation.
            self.bootstraps += 1;
            dependency_checker.has_checked_program = true;
            dependency_checker.prepare_program(&key.program, true);
            dependency_checker.check_dependency_bodies(&key.program);
            if !dependency_checker.errors.is_empty() {
                return checker.check_program(program);
            }
            if self.seeds.len() == MAX_CHECKPOINTS {
                self.seeds.remove(0);
            }
            self.seeds.push(DependencySeed {
                lookup_footprint: dependency_lookup_footprint(&key),
                key,
                checker: dependency_checker,
            });
            self.seeds.len() - 1
        };
        let seed = &self.seeds[index];
        if !root_registration_is_independent(program, &seed.lookup_footprint, &seed.checker) {
            return checker.check_program(program);
        }

        let mut fork = seed.checker.fork_dependency_state();
        // Each real root owns fresh lint text and a new root identity. Existing
        // dependency IDs remain a stable prefix *within this fork*, never an
        // identity authority between separate outputs/source snapshots.
        fork.lint_sources = checker.lint_sources.clone();
        // mint_module_identities restores this table before its first query or
        // mint. Move the fork-owned prefix through that handoff, rather than
        // cloning it a second time only to discard the first copy.
        fork.seed_defs = Some(std::mem::take(&mut fork.defs));
        let root = root_registration_program(program);
        fork.mint_module_identities(&root);
        fork.mint_source_declaration_identities(&root);
        if let Some(graph) = &program.module_graph {
            let module = &graph.modules[&graph.root];
            let owner = graph.root.dotted();
            fork.module_source_paths
                .insert(owner.clone(), module.source_paths.clone());
            if let Some(sources) = graph.item_sources.get(&owner) {
                fork.module_item_sources.insert(owner, sources.clone());
            }
            for source in &module.source_paths {
                fork.source_file_span_indices.insert(source.clone(), 0);
            }
        }
        fork.collect_types(&root);
        fork.collect_declared_type_param_names(&root);
        fork.collect_functions(&root);
        fork.resolve_alias_declarations(&root);
        fork.reresolve_member_types_after_imports(&root);
        // The ordinary dependency-body pass restores this context before root
        // bodies; root-only registration above must preserve the same boundary.
        fork.current_item_source = None;
        fork.current_item_ordinal = 0;
        fork.current_module = None;
        fork.current_module_idx = 0;
        fork.check_root_bodies(program);
        let output = fork.finish_program(program, None);
        *checker = fork;
        self.reused_roots += 1;
        if seed_index.is_some() {
            self.cache_hits += 1;
        }
        output
    }
}

fn ordinary_function_or_import(item: &Item) -> bool {
    match item {
        Item::Function(function) => {
            function.attributes.is_empty()
                && function.intrinsic.is_none()
                && !signature_has_infer(function)
        }
        Item::Import(import) => import.file_path.is_none(),
        _ => false,
    }
}

fn dependency_program(program: &Program) -> Option<Program> {
    let graph = program.module_graph.as_ref()?;
    let root = graph.modules.get(&graph.root)?;
    // Directory roots and source-less/synthetic roots retain their existing
    // frontend authority. A physical root inside the dependency closure would
    // need cycle-aware reanchoring and cannot use this independent-root seed.
    if root.source_paths.len() != 1
        || !program
            .items
            .iter()
            .all(|(item, _)| ordinary_function_or_import(item))
    {
        return None;
    }
    let root_source = &root.source_paths[0];
    if crate::module_registry::canonical_stdlib_module_for_source(root_source).is_some() {
        return None;
    }
    let root_physical = root_source
        .canonicalize()
        .unwrap_or_else(|_| root_source.clone());
    let automatic_prelude = automatic_prelude_closure(program, graph);
    for (id, module) in &graph.modules {
        if *id == graph.root {
            continue;
        }
        if module
            .imports
            .iter()
            .any(|import| import.target == graph.root)
            || module.source_paths.iter().any(|source| {
                source.canonicalize().unwrap_or_else(|_| source.clone()) == root_physical
            })
        {
            return None;
        }
        let prelude_floor = automatic_prelude.contains(id)
            && !module.source_paths.is_empty()
            && module.source_paths.iter().all(|source| {
                crate::module_registry::is_canonical_stdlib_module_source(source, &id.dotted())
            });
        if (!prelude_floor
            && !module
                .items
                .iter()
                .all(|(item, _)| ordinary_function_or_import(item)))
            || module
                .items
                .iter()
                .any(|(item, _)| item_has_inferred_declaration(item))
        {
            return None;
        }
    }
    let mut dependencies = program.clone();
    dependencies.items.clear();
    dependencies.module_doc = None;
    let graph = dependencies.module_graph.as_mut()?;
    let original_root = graph.root.clone();
    let mut root = graph.modules.remove(&original_root)?;
    root.items.clear();
    root.imports.clear();
    root.source_paths.clear();
    root.doc = None;
    graph.item_sources.remove(&original_root.dotted());
    // The seed root is a private graph floor only: no real source, imports or
    // declarations survive. Rename its graph key without recomputing topology:
    // the exact non-root traversal/file-index order must remain unchanged.
    let checkpoint_root = ModulePath::root();
    if graph.modules.contains_key(&checkpoint_root) {
        return None;
    }
    root.id = checkpoint_root.clone();
    for id in &mut graph.topo_order {
        if *id == original_root {
            *id = checkpoint_root.clone();
        }
    }
    graph.root = checkpoint_root.clone();
    graph.modules.insert(checkpoint_root, root);
    Some(dependencies)
}

fn automatic_prelude_closure(
    program: &Program,
    graph: &ModuleGraph,
) -> std::collections::HashSet<ModulePath> {
    let mut closure = std::collections::HashSet::new();
    for leaf in ["builtins", "option", "result", "iter"] {
        closure.insert(ModulePath::new(["std", leaf]));
    }
    // The shared frontend injects this exact empty-selection load at a
    // synthetic span. A source-authored import cannot have a zero-length span.
    // It brings lifecycle types and their transitive imports into every root,
    // just like the four floor modules. Membership never confers std authority:
    // each selected physical source must separately prove canonical provenance.
    let injected_monitor = program.items.iter().any(|(item, span)| {
        let Item::Import(import) = item else {
            return false;
        };
        span.start == 0
            && span.end == 0
            && import.file_path.is_none()
            && import.module_alias.is_none()
            && matches!(&import.spec, Some(ImportSpec::Names(names)) if names.is_empty())
            && import
                .path
                .segments
                .iter()
                .map(|(segment, _)| segment.name.as_str())
                .eq(["std", "link_monitor"])
    });
    if !injected_monitor {
        return closure;
    }
    let mut pending = vec![ModulePath::new(["std", "link_monitor"])];
    while let Some(id) = pending.pop() {
        if !closure.insert(id.clone()) {
            continue;
        }
        if let Some(module) = graph.modules.get(&id) {
            pending.extend(module.imports.iter().map(|import| import.target.clone()));
        }
    }
    closure
}

fn root_registration_program(program: &Program) -> Program {
    let mut root = program.clone();
    if let Some(graph) = root.module_graph.as_mut() {
        graph.modules.retain(|id, _| *id == graph.root);
        graph.topo_order.retain(|id| *id == graph.root);
        graph
            .item_sources
            .retain(|owner, _| *owner == graph.root.dotted());
    }
    root
}

fn root_registration_is_independent(
    program: &Program,
    footprint: &std::collections::HashSet<String>,
    seed: &Checker,
) -> bool {
    let mut bindings = Vec::new();
    for (item, _) in &program.items {
        match item {
            Item::Function(function) => {
                bindings.push(function.name.to_string());
                // This intentionally global checker guard is populated before
                // dependency bodies in the fresh path. New root binder names
                // could suppress a dependency undefined-type error.
                if function.type_params.as_ref().is_some_and(|parameters| {
                    parameters.iter().any(|parameter| {
                        !seed
                            .declared_type_param_names
                            .contains(parameter.name.name.as_str())
                    })
                }) {
                    return false;
                }
            }
            Item::Import(import) => {
                if let Some(ImportSpec::Names(names)) = &import.spec {
                    bindings.extend(
                        names
                            .iter()
                            .map(|name| name.alias.unwrap_or(name.name).to_string()),
                    );
                } else if let Some(alias) = import.module_alias {
                    bindings.push(alias.to_string());
                } else if let Some(name) = import.path.segments.last() {
                    bindings.push(name.0.to_string());
                }
            }
            _ => return false,
        }
    }
    !bindings.iter().any(|name| {
        footprint.contains(name)
            || seed
                .builtin_fn_sigs
                .keys()
                .any(|builtin| builtin.as_str() == name)
            || super::builtin_named_type(name).is_some()
    })
}

fn dependency_lookup_footprint(key: &DependencyKey) -> std::collections::HashSet<String> {
    let mut names = std::collections::HashSet::new();
    for (routing, source, _filename) in &key.sources {
        let mut declaration_names = Vec::new();
        if let Some(graph) = &key.program.module_graph {
            for (id, module) in &graph.modules {
                if *id == graph.root {
                    continue;
                }
                let owner = id.dotted();
                for (index, (item, _)) in module.items.iter().enumerate() {
                    let path = graph
                        .item_sources
                        .get(&owner)
                        .and_then(|sources| sources.get(index))
                        .or_else(|| module.source_paths.first());
                    let Some(path) = path else {
                        continue;
                    };
                    if path.to_string_lossy() != routing.as_str() && owner != *routing {
                        continue;
                    }
                    if owner == *routing && module.source_paths.first() != Some(path) {
                        continue;
                    }
                    if let Item::Function(function) = item {
                        declaration_names.push(function.decl_span.clone());
                    }
                }
            }
        }
        // Identifiers outside declaration-name tokens conservatively include
        // dependency lookups. The compiler lexer preserves Unicode identity
        // and skips comment/literal contents, which cannot bind a root name.
        for (token, span) in hew_lexer::lex(source) {
            if let hew_lexer::Token::Identifier(name) = token {
                if !declaration_names
                    .iter()
                    .any(|decl| decl.start == span.start && decl.end == span.end)
                {
                    names.insert(name.to_string());
                }
            } else if let hew_lexer::Token::InterpolatedString(text) = token {
                collect_interpolated_identifiers(text, &mut names);
            }
        }
    }
    names
}

fn collect_interpolated_identifiers(text: &str, names: &mut std::collections::HashSet<String>) {
    // The outer literal token encloses expressions the parser checks. Remove
    // ASCII literal punctuation before lexing its complete content, so quotes
    // or comment markers in literal text cannot hide an enclosed expression.
    // This deliberately includes literal words as safe false positives, while
    // the lexer remains the authority for Unicode identifier boundaries.
    let lexable = text
        .chars()
        .map(|character| {
            if character.is_ascii() && !character.is_ascii_alphanumeric() && character != '_' {
                ' '
            } else {
                character
            }
        })
        .collect::<String>();
    for (token, _) in hew_lexer::lex(&lexable) {
        if let hew_lexer::Token::Identifier(name) = token {
            names.insert(name.to_string());
        }
    }
}

fn signature_has_infer(function: &FnDecl) -> bool {
    callable_signature_has_infer(
        &function.params,
        function.return_type.as_ref(),
        function.type_params.as_deref(),
        function.where_clause.as_ref(),
    )
}

fn callable_signature_has_infer(
    params: &[Param],
    result: Option<&Spanned<TypeExpr>>,
    generics: Option<&[TypeParam]>,
    where_clause: Option<&WhereClause>,
) -> bool {
    params
        .iter()
        .any(|parameter| type_has_infer(&parameter.ty.0))
        || result.is_some_and(|ty| type_has_infer(&ty.0))
        || type_parameters_have_infer(generics)
        || where_clause_has_infer(where_clause)
}

fn type_parameters_have_infer(parameters: Option<&[TypeParam]>) -> bool {
    parameters.is_some_and(|parameters| {
        parameters
            .iter()
            .any(|parameter| parameter.bounds.iter().any(bound_has_infer))
    })
}

fn where_clause_has_infer(clause: Option<&WhereClause>) -> bool {
    clause.is_some_and(|clause| {
        clause.predicates.iter().any(|predicate| {
            type_has_infer(&predicate.ty.0) || predicate.bounds.iter().any(bound_has_infer)
        })
    })
}

fn item_has_inferred_declaration(item: &Item) -> bool {
    match item {
        Item::Function(function) => signature_has_infer(function),
        Item::Import(_) => false,
        Item::Const(declaration) => type_has_infer(&declaration.ty.0),
        Item::TypeAlias(declaration) => {
            type_has_infer(&declaration.ty.0)
                || type_parameters_have_infer(declaration.type_params.as_deref())
        }
        Item::TypeDecl(declaration) => {
            type_parameters_have_infer(declaration.type_params.as_deref())
                || where_clause_has_infer(declaration.where_clause.as_ref())
                || declaration.body.iter().any(|item| match item {
                    TypeBodyItem::Field { ty, .. } => type_has_infer(&ty.0),
                    TypeBodyItem::Method(function) => signature_has_infer(function),
                    TypeBodyItem::Variant(variant) => match &variant.kind {
                        VariantKind::Unit => false,
                        VariantKind::Tuple(types) => types.iter().any(|ty| type_has_infer(&ty.0)),
                        VariantKind::Struct(fields) => {
                            fields.iter().any(|(_, ty)| type_has_infer(&ty.0))
                        }
                    },
                })
        }
        Item::Record(declaration) => {
            type_parameters_have_infer(declaration.type_params.as_deref())
                || where_clause_has_infer(declaration.where_clause.as_ref())
                || match &declaration.kind {
                    RecordKind::Named(fields) => {
                        fields.iter().any(|field| type_has_infer(&field.ty.0))
                    }
                    RecordKind::Tuple(types) => types.iter().any(|ty| type_has_infer(&ty.0)),
                }
        }
        Item::Trait(declaration) => {
            type_parameters_have_infer(declaration.type_params.as_deref())
                || declaration
                    .super_traits
                    .as_ref()
                    .is_some_and(|bounds| bounds.iter().any(bound_has_infer))
                || declaration.items.iter().any(|item| match item {
                    TraitItem::Method(method) => callable_signature_has_infer(
                        &method.params,
                        method.return_type.as_ref(),
                        method.type_params.as_deref(),
                        method.where_clause.as_ref(),
                    ),
                    TraitItem::AssociatedType {
                        bounds, default, ..
                    } => {
                        bounds.iter().any(bound_has_infer)
                            || default.as_ref().is_some_and(|ty| type_has_infer(&ty.0))
                    }
                })
        }
        Item::Impl(declaration) => {
            type_parameters_have_infer(declaration.type_params.as_deref())
                || declaration
                    .trait_bound
                    .as_ref()
                    .is_some_and(bound_has_infer)
                || type_has_infer(&declaration.target_type.0)
                || where_clause_has_infer(declaration.where_clause.as_ref())
                || declaration
                    .type_aliases
                    .iter()
                    .any(|alias| type_has_infer(&alias.ty.0))
                || declaration.methods.iter().any(signature_has_infer)
        }
        Item::ExternBlock(declaration) => declaration.functions.iter().any(|function| {
            callable_signature_has_infer(
                &function.params,
                function.return_type.as_ref(),
                None,
                None,
            )
        }),
        // These declaration families require additional preparation/normalizing
        // contracts; even an automatic dependency conservatively uses the full
        // checker rather than treating an unvisited signature as explicit.
        Item::Actor(_) | Item::Supervisor(_) | Item::Machine(_) => true,
    }
}

fn bound_has_infer(bound: &TraitBound) -> bool {
    bound
        .type_args
        .as_ref()
        .is_some_and(|arguments| arguments.iter().any(|argument| type_has_infer(&argument.0)))
        || bound
            .assoc_type_bindings
            .iter()
            .any(|binding| type_has_infer(&binding.ty.0))
}

fn type_has_infer(ty: &TypeExpr) -> bool {
    match ty {
        TypeExpr::Infer => true,
        TypeExpr::Named { type_args, .. } => type_args
            .as_ref()
            .is_some_and(|arguments| arguments.iter().any(|argument| type_has_infer(&argument.0))),
        TypeExpr::QualifiedAssocPath(path) => type_has_infer(&path.base.0),
        TypeExpr::Fallible { success, error } => {
            type_has_infer(&success.0) || type_has_infer(&error.0)
        }
        TypeExpr::Result { ok, err } => type_has_infer(&ok.0) || type_has_infer(&err.0),
        TypeExpr::Option(inner) | TypeExpr::Slice(inner) | TypeExpr::Borrow(inner) => {
            type_has_infer(&inner.0)
        }
        TypeExpr::Tuple(items) => items.iter().any(|item| type_has_infer(&item.0)),
        TypeExpr::Array { element, .. } => type_has_infer(&element.0),
        TypeExpr::Function {
            params,
            return_type,
            ..
        }
        | TypeExpr::ActorFn {
            params,
            return_type,
        } => {
            params.iter().any(|parameter| type_has_infer(&parameter.0))
                || type_has_infer(&return_type.0)
        }
        TypeExpr::Pointer { pointee, .. } => type_has_infer(&pointee.0),
        TypeExpr::TraitObject(bounds) => bounds.iter().any(bound_has_infer),
    }
}

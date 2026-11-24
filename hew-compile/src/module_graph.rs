//! Module-graph assembly, prelude module attachment, cycle reporting, and macro expansion.

use std::collections::{BTreeMap, HashMap, HashSet};
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use hew_parser::ast::{ImportDecl, Item, Program, Spanned};
use hew_parser::module::ModulePath;

use crate::diagnostics::{display_path, FrontendDiagnostic, FrontendFailure, FrontendMessageNote};
use crate::frontend_options::FrontendOptions;
use crate::manifest::ProjectContext;
use crate::module_loader::{
    anchor_for, canonical_direct_stdlib_module_for_source, comparable_std_roots, import_segments,
    module_id_from_file, resolve_file_imports_internal, resolve_prelude_std_source,
    undeclared_imports, ImportResolutionContext,
};
use crate::source_set::{read_source, resolve_candidate, SourceSet};

pub(crate) fn resolve_imports_internal(
    program: &mut Program,
    source: &str,
    input: &str,
    project: &ProjectContext,
    options: &FrontendOptions,
    diagnostics: &mut Vec<FrontendDiagnostic>,
) -> Result<(), FrontendFailure> {
    inject_implicit_imports(&mut program.items, source);

    let input_path = Path::new(input);
    let std_roots = options.std_roots();
    inject_prelude_module_loads(&mut program.items, input_path, &std_roots);
    let mut import_ctx = ImportResolutionContext {
        in_progress_imports: HashSet::new(),
        resolved_imports: HashMap::new(),
        manifest_deps: project.manifest_deps.as_deref(),
        extra_pkg_path: options.pkg_path.as_deref(),
        locked_versions: project.locked_versions.as_deref(),
        package_name: project.package_name.as_deref(),
        project_dir: &project.project_dir,
        std_roots: &std_roots,
        documents: &options.documents,
    };
    let input_canonical = resolve_candidate(&options.documents, input_path)
        .unwrap_or_else(|| input_path.to_path_buf());
    let undeclared = undeclared_imports(&input_canonical, &program.items, &import_ctx)?;
    if !undeclared.is_empty() {
        return Err(FrontendFailure::new(
            "undeclared dependencies",
            undeclared
                .into_iter()
                .map(FrontendDiagnostic::message)
                .collect(),
        ));
    }
    let module_graph = build_module_graph_with_diagnostics(
        input_path,
        &mut program.items,
        program.module_doc.clone(),
        &mut import_ctx,
        diagnostics,
    )?;
    program.module_graph = Some(module_graph);
    let mut sources = BTreeMap::new();
    for module in program
        .module_graph
        .as_ref()
        .expect("graph was built")
        .modules
        .values()
    {
        for path in &module.source_paths {
            if let Ok(text) = read_source(&options.documents, path) {
                sources.insert(path.clone(), text.to_string());
            }
        }
    }
    sources.insert(input_canonical, source.to_string());
    if !options.documents.prepared_expansions.templates.is_empty()
        || !options.documents.prepared_expansions.expansions.is_empty()
    {
        program.expansions = (*options.documents.prepared_expansions).clone();
    }
    if let Err((path, error)) = hew_expand::expand_program(program, sources) {
        let text = options
            .documents
            .admitted_text(&path)
            .unwrap_or_else(|| Arc::from(source));
        return Err(FrontendFailure::new(
            "macro expansion failed",
            vec![FrontendDiagnostic::parse(text, &display_path(&path), error)],
        ));
    }
    Ok(())
}

pub fn inject_implicit_imports(items: &mut Vec<Spanned<Item>>, source: &str) {
    let existing = items
        .iter()
        .filter_map(|(item, _)| {
            if let Item::Import(decl) = item {
                if !decl.path.segments.is_empty() {
                    return Some(import_segments(&decl.path).join("::"));
                }
            }
            None
        })
        .collect::<HashSet<_>>();

    let mut needed: Vec<&[&str]> = Vec::new();
    if source_contains_regex_literal(source) {
        let path: &[&str] = &["std", "text", "regex"];
        let key = path.join("::");
        if !existing.contains(&key) {
            needed.push(path);
        }
    }

    let mut seen = HashSet::new();
    for path in needed {
        let key = path.join("::");
        if seen.insert(key) {
            items.push((
                Item::Import(ImportDecl {
                    path: hew_parser::ast::Path::from_spellings(path),
                    spec: None,
                    selection_trailing_comma: false,
                    module_alias: None,
                    file_path: None,
                    resolved_items: None,
                    resolved_item_source_paths: Vec::new(),
                    resolved_source_paths: Vec::new(),
                }),
                0..0,
            ));
        }
    }
}

/// Load the modules whose declarations the prelude publishes. `monitor`
/// returns a `MonitorRef`, so `std.link_monitor` belongs to every program that
/// could name one; the import selects no names, so it loads the declarations
/// without binding anything. A standard-library module checked directly is
/// the prelude's own floor and loads nothing: `std.link_monitor` would import
/// itself, and `std.failure` would close a cycle through it. Stream's ordinary
/// source methods also belong to every program, including programs that obtain
/// a Stream from an actor without importing std.stream. Loading it through the
/// normal resolver retains its transitive codec declarations and identities.
fn inject_prelude_module_loads(
    items: &mut Vec<Spanned<Item>>,
    input: &Path,
    std_roots: &[PathBuf],
) {
    if canonical_direct_stdlib_module_for_source(input, std_roots).is_some() {
        return;
    }
    for leaf in hew_types::stdlib_authority::IMPLICIT_PRELUDE_MODULE_LOADS {
        let path = ["std", leaf];
        let already_imported = items.iter().any(
            |(item, _)| matches!(item, Item::Import(decl) if import_segments(&decl.path) == path),
        );
        if already_imported {
            continue;
        }
        items.push((
            Item::Import(ImportDecl {
                path: hew_parser::ast::Path::from_spellings(&path),
                spec: Some(hew_parser::ast::ImportSpec::Names(Vec::new())),
                selection_trailing_comma: false,
                module_alias: None,
                file_path: None,
                resolved_items: None,
                resolved_item_source_paths: Vec::new(),
                resolved_source_paths: Vec::new(),
            }),
            0..0,
        ));
    }
}

fn source_contains_regex_literal(source: &str) -> bool {
    hew_lexer::Lexer::new(source)
        .any(|(token, _)| matches!(token, hew_lexer::Token::RegexLiteral(_)))
}

/// Render a module-graph [`CycleError`](hew_parser::module::CycleError) into a
/// positioned diagnostic: the first edge on the cycle path becomes the
/// diagnostic's primary location, every remaining edge becomes a note in path
/// order (each pointing into the module that declares that import), and a
/// help line steers the fix.
///
/// A cycle where every module's entry file lives in the same directory is the
/// directory-module shape described in spec 3.5.1 — the fix is to promote
/// that directory to a directory module rather than importing between its
/// files. That needs the directory to lie strictly below a package or std
/// anchor: the anchor directory itself holds otherwise unrelated top-level
/// modules, and a loose directory first needs a package. Otherwise the fix is
/// a shared module both sides import.
///
/// Falls back to the bare chain message (former behaviour) if a cycle member
/// is missing from `graph` or its source file cannot be re-read; both should
/// be unreachable since every cycle member was inserted into `graph` before
/// `compute_topo_order` ran and its source was just parsed.
fn cycle_error_to_frontend_failure(
    graph: &hew_parser::module::ModuleGraph,
    cycle_err: &hew_parser::module::CycleError,
    std_roots: &[PathBuf],
    documents: &SourceSet,
) -> FrontendFailure {
    let chain = cycle_err.to_string();
    let edge_count = cycle_err.import_spans.len();

    let mut locations: Vec<(PathBuf, Arc<str>, Range<usize>, String)> =
        Vec::with_capacity(edge_count);
    for i in 0..edge_count {
        let from_module = &cycle_err.cycle[i];
        let to_module = &cycle_err.cycle[i + 1];
        let Some(source_path) = graph
            .modules
            .get(from_module)
            .and_then(|module| module.source_paths.first())
        else {
            return FrontendFailure::message_only(chain);
        };
        let Ok(source) = read_source(documents, source_path) else {
            return FrontendFailure::message_only(chain);
        };
        let label = match (i == 0, i + 1 == edge_count) {
            (true, true) => format!(
                "import cycle: `{from_module}` imports `{to_module}`, closing the cycle on itself"
            ),
            (true, false) => format!("import cycle: `{from_module}` imports `{to_module}` here"),
            (false, true) => {
                format!("`{from_module}` imports `{to_module}` here, closing the cycle")
            }
            (false, false) => format!("`{from_module}` imports `{to_module}` here"),
        };
        locations.push((
            source_path.clone(),
            source,
            cycle_err.import_spans[i].clone(),
            label,
        ));
    }

    let shared_dir = locations[0].0.parent();
    let same_directory = shared_dir.is_some()
        && locations
            .windows(2)
            .all(|pair| pair[0].0.parent() == pair[1].0.parent());
    let anchor = anchor_for(&locations[0].0, std_roots, documents)
        .unwrap_or(hew_types::module_registry::ModuleAnchor::Loose);
    let below_anchor = anchor.dir().is_some_and(|dir| {
        shared_dir.is_some_and(|shared| shared != dir && shared.starts_with(dir))
    });
    let is_loose = matches!(anchor, hew_types::module_registry::ModuleAnchor::Loose);
    let help = if same_directory && (below_anchor || is_loose) {
        let dir_name = locations[0]
            .0
            .parent()
            .and_then(Path::file_name)
            .and_then(|name| name.to_str())
            .unwrap_or("<dir>");
        let package = if is_loose {
            "; directory modules belong to a package, so run `hew init` first"
        } else {
            ""
        };
        format!(
            "these modules share one directory; make `{dir_name}/{dir_name}.hew` the entry and \
             let the others be peers (spec 3.5.1), then drop the imports between them{package}"
        )
    } else {
        "move the shared declarations into a module both sides import".to_string()
    };

    let (first_path, first_source, first_span, first_label) = locations[0].clone();
    let notes = locations[1..]
        .iter()
        .map(|(path, source, span, label)| FrontendMessageNote {
            message: label.clone(),
            span: span.clone(),
            source: Arc::clone(source),
            filename: path.display().to_string(),
        })
        .collect();

    FrontendFailure::coded_message_with_notes(
        "E_IMPORT_CYCLE",
        first_label,
        first_span,
        &first_source,
        &first_path.display().to_string(),
        notes,
        vec![help],
    )
}

fn rewrite_direct_stdlib_module_root(
    module_graph: &mut hew_parser::module::ModuleGraph,
    items: &mut Vec<Spanned<Item>>,
    source_file: &Path,
    std_roots: &[PathBuf],
    documents: &SourceSet,
) -> Result<(), FrontendFailure> {
    use hew_parser::module::Module;

    let Some(stdlib_id) = canonical_direct_stdlib_module_for_source(source_file, std_roots) else {
        return Ok(());
    };

    let original_root = module_graph.root.clone();
    let Some(mut stdlib_module) = module_graph.modules.remove(&original_root) else {
        return Ok(());
    };

    stdlib_module.id = stdlib_id.clone();
    module_graph.root = ModulePath::root();
    module_graph.modules.insert(stdlib_id, stdlib_module);
    module_graph
        .add_module(Module {
            id: module_graph.root.clone(),
            items: Vec::new(),
            imports: Vec::new(),
            source_paths: Vec::new(),
            doc: None,
        })
        .expect("synthetic floor-check root is unique");
    module_graph.compute_topo_order().map_err(|cycle_err| {
        cycle_error_to_frontend_failure(module_graph, &cycle_err, std_roots, documents)
    })?;
    items.clear();

    Ok(())
}

fn build_module_graph_with_diagnostics(
    source_file: &Path,
    items: &mut Vec<Spanned<Item>>,
    module_doc: Option<String>,
    ctx: &mut ImportResolutionContext<'_>,
    diagnostics: &mut Vec<FrontendDiagnostic>,
) -> Result<hew_parser::module::ModuleGraph, FrontendFailure> {
    use hew_parser::module::{Module, ModuleGraph};

    let input_canonical =
        resolve_candidate(ctx.documents, source_file).unwrap_or_else(|| source_file.to_path_buf());
    let source_dir = input_canonical.parent().unwrap_or(Path::new("."));

    ctx.in_progress_imports.insert(input_canonical.clone());
    let resolve_result = resolve_file_imports_internal(&input_canonical, items, ctx, diagnostics);
    ctx.in_progress_imports.remove(&input_canonical);
    resolve_result?;

    let root_id = module_id_from_file(source_dir, &input_canonical);
    let mut graph = ModuleGraph::new(root_id.clone());
    let mut seen_ids: HashSet<ModulePath> = HashSet::from([root_id.clone()]);

    let root_imports = extract_module_info(
        items,
        &input_canonical,
        source_dir,
        &input_canonical,
        &root_id,
        ctx.documents,
        ctx.std_roots,
        &mut graph,
        &mut seen_ids,
    );

    let root_module = Module {
        id: root_id,
        items: items.clone(),
        imports: root_imports,
        source_paths: vec![input_canonical.clone()],
        doc: module_doc,
    };
    graph
        .add_module(root_module)
        .expect("root module id is unique");

    if let Err(cycle_err) = graph.compute_topo_order() {
        return Err(cycle_error_to_frontend_failure(
            &graph,
            &cycle_err,
            &comparable_std_roots(ctx.std_roots),
            ctx.documents,
        ));
    }

    add_prelude_std_modules(&mut graph, |name| {
        resolve_prelude_std_source(ctx, name).map(|(path, source)| (Some(path), source))
    })?;

    rewrite_direct_stdlib_module_root(
        &mut graph,
        items,
        &input_canonical,
        &comparable_std_roots(ctx.std_roots),
        ctx.documents,
    )?;

    // Canonical module IDs may share a final component. Reject only when two
    // whole-module imports in the SAME source scope publish the same surface
    // binding for different canonical paths. Distinct module aliases are
    // unambiguous; named/glob symbol bindings remain checker-owned.
    if let Err(msg) = check_ambiguous_module_import_bindings(&graph) {
        return Err(FrontendFailure::message_only(msg));
    }

    // Reject a single module declaring two actors with one name.  Cross-module
    // duplicates are LEGAL: actor identity is the qualified (defining-module,
    // name) pair end-to-end — the checker emits `bank.Account`'s own
    // actor-handle type, MIR
    // layouts key on the dotted name, and native symbols mangle through
    // `bank$Account` — so `spawn bank.Account { ... }` and `spawn
    // store.Account(...)` bind their own handlers/state/drop glue.  Within one
    // module there is no qualifier left to tell two same-named actors apart,
    // so that case stays a hard error.
    if let Err(msg) = check_duplicate_actor_layout_names(&graph) {
        return Err(FrontendFailure::message_only(msg));
    }

    Ok(graph)
}

/// Give a single-source program, parsed without import resolution, the same
/// standard-library modules [`build_module_graph`] adds to every program, so
/// an editor analysis lowers builtin-type methods exactly as a build does. It
/// has no module search path, so the sources are the ones compiled in.
///
/// # Panics
///
/// Never in practice: the root module is added to a freshly created graph,
/// and the compiled-in prelude sources parse.
pub fn attach_prelude_std_modules(program: &mut Program) {
    use hew_parser::module::{Module, ModuleGraph};
    let graph = program.module_graph.get_or_insert_with(|| {
        let root = ModulePath::root();
        let mut graph = ModuleGraph::new(root.clone());
        graph
            .add_module(Module {
                id: root.clone(),
                items: program.items.clone(),
                imports: Vec::new(),
                source_paths: Vec::new(),
                doc: program.module_doc.clone(),
            })
            .expect("a fresh graph has no root module");
        graph.topo_order.push(root);
        graph
    });
    add_prelude_std_modules(graph, |name| {
        let (_, source) = hew_types::stdlib_authority::IMPLICIT_PRELUDE_STD_SOURCES
            .iter()
            .find(|(leaf, _)| *leaf == name)
            .expect("every prelude module has a compiled-in source");
        Ok((None, Arc::from(*source)))
    })
    .expect("the compiled-in prelude sources parse");
}

/// Methods on builtin types are always available, like `.len()`: the
/// standard-library modules that own them join every program as ordinary
/// modules unless the program already imports them. A filesystem-selected
/// builtins source enters the graph in full, allowing the checker and HIR to
/// share its declaration occurrences. When analysis uses compiled-in sources,
/// the checker's embedded builtin registration already owns those declarations.
fn add_prelude_std_modules(
    graph: &mut hew_parser::module::ModuleGraph,
    mut source_for: impl FnMut(&str) -> Result<(Option<PathBuf>, Arc<str>), FrontendFailure>,
) -> Result<(), FrontendFailure> {
    use hew_parser::module::Module;
    for (name, _) in hew_types::stdlib_authority::IMPLICIT_PRELUDE_STD_SOURCES {
        let id = ModulePath::new(["std", name]);
        if graph.modules.contains_key(&id) {
            continue;
        }
        let (path, source) = source_for(name)?;
        if name == "builtins" && path.is_none() {
            continue;
        }
        let filename = path
            .as_ref()
            .map_or_else(|| format!("std/{name}.hew"), |path| display_path(path));
        let parsed = hew_parser::parse(&source);
        if parsed
            .errors
            .iter()
            .any(|error| error.severity == hew_parser::Severity::Error)
        {
            return Err(FrontendFailure::new(
                format!("Error: the standard library source {filename} does not parse"),
                parsed
                    .errors
                    .into_iter()
                    .map(|error| FrontendDiagnostic::parse(Arc::clone(&source), &filename, error))
                    .collect(),
            ));
        }
        graph
            .add_module(Module {
                id: id.clone(),
                items: parsed.program.items,
                imports: Vec::new(),
                source_paths: path.into_iter().collect(),
                doc: None,
            })
            .expect("prelude module absence was checked");
        graph.topo_order.push(id);
    }
    Ok(())
}

fn check_ambiguous_module_import_bindings(
    graph: &hew_parser::module::ModuleGraph,
) -> Result<(), String> {
    for (owner_id, module) in &graph.modules {
        let mut seen: HashMap<String, String> = HashMap::new();
        for (item, _) in &module.items {
            let Item::Import(import) = item else {
                continue;
            };
            if import.path.segments.is_empty() || import.spec.is_some() {
                continue;
            }
            let source = import_segments(&import.path).join(".");
            let binding = import
                .module_alias
                .or_else(|| import.path.last())
                .expect("non-file module imports have a path");
            if let Some(existing) = seen.insert(binding.to_string(), source.clone()) {
                if existing != source {
                    return Err(format!(
                        "Error: module `{}` imports both `{existing}` and `{source}` \
                         under the ambiguous binding `{binding}`. \
                         Give one import a distinct module alias.",
                        owner_id.to_string().replace("::", ".")
                    ));
                }
            }
        }
    }
    Ok(())
}

/// Reject a single module (or the root program) declaring two actors with
/// the same name.
///
/// Actor identity is the qualified `(defining-module, name)` pair, so
/// same-named actors from DIFFERENT modules are legal and keep distinct
/// layouts, handle types, and native symbols.  Within one module the
/// qualified identities collide — `bank.Account` twice — and no spawn
/// spelling could tell them apart, so that shape stays a hard error.
fn check_duplicate_actor_layout_names(
    graph: &hew_parser::module::ModuleGraph,
) -> Result<(), String> {
    for mod_id in &graph.topo_order {
        let Some(module) = graph.modules.get(mod_id) else {
            continue;
        };
        let mut seen: HashSet<&str> = HashSet::new();
        for (item, _) in &module.items {
            let Item::Actor(actor) = item else { continue };
            if !seen.insert(actor.name.name.as_str()) {
                let owner = describe_actor_module(mod_id, graph);
                return Err(format!(
                    "Error: {owner} declares two actors named `{}`; the \
                     qualified actor identity is (module, name), so two \
                     declarations in one module cannot be told apart. Rename \
                     one of the actors.",
                    actor.name
                ));
            }
        }
    }
    Ok(())
}

/// Render a module id for the duplicate-actor diagnostic, naming the root
/// program explicitly instead of the bare `(root)` placeholder.
fn describe_actor_module(
    id: &hew_parser::module::ModulePath,
    graph: &hew_parser::module::ModuleGraph,
) -> String {
    if *id == graph.root {
        "the root program".to_string()
    } else {
        format!("module `{id}`")
    }
}

/// Resolve imports and build a module graph rooted at `source_file`.
///
/// # Errors
///
/// Returns [`FrontendFailure`] when import resolution or cycle detection fails.
pub fn build_module_graph(
    source_file: &Path,
    items: &mut Vec<Spanned<Item>>,
    module_doc: Option<String>,
    ctx: &mut ImportResolutionContext<'_>,
) -> Result<hew_parser::module::ModuleGraph, FrontendFailure> {
    let mut diagnostics = Vec::new();
    build_module_graph_with_diagnostics(source_file, items, module_doc, ctx, &mut diagnostics)
}

/// The graph node already assembled from `source`, if the walk reached that
/// file under an earlier spelling. The two spellings arrive through different
/// candidate roots, but import resolution records every resolved source path
/// canonically, so the paths compare directly.
fn graph_module_for_source(
    graph: &hew_parser::module::ModuleGraph,
    source: &Path,
) -> Option<hew_parser::module::ModulePath> {
    graph
        .modules
        .iter()
        .find(|(_, module)| module.source_paths.first().map(PathBuf::as_path) == Some(source))
        .map(|(module_id, _)| module_id.clone())
}

#[expect(
    clippy::too_many_arguments,
    reason = "one module-graph frame: the source being walked, its directory, \
              the root it belongs to, the open documents, the std roots, and the \
              graph and seen-id state the walk threads; grouping them would hide \
              which of the three paths each argument comes from"
)]
fn extract_module_info(
    items: &[Spanned<Item>],
    current_source: &Path,
    source_dir: &Path,
    root_source: &Path,
    root_id: &hew_parser::module::ModulePath,
    documents: &SourceSet,
    std_roots: &[PathBuf],
    graph: &mut hew_parser::module::ModuleGraph,
    seen_ids: &mut HashSet<hew_parser::module::ModulePath>,
) -> Vec<hew_parser::module::ModuleImport> {
    use hew_parser::module::{Module, ModuleImport};

    let mut imports = Vec::new();

    for (item, span) in items {
        let Item::Import(decl) = item else { continue };

        let (module_id, first_source_path) = if !decl.path.segments.is_empty() {
            // One source is one module, however the import spelled it: a
            // package-qualified `probe.lib` and a directory-relative `lib`
            // reach the same file, and a second graph node would have the
            // checker register those declarations twice under two owners.
            let existing = decl
                .resolved_source_paths
                .first()
                .and_then(|source| graph_module_for_source(graph, source));
            let module_id = existing.unwrap_or_else(|| {
                hew_types::module_registry::canonical_source_module_identity(
                    &ModulePath::new(import_segments(&decl.path)),
                    &decl.resolved_source_paths,
                    std_roots,
                )
            });
            (module_id, None)
        } else if let Some(file_path) = &decl.file_path {
            let resolved = current_source
                .parent()
                .unwrap_or(source_dir)
                .join(file_path);
            let canonical = resolve_candidate(documents, &resolved).unwrap_or(resolved);
            let module_id = if canonical == root_source {
                root_id.clone()
            } else {
                module_id_from_file(source_dir, &canonical)
            };
            (module_id, Some(canonical))
        } else {
            continue;
        };

        imports.push(ModuleImport {
            target: module_id.clone(),
            spec: decl.spec.clone(),
            span: span.clone(),
        });

        if seen_ids.insert(module_id.clone()) {
            if let Some(resolved) = &decl.resolved_items {
                let child_source = first_source_path.as_deref().unwrap_or(current_source);
                let child_imports = extract_module_info(
                    resolved,
                    child_source,
                    source_dir,
                    root_source,
                    root_id,
                    documents,
                    std_roots,
                    graph,
                    seen_ids,
                );
                let source_paths = if decl.resolved_source_paths.is_empty() {
                    first_source_path.into_iter().collect()
                } else {
                    decl.resolved_source_paths.clone()
                };
                // Per-item file attribution for directory-assembled modules:
                // `resolved_item_source_paths` is built parallel to the
                // resolved items, so record it only when that parallelism
                // holds (an absent entry means "first source path").
                if decl.resolved_item_source_paths.len() == resolved.len() {
                    graph
                        .item_sources
                        .insert(module_id.dotted(), decl.resolved_item_source_paths.clone());
                }
                let module = Module {
                    id: module_id,
                    items: resolved.as_ref().clone(),
                    imports: child_imports,
                    source_paths,
                    doc: None,
                };
                graph
                    .add_module(module)
                    .expect("seen_ids prevents duplicate insertion");
            }
        }
    }

    imports
}

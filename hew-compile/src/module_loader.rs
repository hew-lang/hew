//! Module membership, package/stdlib anchor resolution, and recursive file import loading.

use std::collections::{HashMap, HashSet};
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use hew_parser::ast::{ImportDecl, Item, Spanned};
use hew_parser::module::ModulePath;

use crate::compile_time;
use crate::diagnostics::{display_path, FrontendDiagnostic, FrontendFailure};
use crate::frontend_options::FrontendOptions;
use crate::manifest::{
    load_lockfile_from, load_manifest_from, load_manifest_metadata_from,
    verify_locked_project_package, LockedPackageCheck,
};
use crate::source_set::{read_source, resolve_candidate, SourceSet};

#[derive(Clone, Debug)]
pub struct ResolvedImport {
    pub(crate) items: std::sync::Arc<Vec<Spanned<Item>>>,
    pub(crate) item_source_paths: Vec<PathBuf>,
    pub(crate) source_paths: Vec<PathBuf>,
}

#[derive(Debug)]
pub struct ImportResolutionContext<'a> {
    pub in_progress_imports: HashSet<PathBuf>,
    pub resolved_imports: HashMap<PathBuf, ResolvedImport>,
    pub manifest_deps: Option<&'a [String]>,
    pub extra_pkg_path: Option<&'a Path>,
    pub locked_versions: Option<&'a [(String, String)]>,
    pub package_name: Option<&'a str>,
    pub project_dir: &'a Path,
    /// The run's standard-library roots, from [`FrontendOptions::std_roots`].
    pub std_roots: &'a [PathBuf],
    /// Open buffers that override on-disk content while resolving imports.
    pub documents: &'a SourceSet,
}

/// The module imports of a package's root file that its manifest does not
/// declare and nothing local supplies.
///
/// An import that resolves from the standard library, from the package's
/// own name or beside the importing file needs no declaration; anything else
/// must be a declared dependency, whether or not it is installed.
pub(crate) fn undeclared_imports(
    source_file: &Path,
    items: &[Spanned<Item>],
    ctx: &ImportResolutionContext<'_>,
) -> Result<Vec<String>, FrontendFailure> {
    let Some(manifest_deps) = ctx.manifest_deps else {
        return Ok(Vec::new());
    };
    let std_roots = comparable_std_roots(ctx.std_roots);
    let anchor = importer_anchor(source_file, ctx, &std_roots)?;
    let mut errors = Vec::new();
    for (item, _) in items {
        let Item::Import(decl) = item else { continue };
        if decl.file_path.is_some() || decl.path.segments.is_empty() {
            continue;
        }
        let segments = import_segments(&decl.path);
        // The package's own name is local: a module it does not have is
        // reported as not found, not as an undeclared dependency.
        let names_own_package = matches!(&anchor,
            hew_types::module_registry::ModuleAnchor::Package { name: Some(package), .. }
                if package_relative(&decl.path, package).is_some());
        if names_own_package
            || is_builtin_module(&segments.join("::"))
            || declares_dependency(Some(manifest_deps), &segments)
        {
            continue;
        }
        let resolves_locally = module_candidates(&decl.path, source_file, &anchor, ctx, &std_roots)
            .candidates
            .iter()
            .any(|entry| {
                entry.2 != CandidateRoot::Dependency
                    && resolve_module_candidate(ctx, &std_roots, entry)
                        .is_ok_and(|found| found.is_some())
            });
        if !resolves_locally {
            let source_module = segments.join(".");
            errors.push(format!(
                "Error: module `{source_module}` is not declared in hew.toml\n  hint: add it with `hew add {source_module}`"
            ));
        }
    }
    Ok(errors)
}

fn is_builtin_module(module_path: &str) -> bool {
    module_path.starts_with("std::")
        || module_path.starts_with("hew::")
        || module_path.starts_with("ecosystem::")
}

/// The closest existing `std` module to a mistyped import's last path
/// segment: an exact leaf match (`std.server` -> `std.net.http.server`, a missed or
/// mis-nested path segment) when one exists, else a near-miss typo
/// (`std.servr` -> `std.net.http.server`).
///
/// Near-miss matching compares leaf names (the last dotted segment), not
/// full dotted paths: `std.servr` and `std.net.http.server` differ by far more than
/// [`hew_types::error::find_similar`]'s length-scaled Levenshtein threshold
/// allows, but `htp` and `http` — the part the user actually mistyped — are
/// one edit apart. An exact leaf match is checked first because
/// `find_similar` filters exact matches out entirely (it exists to catch
/// spelling, not path mistakes) — without this, the single most likely
/// real-world slip (the right module name, wrong or missing nesting) would
/// get no suggestion at all. Returns `None` when neither finds anything.
fn nearest_std_module(leaf: &str, search_paths: &[PathBuf]) -> Option<String> {
    let mut modules = Vec::new();
    for root in search_paths {
        let std_dir = root.join("std");
        if std_dir.is_dir() {
            collect_std_module_names(&std_dir, &mut modules);
        }
    }
    let module_leaf = |module: &ModulePath| module.segments.last().map_or("", |leaf| leaf.as_str());
    if let Some(exact) = modules.iter().find(|module| module_leaf(module) == leaf) {
        return Some(exact.dotted());
    }
    let leaves: Vec<&str> = modules.iter().map(module_leaf).collect();
    let best_leaf = hew_types::error::find_similar(leaf, leaves.iter().copied())
        .into_iter()
        .next()?;
    modules
        .iter()
        .find(|module| module_leaf(module) == best_leaf)
        .map(ModulePath::dotted)
}

/// Recursively collect every `std.…` module path reachable under `std_dir`
/// into `out`.
///
/// Mirrors the two entry-file shapes the import resolver's own per-import
/// candidate list already assumes (`dir_path`/`rel_path` above: a directory
/// names its module through a same-named `<dir>/<dir-name>.hew`, or a flat
/// `<name>.hew` file sits directly at that path) — this walks the whole
/// tree once instead of probing one path, so "did you mean" has a full
/// candidate list to compare a typo against. Cold path (only runs once
/// import resolution has already failed), so no caching: the stdlib tree is
/// a few hundred files, and this only walks it on a diagnostic.
fn collect_std_module_names(std_dir: &Path, out: &mut Vec<ModulePath>) {
    let mut segments = vec!["std".to_string()];
    collect_module_names_at(std_dir, &mut segments, out);
}

fn collect_module_names_at(dir: &Path, segments: &mut Vec<String>, out: &mut Vec<ModulePath>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            let Some(name) = path.file_name().and_then(std::ffi::OsStr::to_str) else {
                continue;
            };
            segments.push(name.to_string());
            collect_module_names_at(&path, segments, out);
            segments.pop();
        } else if path.extension().is_some_and(|ext| ext == "hew") {
            let Some(stem) = path.file_stem().and_then(std::ffi::OsStr::to_str) else {
                continue;
            };
            if segments.last().map(String::as_str) == Some(stem) {
                // `<dir>/<dir-name>.hew` — the directory's own canonical entry.
                out.push(ModulePath::new(segments.iter()));
            } else {
                // A flat file: its own name is the trailing path segment.
                segments.push(stem.to_string());
                out.push(ModulePath::new(segments.iter()));
                segments.pop();
            }
        }
    }
}

pub(crate) fn file_import(file_path: String) -> Spanned<Item> {
    (
        Item::Import(ImportDecl {
            path: hew_parser::ast::Path {
                segments: Vec::new(),
            },
            spec: None,
            selection_trailing_comma: false,
            module_alias: None,
            file_path: Some(file_path),
            resolved_items: None,
            resolved_item_source_paths: Vec::new(),
            resolved_source_paths: Vec::new(),
        }),
        0..0,
    )
}

/// A quoted file's identity is relative to the root source directory. Parent
/// steps and exceptional filesystem components occupy disjoint namespaces,
/// while ordinary Hew file and directory names remain readable.
pub(crate) fn module_id_from_file(
    source_dir: &Path,
    canonical_path: &Path,
) -> hew_parser::module::ModulePath {
    use std::path::Component;

    let base: Vec<_> = source_dir.components().collect();
    let target: Vec<_> = canonical_path.components().collect();
    // Compare the full filename: stripping its extension here would confuse
    // a sibling `app.hew` with the root directory named `app`.
    let shared = base.iter().zip(&target).take_while(|(a, b)| a == b).count();
    let mut segments: Vec<String> = if shared == 0 {
        Vec::new()
    } else {
        base[shared..]
            .iter()
            .filter(|component| matches!(component, Component::Normal(_)))
            .map(|_| "super".to_string())
            .collect()
    };
    for (index, component) in target.iter().enumerate().skip(shared) {
        let segment = match component {
            Component::Normal(name) if index + 1 == target.len() => {
                if canonical_path
                    .extension()
                    .is_some_and(|extension| extension == "hew")
                {
                    file_module_segment(canonical_path.file_stem().expect("source filename"))
                } else {
                    escaped_file_module_segment("source", name)
                }
            }
            Component::Normal(name) => file_module_segment(name),
            Component::Prefix(prefix) => escaped_file_module_segment("volume", prefix.as_os_str()),
            Component::RootDir => "__hew_file_root".to_string(),
            Component::ParentDir => "super".to_string(),
            Component::CurDir => continue,
        };
        segments.push(segment);
    }
    hew_parser::module::ModulePath::new(segments)
}

fn file_module_segment(component: &std::ffi::OsStr) -> String {
    if let Some(name) = component.to_str() {
        let mut tokens = hew_lexer::Lexer::new(name);
        if name != "super"
            && !name.starts_with("__hew_file_")
            && matches!(tokens.next(), Some((hew_lexer::Token::Identifier(_), span)) if span.start == 0 && span.end == name.len())
            && tokens.next().is_none()
        {
            return name.to_string();
        }
    }
    escaped_file_module_segment("name", component)
}

fn escaped_file_module_segment(kind: &str, component: &std::ffi::OsStr) -> String {
    use std::fmt::Write;

    let mut segment = format!("__hew_file_{kind}_");
    for byte in component.as_encoded_bytes() {
        write!(segment, "{byte:02x}").expect("writing to a String cannot fail");
    }
    segment
}

/// A run's standard-library roots in the spelling resolved sources are
/// compared in.
pub(crate) fn comparable_std_roots(std_roots: &[PathBuf]) -> Vec<PathBuf> {
    std_roots
        .iter()
        .map(|root| compile_time::normalize(&root.canonicalize().unwrap_or_else(|_| root.clone())))
        .collect()
}

/// The module path a `[package] name` spells. A package name is a dotted
/// module path (older manifests separate segments with `::`); the manifest is
/// the one place it is read as text.
fn package_module_path(name: &str) -> ModulePath {
    ModulePath::new(
        name.split("::")
            .flat_map(|part| part.split('.'))
            .filter(|segment| !segment.is_empty()),
    )
}

/// The anchor of `file` (spec 3.5.1): the std root for a file under
/// `<std root>/std/`, otherwise the nearest manifest if it declares `[package]`.
/// A virtual workspace is a search boundary, not a package anchor. Rooted
/// in-memory source identities can name a supplied package without a host
/// filesystem; loose relative buffers have no package of their own.
pub(crate) fn anchor_for(
    file: &Path,
    std_roots: &[PathBuf],
    documents: &SourceSet,
) -> Result<hew_types::module_registry::ModuleAnchor, FrontendFailure> {
    use hew_types::module_registry::ModuleAnchor;
    if let Some(anchor) = hew_types::module_registry::std_anchor_for(file, std_roots) {
        return Ok(anchor);
    }
    let Some(dir) = file
        .parent()
        .filter(|_| file.is_absolute() || (!documents.filesystem && file.has_root()))
        .and_then(|dir| {
            dir.ancestors()
                .find(|dir| documents.is_file(&dir.join("hew.toml")))
        })
    else {
        return Ok(ModuleAnchor::Loose);
    };
    let Some(package) = load_manifest_from(dir, documents)?.and_then(|manifest| manifest.package)
    else {
        return Ok(ModuleAnchor::Loose);
    };
    Ok(ModuleAnchor::Package {
        dir: dir.to_path_buf(),
        name: Some(package_module_path(&package.name)),
    })
}

/// The anchor imports written in `file` resolve against. A file outside any
/// package compiled with an explicit project (`--project-dir`, an editor
/// workspace) imports as a member of that project's package.
fn importer_anchor(
    file: &Path,
    ctx: &ImportResolutionContext<'_>,
    std_roots: &[PathBuf],
) -> Result<hew_types::module_registry::ModuleAnchor, FrontendFailure> {
    use hew_types::module_registry::ModuleAnchor;
    match anchor_for(file, std_roots, ctx.documents)? {
        ModuleAnchor::Loose => Ok(match ctx.package_name {
            Some(name) => ModuleAnchor::Package {
                dir: ctx.project_dir.to_path_buf(),
                name: Some(package_module_path(name)),
            },
            None => ModuleAnchor::Loose,
        }),
        anchor => Ok(anchor),
    }
}

/// The module `file` belongs to (spec 3.5.1), with directories listed from
/// the filesystem and the open documents together.
fn membership_of(
    file: &Path,
    std_roots: &[PathBuf],
    documents: &SourceSet,
) -> Result<hew_types::module_registry::ModuleMembership, FrontendFailure> {
    let anchor = anchor_for(file, std_roots, documents)?;
    let list_dir = |dir: &Path| {
        let mut files = if documents.filesystem {
            hew_types::module_registry::filesystem_dir_listing(dir)
        } else {
            Vec::new()
        };
        for path in documents.files_in(dir) {
            if !files.contains(&path) {
                files.push(path);
            }
        }
        files
    };
    Ok(hew_types::module_registry::module_membership(
        &anchor, file, &list_dir,
    ))
}

/// Which module the source file at `path` belongs to: the one answer the
/// import resolver, `hew check`, the language server, `hew test` and
/// migration share (spec 3.5.1). `None` when the file's manifest is invalid;
/// a frontend run over it reports why.
#[must_use]
pub fn module_membership(
    path: &Path,
    options: &FrontendOptions,
) -> Option<hew_types::module_registry::ModuleMembership> {
    let path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    let std_roots = comparable_std_roots(&options.std_roots());
    membership_of(&path, &std_roots, &options.documents).ok()
}

/// The source file a module import written in `importer` names, from the
/// import resolver's own search roots; `None` when nothing, or more than one
/// file, answers. Editor navigation follows imports through this rather than
/// a search of its own.
#[must_use]
pub fn resolve_module_import(
    importer: &Path,
    path: &hew_parser::ast::Path,
    options: &FrontendOptions,
) -> Option<PathBuf> {
    if path.segments.is_empty() {
        return None;
    }
    let importer = importer
        .canonicalize()
        .unwrap_or_else(|_| importer.to_path_buf());
    let project_dir = options.project_dir.clone().unwrap_or_else(|| {
        let dir = importer.parent().unwrap_or(Path::new("."));
        dir.ancestors()
            .find(|dir| options.documents.is_file(&dir.join("hew.toml")))
            .unwrap_or(dir)
            .to_path_buf()
    });
    let (manifest_deps, package_name) =
        load_manifest_metadata_from(&project_dir, &options.documents).ok()?;
    let locked_versions = if options.ignore_lockfile {
        None
    } else {
        load_lockfile_from(&project_dir, &options.documents).ok()?
    };
    let std_roots = options.std_roots();
    let ctx = ImportResolutionContext {
        in_progress_imports: HashSet::new(),
        resolved_imports: HashMap::new(),
        manifest_deps: manifest_deps.as_deref(),
        extra_pkg_path: options.pkg_path.as_deref(),
        locked_versions: locked_versions.as_deref(),
        package_name: package_name.as_deref(),
        project_dir: &project_dir,
        std_roots: &std_roots,
        documents: &options.documents,
    };
    let std_roots = comparable_std_roots(ctx.std_roots);
    let anchor = anchor_for(&importer, &std_roots, &options.documents).ok()?;
    let mut resolved = module_candidates(path, &importer, &anchor, &ctx, &std_roots)
        .candidates
        .iter()
        .filter_map(|entry| {
            resolve_module_candidate(&ctx, &std_roots, entry)
                .ok()
                .flatten()
        })
        .collect::<Vec<_>>();
    resolved.sort();
    resolved.dedup();
    let [found] = resolved.as_slice() else {
        return None;
    };
    Some(found.clone())
}

/// The shape a dotted import path was turned into a candidate file with: the
/// directory form `a/b/b.hew` or the flat form `a/b.hew`. Which one a module
/// resolved through is the only thing that separates the entry-file spelling of
/// a directory module from a nested module that repeats its own name, so the
/// resolver carries the form rather than reading it back off the path.
#[derive(Clone, Copy, PartialEq, Eq)]
enum CandidateForm {
    Directory,
    Flat,
}

/// The spelled segments of a module import path. Mapping a module path onto
/// source files is the one place a segment is needed as text.
pub(crate) fn import_segments(path: &hew_parser::ast::Path) -> Vec<&'static str> {
    path.segments
        .iter()
        .map(|(segment, _)| segment.name.as_str())
        .collect()
}

pub(crate) fn canonical_direct_stdlib_module_for_source(
    source_file: &Path,
    std_roots: &[PathBuf],
) -> Option<hew_parser::module::ModulePath> {
    hew_types::module_registry::canonical_stdlib_module_for_source(source_file, std_roots)
}

/// Resolve the prelude source `std/<name>.hew` from the run's
/// standard-library roots, the ones a `std` import uses. The module carries
/// the path its diagnostics belong to.
pub(crate) fn resolve_prelude_std_source(
    ctx: &ImportResolutionContext<'_>,
    name: &str,
) -> Result<(PathBuf, Arc<str>), FrontendFailure> {
    ctx.std_roots
        .iter()
        .find_map(|root| {
            let candidate = root.join("std").join(format!("{name}.hew"));
            let path = resolve_candidate(ctx.documents, &candidate)?;
            let source = read_source(ctx.documents, &path).ok()?;
            Some((path, source))
        })
        .ok_or_else(|| {
            // With no std root at all, name where the toolchain looked for
            // its own.
            let probed = if ctx.std_roots.is_empty() {
                hew_types::module_registry::compiler_stdlib_root_candidates()
            } else {
                ctx.std_roots.to_vec()
            };
            let tried = probed
                .iter()
                .map(|root| display_path(&root.join("std")))
                .collect::<Vec<_>>()
                .join(", ");
            let cause = misnamed_hew_std_cause()
                .unwrap_or_else(|| "set HEW_STD to a std/ directory".to_string());
            FrontendFailure::message_only(format!(
                "Error: std not found: `std/{name}.hew` is not in the toolchain's standard \
                 library (tried: {tried}); {cause}"
            ))
        })
}

/// Why `std.*` resolves nowhere when `HEW_STD` names a directory not called
/// `std`: its parent is the std root, so the std it names is never read.
fn misnamed_hew_std_cause() -> Option<String> {
    hew_types::module_registry::misnamed_hew_std().map(|std_dir| {
        format!(
            "HEW_STD names `{}`, but it must name a directory called `std`",
            display_path(&std_dir)
        )
    })
}

/// Where an import candidate comes from, in search order (spec 3.5.3).
#[derive(Clone, Copy, PartialEq, Eq)]
enum CandidateRoot {
    /// The standard-library root, for `std.*` imports only.
    Std,
    /// The importing file's own package, reached through its whole name.
    Package,
    /// The importing file's directory.
    Relative,
    /// An installed or locked dependency, or a `--pkg-path` root.
    Dependency,
}

/// Every file one module import may name.
struct ModuleCandidates {
    candidates: Vec<(PathBuf, CandidateForm, CandidateRoot)>,
    locked_project_candidates: Vec<(PathBuf, LockedPackageCheck)>,
    installed_package_dir: Option<PathBuf>,
}

/// The part of an import path below the package `package`, when the path
/// starts with the package's whole dotted name.
fn package_relative<'p>(
    path: &'p hew_parser::ast::Path,
    package: &ModulePath,
) -> Option<&'p [Spanned<hew_parser::ast::Ident>]> {
    let prefix_len = package.segments.len();
    (path.segments.len() >= prefix_len
        && path
            .segments
            .iter()
            .zip(&package.segments)
            .all(|((segment, _), package_segment)| segment.name == *package_segment))
    .then(|| &path.segments[prefix_len..])
}

/// Whether the manifest declares the dependency an import path names: the
/// dependency itself or a module inside it.
fn declares_dependency(manifest_deps: Option<&[String]>, segments: &[&str]) -> bool {
    manifest_deps.is_some_and(|deps| {
        deps.iter().any(|dependency| {
            let declared = dependency
                .split("::")
                .flat_map(|part| part.split('.'))
                .collect::<Vec<_>>();
            segments.starts_with(&declared)
        })
    })
}

/// The directory form `a/b/c/c.hew` and flat form `a/b/c.hew` of a module
/// path below `base`.
fn module_forms(base: &Path, segments: &[&str]) -> [(PathBuf, CandidateForm); 2] {
    let relative = segments.iter().collect::<PathBuf>();
    let last = segments.last().copied().unwrap_or_default();
    [
        (
            base.join(&relative).join(last).with_extension("hew"),
            CandidateForm::Directory,
        ),
        (
            base.join(relative.with_extension("hew")),
            CandidateForm::Flat,
        ),
    ]
}

/// The files `import a.b.c` written in `importer` may name, in search order
/// (spec 3.5.3):
///
/// 1. `std.*` resolves from the standard-library root only.
/// 2. A path starting with the importing package's whole name resolves inside
///    that package: the bare name is its root module `<leaf>.hew`.
/// 3. Paths relative to the importing file's directory; the directory form
///    only when the importer belongs to a package.
/// 4. Dependencies, then the `--pkg-path`, `hew.` and `ecosystem.` roots.
///
/// The working directory and the std root are not searched for user modules.
fn module_candidates(
    path: &hew_parser::ast::Path,
    importer: &Path,
    anchor: &hew_types::module_registry::ModuleAnchor,
    ctx: &ImportResolutionContext<'_>,
    std_roots: &[PathBuf],
) -> ModuleCandidates {
    use hew_types::module_registry::ModuleAnchor;
    let segments = import_segments(path);
    let module_str = segments.join("::");
    let mut out = ModuleCandidates {
        candidates: Vec::new(),
        locked_project_candidates: Vec::new(),
        installed_package_dir: None,
    };
    let push = |candidates: &mut Vec<(PathBuf, CandidateForm, CandidateRoot)>,
                forms: [(PathBuf, CandidateForm); 2],
                root: CandidateRoot,
                directory: bool| {
        for (candidate, form) in forms {
            if directory || form == CandidateForm::Flat {
                candidates.push((candidate, form, root));
            }
        }
    };

    if module_str.starts_with("std::") {
        for root in std_roots {
            push(
                &mut out.candidates,
                module_forms(root, &segments),
                CandidateRoot::Std,
                true,
            );
        }
        return out;
    }

    if let ModuleAnchor::Package {
        dir,
        name: Some(package),
    } = anchor
    {
        if let Some(rest) = package_relative(path, package) {
            if rest.is_empty() {
                let leaf = package.segments.last().map_or("", |leaf| leaf.as_str());
                out.candidates.push((
                    dir.join(leaf).with_extension("hew"),
                    CandidateForm::Flat,
                    CandidateRoot::Package,
                ));
            } else {
                let rest = rest
                    .iter()
                    .map(|(segment, _)| segment.name.as_str())
                    .collect::<Vec<_>>();
                push(
                    &mut out.candidates,
                    module_forms(dir, &rest),
                    CandidateRoot::Package,
                    true,
                );
            }
        }
    }

    let importer_dir = importer.parent().unwrap_or(Path::new("."));
    push(
        &mut out.candidates,
        module_forms(importer_dir, &segments),
        CandidateRoot::Relative,
        true,
    );
    if let ModuleAnchor::Package { dir, .. } = anchor {
        if importer_dir != dir.as_path() {
            push(
                &mut out.candidates,
                module_forms(dir, &segments),
                CandidateRoot::Package,
                true,
            );
        }
    }

    dependency_candidates(&mut out, &segments, ctx);
    out
}

/// Step 4 of [`module_candidates`]: the project's installed and locked
/// packages, then the `--pkg-path`, `hew.` and `ecosystem.` roots.
fn dependency_candidates(
    out: &mut ModuleCandidates,
    segments: &[&str],
    ctx: &ImportResolutionContext<'_>,
) {
    let module_str = segments.join("::");
    let source_module = segments.join(".");
    let [(dir_rel, _), (flat_rel, _)] = module_forms(Path::new(""), segments);
    let module_dir = segments.iter().collect::<PathBuf>();
    let packages = ctx.project_dir.join(".hew/packages");
    let locked_version = ctx
        .locked_versions
        .and_then(|locked| {
            locked
                .iter()
                .find(|(name, _)| name == &module_str || name == &source_module)
        })
        .map(|(_, version)| version.as_str());
    if let Some(version) = locked_version {
        let last = segments.last().copied().unwrap_or_default();
        let versioned_rel = module_dir.join(version).join(last).with_extension("hew");
        // The version directory sits between the module and its entry file,
        // so this is a package root, never a flat file.
        out.candidates.push((
            packages.join(&versioned_rel),
            CandidateForm::Directory,
            CandidateRoot::Dependency,
        ));
        if let Some(pkg) = ctx.extra_pkg_path {
            out.candidates.push((
                pkg.join(&versioned_rel),
                CandidateForm::Directory,
                CandidateRoot::Dependency,
            ));
        }
    }
    out.candidates.push((
        packages.join(&flat_rel),
        CandidateForm::Flat,
        CandidateRoot::Dependency,
    ));
    let project_package_dir = packages.join(&module_dir);
    if declares_dependency(ctx.manifest_deps, segments) {
        out.installed_package_dir = Some(project_package_dir.clone());
    }
    let project_package_entry = packages.join(&dir_rel);
    if let Some(version) = locked_version {
        out.locked_project_candidates.push((
            project_package_entry.clone(),
            LockedPackageCheck {
                package_dir: project_package_dir,
                name: source_module.clone(),
                version: version.to_string(),
            },
        ));
    }
    out.candidates.push((
        project_package_entry,
        CandidateForm::Directory,
        CandidateRoot::Dependency,
    ));

    if let Some(pkg) = ctx.extra_pkg_path {
        out.candidates.extend(
            module_forms(pkg, segments)
                .into_iter()
                .map(|(candidate, form)| (candidate, form, CandidateRoot::Dependency)),
        );
        // The `--pkg-path` root holds `hew.` and `ecosystem.` packages below
        // their namespace, and others below their first segment.
        if segments.len() > 1 {
            out.candidates.extend(
                module_forms(pkg, &segments[1..])
                    .into_iter()
                    .map(|(candidate, form)| (candidate, form, CandidateRoot::Dependency)),
            );
        }
    }
}

/// Whether a module path repeats its last segment, the spelling that names a
/// directory module's entry file (`pkg.dir.dir` for `pkg/dir/dir.hew`).
fn repeats_last_segment(segments: &[&str]) -> bool {
    matches!(segments, [.., parent, last] if parent == last)
}

/// The file a module-import candidate names, if it exists and the spelling
/// may reach it. Beside the importer, the directory form `a/b/b.hew` names a
/// module only when that file roots one: a directory module's entry, or a
/// package directory's own `b.hew`. A loose `b/b.hew` is a file in a namespace
/// directory, never a directory module (spec 3.5.1).
fn resolve_module_candidate(
    ctx: &ImportResolutionContext<'_>,
    std_roots: &[PathBuf],
    (candidate, form, root): &(PathBuf, CandidateForm, CandidateRoot),
) -> Result<Option<PathBuf>, FrontendFailure> {
    use hew_types::module_registry::{MembershipRole, ModuleAnchor};
    let Some(canonical) = resolve_candidate(ctx.documents, candidate) else {
        return Ok(None);
    };
    if *form == CandidateForm::Flat || *root != CandidateRoot::Relative {
        return Ok(Some(canonical));
    }
    let membership = membership_of(&canonical, std_roots, ctx.documents)?;
    let roots_a_module = membership.role == MembershipRole::Entry
        || matches!(&membership.anchor, ModuleAnchor::Package { dir, .. }
            if canonical.parent() == Some(dir.as_path()));
    Ok(roots_a_module.then_some(canonical))
}

/// A positioned resolver diagnostic on the import at `span`, or a plain one
/// when the importing file can no longer be read.
fn import_failure(
    ctx: &ImportResolutionContext<'_>,
    source_file: &Path,
    span: &Range<usize>,
    code: &'static str,
    message: String,
) -> FrontendFailure {
    match read_source(ctx.documents, source_file) {
        Ok(module_source) => FrontendFailure::coded_message_at(
            code,
            message,
            span.clone(),
            &module_source,
            &source_file.display().to_string(),
        ),
        Err(_) => FrontendFailure::coded_message(code, message),
    }
}

/// Why a module import found nothing, beyond the paths it tried: a directory
/// module beside a file that belongs to no package, or a module that sits in
/// a parent directory of the importing file.
fn module_not_found_hints(
    importer: &Path,
    anchor: &hew_types::module_registry::ModuleAnchor,
    segments: &[&str],
    ctx: &ImportResolutionContext<'_>,
) -> String {
    use hew_types::module_registry::ModuleAnchor;
    let importer_dir = importer.parent().unwrap_or(Path::new("."));
    let [(directory_form, _), (flat_form, _)] = module_forms(Path::new(""), segments);
    let source_module = segments.join(".");
    if matches!(anchor, ModuleAnchor::Loose)
        && resolve_candidate(ctx.documents, &importer_dir.join(&directory_form)).is_some()
    {
        return format!(
            "\n  note: {} forms a directory module, and directory modules belong to a package\n  \
             help: run `hew init` in this directory, or import `{source_module}.{}` for the entry file alone",
            display_path(&directory_form),
            segments.last().copied().unwrap_or_default(),
        );
    }
    let Some(found) = importer_dir.ancestors().skip(1).find_map(|dir| {
        [&directory_form, &flat_form]
            .into_iter()
            .map(|form| dir.join(form))
            .find(|candidate| resolve_candidate(ctx.documents, candidate).is_some())
    }) else {
        return String::new();
    };
    let help = match anchor {
        ModuleAnchor::Package {
            dir,
            name: Some(package),
        } if found.starts_with(dir) => {
            let below = found
                .strip_prefix(dir)
                .unwrap_or(&found)
                .with_extension("");
            let mut module = package.segments.iter().map(|segment| segment.as_str()).collect::<Vec<_>>();
            let below = below
                .iter()
                .filter_map(|segment| segment.to_str())
                .collect::<Vec<_>>();
            // `dir/dir.hew` is reached as `dir`.
            let below = if repeats_last_segment(&below) {
                &below[..below.len() - 1]
            } else {
                &below[..]
            };
            if below != [module.last().copied().unwrap_or_default()] {
                module.extend(below);
            }
            format!("import `{}` names it through the package", module.join("."))
        }
        _ => "imports are relative to the importing file; a package (`hew init`) names its modules from its root".to_string(),
    };
    format!(
        "\n  note: `{}` is in a parent directory of the importing file\n  help: {help}",
        display_path(&found)
    )
}

#[expect(
    clippy::too_many_lines,
    reason = "sequential import resolution steps for file and module imports"
)]
pub(crate) fn resolve_file_imports_internal(
    source_file: &Path,
    items: &mut [Spanned<Item>],
    ctx: &mut ImportResolutionContext<'_>,
    diagnostics: &mut Vec<FrontendDiagnostic>,
) -> Result<(), FrontendFailure> {
    let source_dir = source_file
        .parent()
        .expect("source file should have a parent directory");

    let import_indices = items
        .iter()
        .enumerate()
        .filter_map(|(index, (item, _))| {
            if let Item::Import(decl) = item {
                if decl.file_path.is_some() || !decl.path.segments.is_empty() {
                    return Some(index);
                }
            }
            None
        })
        .collect::<Vec<_>>();
    if import_indices.is_empty() {
        return Ok(());
    }

    let std_roots = comparable_std_roots(ctx.std_roots);
    let anchor = importer_anchor(source_file, ctx, &std_roots)?;

    for idx in &import_indices {
        let canonical = match &items[*idx].0 {
            Item::Import(decl) if decl.file_path.is_some() => {
                let file_path = decl.file_path.as_ref().expect("checked above");
                let resolved = source_dir.join(file_path);
                if let Some(canonical) = resolve_candidate(ctx.documents, &resolved) {
                    canonical
                } else {
                    return Err(FrontendFailure::message_only(format!(
                        "Error: imported file not found: {file_path} (resolved to {})",
                        resolved.display()
                    )));
                }
            }
            Item::Import(decl) if !decl.path.segments.is_empty() => {
                let segments = import_segments(&decl.path);
                let source_module = segments.join(".");
                let is_std_import = segments.join("::").starts_with("std::");
                let ModuleCandidates {
                    candidates,
                    locked_project_candidates,
                    installed_package_dir,
                } = module_candidates(&decl.path, source_file, &anchor, ctx, &std_roots);

                // Collect ALL candidates that resolve, then deduplicate by canonical path.
                // If two or more distinct canonical paths resolve, the import is ambiguous —
                // fail-closed rather than silently picking the first match.
                let mut resolved: Vec<(PathBuf, CandidateForm, CandidateRoot)> = Vec::new();
                for entry in &candidates {
                    let (candidate, form, origin) = entry;
                    if let Some(canonical) = resolve_module_candidate(ctx, &std_roots, entry)? {
                        if let Some((_, check)) = locked_project_candidates
                            .iter()
                            .find(|(locked_candidate, _)| locked_candidate == candidate)
                        {
                            verify_locked_project_package(check, ctx.documents)?;
                        }
                        // One file reached by both shapes is a directory module
                        // named by its directory: the directory candidate wins.
                        match resolved.iter_mut().find(|(path, _, _)| *path == canonical) {
                            Some((_, existing, _)) => {
                                if *form == CandidateForm::Directory {
                                    *existing = CandidateForm::Directory;
                                }
                            }
                            None => resolved.push((canonical, *form, *origin)),
                        }
                    }
                }
                resolved.sort_by(|(left, _, _), (right, _, _)| left.cmp(right));

                if resolved.len() > 1 {
                    let paths = resolved
                        .iter()
                        .map(|(path, _, _)| path.display().to_string())
                        .collect::<Vec<_>>()
                        .join("` and `");
                    return Err(FrontendFailure::coded_message(
                        "E_IMPORT_AMBIGUOUS",
                        format!(
                            "Error: import `{source_module}` is ambiguous: both `{paths}` exist.\n  Rename or remove one to resolve the ambiguity."
                        ),
                    ));
                }

                if let Some((canonical, form, origin)) = resolved.into_iter().next() {
                    use hew_types::module_registry::{MembershipRole, ModuleAnchor};
                    let membership = membership_of(&canonical, &std_roots, ctx.documents)?;
                    if origin == CandidateRoot::Dependency {
                        if let (
                            ModuleAnchor::Package { dir: importer, .. },
                            ModuleAnchor::Package {
                                dir,
                                name: Some(name),
                            },
                        ) = (&anchor, &membership.anchor)
                        {
                            let (dependencies, _) =
                                load_manifest_metadata_from(importer, ctx.documents)?;
                            if dependencies.is_some_and(|deps| deps.contains(&name.dotted())) {
                                ctx.documents
                                    .dependency_roots
                                    .lock()
                                    .expect("source dependency table poisoned")
                                    .insert(dir.clone());
                            }
                        }
                    }
                    let directory_module = || segments[..segments.len() - 1].join(".");
                    match membership.role {
                        MembershipRole::Entry
                            if form == CandidateForm::Flat && repeats_last_segment(&segments) =>
                        {
                            // A directory module is spelled by its directory,
                            // and its entry file adds no second module (spec
                            // 3.5.1). Accepting both spellings would let one
                            // compilation reach one source under two names.
                            return Err(import_failure(
                                ctx,
                                source_file,
                                &items[*idx].1,
                                "E_ENTRY_FILE_IMPORT",
                                format!(
                                    "cannot import `{source_module}`: `{}` is a directory module and its entry file is not a module of its own; import `{}` instead",
                                    directory_module(),
                                    directory_module(),
                                ),
                            ));
                        }
                        MembershipRole::Peer if segments.len() >= 2 => {
                            if matches!(membership.anchor, ModuleAnchor::Std { .. }) {
                                // The shipped std's directory peers stay
                                // importable by file (`std.net.http.codec.request`,
                                // D461); they load through the directory's
                                // entry so the module is complete however it
                                // was reached.
                                membership.entry
                            } else {
                                // A user peer has no identity of its own: it
                                // shares the directory module's namespace, so
                                // loading it alone would miss the siblings it
                                // refers to.
                                return Err(import_failure(
                                    ctx,
                                    source_file,
                                    &items[*idx].1,
                                    "E_PEER_IMPORT",
                                    format!(
                                        "cannot import `{source_module}` directly: peer files are reached through the directory module; import `{}` instead",
                                        directory_module(),
                                    ),
                                ));
                            }
                        }
                        _ => canonical,
                    }
                } else {
                    let last = *segments.last().expect("path is non-empty");
                    if let Some(package_dir) = installed_package_dir.filter(|dir| dir.is_dir()) {
                        let expected = package_dir.join(last).with_extension("hew");
                        return Err(FrontendFailure::coded_message(
                            "E_PACKAGE_ROOT_MISSING",
                            format!(
                                "Error: installed dependency `{source_module}` has no canonical root module `{last}.hew` at {}\n  hint: a library package exposes `<package-name>.hew`; create `{}` and set `[package] main = \"{last}.hew\"` (new packages get this layout from `hew init --lib {source_module}`)",
                                package_dir.display(),
                                expected.display(),
                            ),
                        ));
                    }
                    let tried = candidates
                        .iter()
                        .map(|(candidate, _, _)| display_path(candidate))
                        .collect::<Vec<_>>()
                        .join(", ");
                    let hint = if declares_dependency(ctx.manifest_deps, &segments) {
                        "\n  hint: this dependency is declared in hew.toml — run `hew install`"
                            .to_string()
                    } else if is_std_import {
                        misnamed_hew_std_cause()
                            .map(|cause| format!("\n  hint: {cause}"))
                            .or_else(|| {
                                nearest_std_module(last, &std_roots)
                                    .map(|nearest| format!("\n  did you mean `{nearest}`?"))
                            })
                            .unwrap_or_default()
                    } else {
                        let hints = module_not_found_hints(source_file, &anchor, &segments, ctx);
                        if hints.is_empty() && ctx.manifest_deps.is_some() {
                            "\n  hint: add this module to [dependencies] in hew.toml".to_string()
                        } else {
                            hints
                        }
                    };
                    let message = if is_std_import && std_roots.is_empty() {
                        // No std root at all: the toolchain's std is missing,
                        // not this one module.
                        let probed = hew_types::module_registry::compiler_stdlib_root_candidates()
                            .iter()
                            .map(|root| display_path(&root.join("std")))
                            .collect::<Vec<_>>()
                            .join(", ");
                        format!(
                            "std not found: module `{source_module}` needs the toolchain's \
                             standard library (tried: {probed}); set HEW_STD to a std/ directory"
                        )
                    } else {
                        format!("module `{source_module}` not found (tried: {tried}){hint}")
                    };
                    return Err(import_failure(
                        ctx,
                        source_file,
                        &items[*idx].1,
                        "E_MODULE_NOT_FOUND",
                        message,
                    ));
                }
            }
            _ => continue,
        };

        let Some(resolved_import) =
            resolve_completed_import_internal(&canonical, ctx, &items[*idx].0, diagnostics)?
        else {
            continue;
        };

        if let Item::Import(decl) = &mut items[*idx].0 {
            decl.resolved_items = Some(resolved_import.items.clone());
            decl.resolved_item_source_paths
                .clone_from(&resolved_import.item_source_paths);
            decl.resolved_source_paths
                .clone_from(&resolved_import.source_paths);
        }
    }

    Ok(())
}

fn resolve_completed_import_internal(
    canonical: &Path,
    ctx: &mut ImportResolutionContext<'_>,
    import_item: &Item,
    diagnostics: &mut Vec<FrontendDiagnostic>,
) -> Result<Option<ResolvedImport>, FrontendFailure> {
    if let Some(cached) = ctx.resolved_imports.get(canonical) {
        return Ok(Some(cached.clone()));
    }
    if ctx.in_progress_imports.contains(canonical) {
        return Ok(None);
    }

    ctx.in_progress_imports.insert(canonical.to_path_buf());
    let resolved = build_resolved_import_internal(canonical, ctx, import_item, diagnostics);
    ctx.in_progress_imports.remove(canonical);

    match resolved {
        Ok(resolved_import) => {
            ctx.resolved_imports
                .insert(canonical.to_path_buf(), resolved_import.clone());
            Ok(Some(resolved_import))
        }
        Err(error) => Err(error),
    }
}

fn build_resolved_import_internal(
    canonical: &Path,
    ctx: &mut ImportResolutionContext<'_>,
    import_item: &Item,
    diagnostics: &mut Vec<FrontendDiagnostic>,
) -> Result<ResolvedImport, FrontendFailure> {
    let std_roots = comparable_std_roots(ctx.std_roots);
    let membership = membership_of(canonical, &std_roots, ctx.documents)?;
    let peer_files = if membership.role == hew_types::module_registry::MembershipRole::Entry {
        membership.peers.clone()
    } else {
        Vec::new()
    };

    let mut import_items = parse_and_resolve_file_internal(canonical, ctx, diagnostics)?;
    let mut import_item_source_paths = vec![canonical.to_path_buf(); import_items.len()];
    let mut source_paths = vec![canonical.to_path_buf()];

    for peer in &peer_files {
        let peer_canonical = peer.canonicalize().unwrap_or_else(|_| peer.clone());
        let Some(peer_resolved) =
            resolve_completed_import_internal(&peer_canonical, ctx, import_item, diagnostics)?
        else {
            continue;
        };
        import_item_source_paths.extend(std::iter::repeat_n(
            peer_canonical.clone(),
            peer_resolved.items.len(),
        ));
        import_items.extend(peer_resolved.items.iter().cloned());
        source_paths.push(peer_canonical);
    }

    if !peer_files.is_empty() {
        check_duplicate_pub_names(&import_items, &import_item_source_paths, &membership)
            .map_err(FrontendFailure::message_only)?;
    }

    Ok(ResolvedImport {
        items: import_items.into(),
        item_source_paths: import_item_source_paths,
        source_paths,
    })
}

fn parse_and_resolve_file_internal(
    canonical: &Path,
    ctx: &mut ImportResolutionContext<'_>,
    diagnostics: &mut Vec<FrontendDiagnostic>,
) -> Result<Vec<Spanned<Item>>, FrontendFailure> {
    let source = read_source(ctx.documents, canonical).map_err(|e| {
        FrontendFailure::message_only(format!(
            "Error reading imported file '{}': {e}",
            canonical.display()
        ))
    })?;

    let result = ctx.documents.parse_document(canonical, &source);
    let display_path = canonical.display().to_string();
    let parse_diagnostics = result
        .errors
        .iter()
        .cloned()
        .map(|diagnostic| FrontendDiagnostic::parse(Arc::clone(&source), &display_path, diagnostic))
        .collect::<Vec<_>>();

    if result
        .errors
        .iter()
        .any(|error| error.severity == hew_parser::Severity::Error)
    {
        return Err(FrontendFailure::new(
            format!("parsing failed in imported file '{}'", canonical.display()),
            parse_diagnostics,
        ));
    }

    diagnostics.extend(parse_diagnostics);
    let mut import_items = result.program.items;
    resolve_file_imports_internal(canonical, &mut import_items, ctx, diagnostics)?;
    Ok(import_items)
}

/// Reject two `pub` declarations of one name across the files of a directory
/// module, naming both files and why they share a namespace.
fn check_duplicate_pub_names(
    items: &[Spanned<Item>],
    item_sources: &[PathBuf],
    membership: &hew_types::module_registry::ModuleMembership,
) -> Result<(), String> {
    use hew_parser::ast::Visibility;

    // Only `Visibility::Pub` items are checked here — intentionally.
    //
    // `Visibility::Package` items are scoped to the package boundary: two
    // modules within the same package can each define `package fn foo()` in
    // their own namespace without creating a global API conflict.  The
    // duplicate-name guard exists to catch clashes in the *globally-exported*
    // interface (i.e. items a downstream package could import by name), which
    // only `pub` items contribute to.
    let mut seen: Vec<(hew_parser::ast::Symbol, &Path)> = Vec::new();
    for ((item, _), source) in items.iter().zip(item_sources) {
        let name = match item {
            Item::Function(f) if f.visibility == Visibility::Pub => f.name.name,
            Item::TypeAlias(t) if t.visibility == Visibility::Pub => t.name.name,
            Item::TypeDecl(t) if t.visibility == Visibility::Pub => t.name.name,
            Item::Actor(a) if a.visibility == Visibility::Pub => a.name.name,
            Item::Trait(t) if t.visibility == Visibility::Pub => t.name.name,
            Item::Const(c) if c.visibility == Visibility::Pub => c.name.name,
            _ => continue,
        };
        if let Some((_, first)) = seen.iter().find(|(seen_name, _)| *seen_name == name) {
            // A file below the anchor is shown as its package-relative path,
            // spelled with `/` on every platform like the module path it is.
            let shown = |path: &Path| match membership
                .anchor
                .dir()
                .and_then(|dir| path.strip_prefix(dir).ok())
            {
                Some(relative) => relative
                    .iter()
                    .map(|component| component.to_string_lossy())
                    .collect::<Vec<_>>()
                    .join("/"),
                None => display_path(path),
            };
            let entry = shown(&membership.entry);
            let directory = membership.entry.parent().map_or_else(String::new, &shown);
            return Err(format!(
                "Error: duplicate pub name `{name}` in module `{}`: declared in `{}` and in `{}`\n  \
                 note: `{directory}/` is a directory module because `{entry}` exists, so its files share one namespace",
                membership.module.dotted(),
                shown(first),
                shown(source),
            ));
        }
        seen.push((name, source));
    }
    Ok(())
}

#[cfg(test)]
mod source_anchor_tests {
    use std::path::Path;

    use super::{anchor_for, package_module_path};
    use crate::source_set::SourceSet;
    use hew_types::module_registry::ModuleAnchor;

    #[test]
    fn rooted_in_memory_sources_use_the_supplied_package_manifest() {
        // wasm32-unknown-unknown has rooted paths but no absolute host paths.
        // Source identity belongs to the provider, not host filesystem support.
        let root = Path::new("/hew-browser-anchor");
        let mut documents = SourceSet::in_memory();
        documents.insert(root.join("hew.toml"), "[package]\nname = \"app\"\n");
        let anchor = anchor_for(&root.join("lib.hew"), &[], &documents).unwrap();
        assert!(
            matches!(anchor, ModuleAnchor::Package { dir, name: Some(name) }
            if dir == root && name == package_module_path("app"))
        );
    }

    #[test]
    fn loose_buffers_keep_relative_import_semantics_with_either_provider() {
        for mut documents in [SourceSet::in_memory(), SourceSet::new()] {
            documents.insert("user/hew.toml", "[package]\nname = \"user\"\n");
            assert!(matches!(
                anchor_for(Path::new("user/nav.hew"), &[], &documents).unwrap(),
                ModuleAnchor::Loose
            ));
        }
    }
}

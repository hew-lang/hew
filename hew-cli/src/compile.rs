//! Frontend helpers for check/eval paths that still need parser/typechecker
//! diagnostics while execution subcommands are cut over to Rust codegen.

#[cfg(test)]
use std::fs;
#[cfg(test)]
use std::path::Path;
use std::path::PathBuf;

#[cfg(test)]
use std::collections::{HashMap, HashSet};

use hew_compile::{FrontendDiagnostic, FrontendDiagnosticKind, FrontendOptions};

#[cfg(test)]
use hew_compile::{
    build_module_graph as frontend_build_module_graph,
    inject_implicit_imports as frontend_inject_implicit_imports,
    line_map_from_source as frontend_line_map_from_source, parse_source as frontend_parse_source,
    typecheck_program as frontend_typecheck_program,
    validate_imports_against_manifest as frontend_validate_imports_against_manifest,
};
#[cfg(test)]
use hew_parser::ast::{ImportDecl, Item, Spanned};

use crate::target::TargetSpec;

#[derive(Debug, Clone, Default)]
pub struct CompileOptions {
    pub no_typecheck: bool,
    pub werror: bool,
    pub target: Option<String>,
    /// Override the package search directory (default: `.adze/packages/`).
    pub pkg_path: Option<PathBuf>,
    /// Anchor an in-memory compile to a specific project directory so that
    /// manifest-aware import resolution matches `compile_file` behaviour.
    pub project_dir: Option<PathBuf>,
}

pub(crate) fn frontend_options(target: &TargetSpec, options: &CompileOptions) -> FrontendOptions {
    FrontendOptions {
        no_typecheck: options.no_typecheck,
        warnings_as_errors: options.werror,
        enable_wasm_target: target.is_wasm(),
        pkg_path: options.pkg_path.clone(),
        project_dir: options.project_dir.clone(),
    }
}

/// Build frontend options for a check-only invocation that does not need to
/// resolve a `TargetSpec`. The check pipeline never reaches codegen, so it
/// does not care about the target — pass `false` for `enable_wasm_target` to
/// match the default check-mode behaviour. Exposed for the
/// `--show-stack-hints` path in `cmd_check` which calls `hew_compile::check_file`
/// directly to retain access to the source string and stack-hint side table.
pub(crate) fn frontend_options_for_check(options: &CompileOptions) -> FrontendOptions {
    FrontendOptions {
        no_typecheck: options.no_typecheck,
        warnings_as_errors: options.werror,
        enable_wasm_target: false,
        pkg_path: options.pkg_path.clone(),
        project_dir: options.project_dir.clone(),
    }
}

pub(crate) fn render_frontend_diagnostics(diagnostics: &[FrontendDiagnostic]) {
    for diagnostic in diagnostics {
        match &diagnostic.kind {
            FrontendDiagnosticKind::Message(message) => {
                crate::diagnostic::emit_plain_diagnostic_line(message);
            }
            FrontendDiagnosticKind::Parse(error) => {
                let suggestions: Vec<String> = error.hint.iter().cloned().collect();
                if let (Some(source), Some(filename)) =
                    (diagnostic.source.as_deref(), diagnostic.filename.as_deref())
                {
                    match error.severity {
                        hew_parser::Severity::Warning => crate::diagnostic::render_warning(
                            source,
                            filename,
                            &error.span,
                            &error.message,
                            &[],
                            &suggestions,
                        ),
                        hew_parser::Severity::Error => crate::diagnostic::render_diagnostic(
                            source,
                            filename,
                            &error.span,
                            &error.message,
                            &[],
                            &suggestions,
                        ),
                    }
                } else {
                    let level = match error.severity {
                        hew_parser::Severity::Warning => "warning",
                        hew_parser::Severity::Error => "error",
                    };
                    crate::diagnostic::emit_plain_diagnostic_line(&format!(
                        "{level}: {}",
                        error.message
                    ));
                }
            }
            FrontendDiagnosticKind::Type(error) => {
                if let (Some(source), Some(filename)) =
                    (diagnostic.source.as_deref(), diagnostic.filename.as_deref())
                {
                    match error.severity {
                        hew_types::error::Severity::Warning => {
                            crate::diagnostic::render_warning_with_raw_notes(
                                source,
                                filename,
                                &error.span,
                                &error.message,
                                &error.notes,
                                &error.suggestions,
                            );
                        }
                        hew_types::error::Severity::Error => {
                            crate::diagnostic::render_diagnostic_with_raw_notes(
                                source,
                                filename,
                                &error.span,
                                &error.message,
                                &error.notes,
                                &error.suggestions,
                            );
                        }
                    }
                } else {
                    let level = match error.severity {
                        hew_types::error::Severity::Warning => "warning",
                        hew_types::error::Severity::Error => "error",
                    };
                    crate::diagnostic::emit_plain_diagnostic_line(&format!(
                        "{level}: {}",
                        error.message
                    ));
                }
            }
        }
    }
}

/// Public wrapper for rendering frontend diagnostics; used by `cmd_check`
/// when `--explain-cow` is set (which calls `check_file` directly instead of
/// going through the normal `compile` path).
pub(crate) fn render_frontend_diagnostics_pub(diagnostics: &[FrontendDiagnostic]) {
    render_frontend_diagnostics(diagnostics);
}

/// Run `check_file` and return the full [`hew_compile::CheckOutput`] so the
/// caller can consume `actor_send_aliasing` for `--explain-cow` rendering.
///
/// Renders diagnostics via [`render_frontend_diagnostics`] on failure.
pub(crate) fn check_explain_cow(
    input: &str,
    options: &CompileOptions,
) -> Result<hew_compile::CheckOutput, String> {
    let target =
        TargetSpec::from_requested(options.target.as_deref()).map_err(|e| format!("Error: {e}"))?;
    let fopts = frontend_options(&target, options);
    match hew_compile::check_file(input, &fopts) {
        Ok(result) => Ok(result),
        Err(failure) => {
            render_frontend_diagnostics(&failure.diagnostics);
            Err(failure.message)
        }
    }
}

#[cfg(test)]
type ImportResolutionContext<'a> = hew_compile::ImportResolutionContext<'a>;

#[cfg(test)]
fn validate_imports_against_manifest(
    items: &[Spanned<Item>],
    manifest_deps: &[String],
    package_name: Option<&str>,
) -> Vec<String> {
    frontend_validate_imports_against_manifest(items, manifest_deps, package_name)
}

#[cfg(test)]
fn line_map_from_source(source: &str) -> Vec<usize> {
    frontend_line_map_from_source(source)
}

#[cfg(test)]
fn parse_source(source: &str, input: &str) -> Result<hew_parser::ast::Program, String> {
    frontend_parse_source(source, input).map_err(|failure| failure.message)
}

#[cfg(test)]
fn build_module_graph(
    source_file: &Path,
    items: &mut Vec<Spanned<Item>>,
    module_doc: Option<String>,
    ctx: &mut ImportResolutionContext<'_>,
) -> Result<hew_parser::module::ModuleGraph, Vec<String>> {
    frontend_build_module_graph(source_file, items, module_doc, ctx)
        .map_err(|failure| vec![failure.message])
}

#[cfg(test)]
fn typecheck_program(
    program: &hew_parser::ast::Program,
    source: &str,
    input: &str,
    target: &TargetSpec,
    options: &CompileOptions,
) -> Result<hew_compile::TypeCheckResult, String> {
    frontend_typecheck_program(program, source, input, &frontend_options(target, options))
        .map_err(|failure| failure.message)
}

#[cfg(test)]
fn inject_implicit_imports(items: &mut Vec<Spanned<Item>>, source: &str) {
    frontend_inject_implicit_imports(items, source);
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_types::{error::TypeErrorKind, module_registry::ModuleRegistry};

    fn make_module_import(path: &[&str]) -> Spanned<Item> {
        let decl = hew_parser::ast::ImportDecl {
            path: path.iter().map(ToString::to_string).collect(),
            spec: None,
            file_path: None,
            resolved_items: None,
            resolved_item_source_paths: Vec::new(),
            resolved_source_paths: Vec::new(),
        };
        (Item::Import(decl), 0..0)
    }

    fn make_file_import(file: &str) -> Spanned<Item> {
        let decl = hew_parser::ast::ImportDecl {
            path: vec![],
            spec: None,
            file_path: Some(file.to_string()),
            resolved_items: None,
            resolved_item_source_paths: Vec::new(),
            resolved_source_paths: Vec::new(),
        };
        (Item::Import(decl), 0..0)
    }

    struct TestFixtureDir {
        path: PathBuf,
    }

    impl TestFixtureDir {
        fn new(name: &str, files: &[(&str, &str)]) -> Self {
            let path = Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("target")
                .join("compile-tests")
                .join(name);
            let _ = fs::remove_dir_all(&path);
            fs::create_dir_all(&path).expect("fixture directory should be created");
            for (relative_path, contents) in files {
                let file_path = path.join(relative_path);
                if let Some(parent) = file_path.parent() {
                    fs::create_dir_all(parent).expect("fixture parent directories should exist");
                }
                fs::write(&file_path, contents).expect("fixture file should be written");
            }
            Self { path }
        }

        fn join(&self, relative_path: &str) -> PathBuf {
            self.path.join(relative_path)
        }
    }

    impl Drop for TestFixtureDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.path);
        }
    }

    fn find_file_import<'a>(items: &'a [Spanned<Item>], file: &str) -> &'a ImportDecl {
        items
            .iter()
            .find_map(|(item, _)| match item {
                Item::Import(decl) if decl.file_path.as_deref() == Some(file) => Some(decl),
                _ => None,
            })
            .unwrap_or_else(|| panic!("expected file import `{file}`"))
    }

    fn implicit_import_paths_for(source: &str) -> Vec<Vec<String>> {
        let mut program = parse_source(source, "test.hew").expect("source should parse");
        inject_implicit_imports(&mut program.items, source);
        program
            .items
            .iter()
            .filter_map(|(item, _)| {
                if let Item::Import(decl) = item {
                    Some(decl.path.clone())
                } else {
                    None
                }
            })
            .collect()
    }

    #[test]
    fn validate_no_manifest_allows_all() {
        // When manifest exists but has no deps, undeclared imports are flagged.
        let items = vec![make_module_import(&["mylib", "utils"])];
        let errs = validate_imports_against_manifest(&items, &[], None);
        assert_eq!(errs.len(), 1, "undeclared import should produce an error");
    }

    #[test]
    fn validate_declared_dep_is_ok() {
        let items = vec![make_module_import(&["mylib", "utils"])];
        let deps = vec!["mylib::utils".to_string()];
        let errs = validate_imports_against_manifest(&items, &deps, None);
        assert!(errs.is_empty());
    }

    #[test]
    fn validate_undeclared_dep_errors() {
        let items = vec![make_module_import(&["mylib", "utils"])];
        let deps: Vec<String> = vec!["mylib::other".to_string()];
        let errs = validate_imports_against_manifest(&items, &deps, None);
        assert_eq!(errs.len(), 1);
        assert!(errs[0].contains("mylib::utils"));
        assert!(errs[0].contains("adze add"));
    }

    #[test]
    fn validate_stdlib_import_is_always_ok() {
        // std::fs is a known stdlib module
        let items = vec![make_module_import(&["std", "fs"])];
        let deps: Vec<String> = vec![];
        let errs = validate_imports_against_manifest(&items, &deps, None);
        assert!(errs.is_empty(), "stdlib imports are always allowed");
    }

    #[test]
    fn validate_file_import_is_not_validated() {
        let items = vec![make_file_import("./lib.hew")];
        let deps: Vec<String> = vec![];
        let errs = validate_imports_against_manifest(&items, &deps, None);
        assert!(
            errs.is_empty(),
            "file-path imports are not subject to manifest validation"
        );
    }

    #[test]
    fn validate_multiple_imports_reports_all_errors() {
        let items = vec![
            make_module_import(&["mylib", "a"]),
            make_module_import(&["mylib", "b"]),
            make_module_import(&["mylib", "c"]),
        ];
        let deps = vec!["mylib::a".to_string()];
        let errs = validate_imports_against_manifest(&items, &deps, None);
        assert_eq!(errs.len(), 2);
    }

    #[test]
    fn validate_local_import_is_exempt() {
        // Imports matching the package name are local and skip manifest validation.
        let items = vec![make_module_import(&["myapp", "models"])];
        let errs = validate_imports_against_manifest(&items, &[], Some("myapp"));
        assert!(
            errs.is_empty(),
            "local imports should be exempt from manifest validation"
        );
    }

    #[test]
    fn inject_implicit_imports_ignores_regex_text_in_comments_and_docstrings() {
        let source = r#"//! Module docs mentioning re"[0-9]+"
/// Function docs mentioning re"[a-z]+"
fn main() {
    // Comment mentioning re"foo"
}
"#;

        let imports = implicit_import_paths_for(source);
        assert!(
            !imports
                .iter()
                .any(|path| path == &["std".to_string(), "text".to_string(), "regex".to_string()]),
            "comments and docstrings should not trigger regex auto-imports"
        );
    }

    #[test]
    fn inject_implicit_imports_adds_regex_import_for_regex_literals() {
        let source = r#"fn main() {
    let digits = re"[0-9]+";
}
"#;

        let imports = implicit_import_paths_for(source);
        assert!(
            imports
                .iter()
                .any(|path| path == &["std".to_string(), "text".to_string(), "regex".to_string()]),
            "regex literals should still trigger regex auto-imports"
        );
    }

    #[test]
    fn test_line_map_simple() {
        let map = line_map_from_source("hello\nworld\n");
        assert_eq!(map, vec![0, 6, 12]);
    }

    #[test]
    fn test_line_map_single_line() {
        let map = line_map_from_source("no newline");
        assert_eq!(map, vec![0]);
    }

    #[test]
    fn test_line_map_empty() {
        let map = line_map_from_source("");
        assert_eq!(map, vec![0]);
    }

    #[test]
    fn typecheck_rejects_unresolved_inferred_binding_before_enrichment() {
        let source = "fn main() {\n    let f = |x| x;\n}\n";
        let program = parse_source(source, "main.hew").expect("source should parse");
        let target = TargetSpec::from_requested(None).expect("host target");

        let err = typecheck_program(
            &program,
            source,
            "main.hew",
            &target,
            &CompileOptions::default(),
        );
        let Err(err) = err else {
            panic!("typecheck should reject unresolved inferred bindings");
        };
        assert_eq!(err, "type errors found");

        let mut checker = hew_types::Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);
        assert!(
            output
                .errors
                .iter()
                .any(|error| matches!(error.kind, TypeErrorKind::InferenceFailed)),
            "expected typecheck-stage InferenceFailed, got: {:?}",
            output.errors
        );
    }

    #[test]
    fn build_module_graph_reuses_resolved_file_imports_for_diamond_deps() {
        let fixture = TestFixtureDir::new(
            "diamond-file-import-cache",
            &[
                (
                    "module_diamond.hew",
                    r#"import "diamond_base.hew";
import "diamond_left.hew";
import "diamond_right.hew";

fn main() {
    let b = base_value();
    let l = left_value();
    let r = right_value();
    println(f"{b}");
    println(f"{l}");
    println(f"{r}");
    println(f"{l + r}");
}
"#,
                ),
                ("diamond_base.hew", "pub fn base_value() -> i64 { 100 }\n"),
                (
                    "diamond_left.hew",
                    "import \"diamond_base.hew\";\npub fn left_value() -> i64 { base_value() + 1 }\n",
                ),
                (
                    "diamond_right.hew",
                    "import \"diamond_base.hew\";\npub fn right_value() -> i64 { base_value() + 2 }\n",
                ),
            ],
        );
        let root_path = fixture.join("module_diamond.hew");
        let root_label = root_path.display().to_string();
        let root_source = fs::read_to_string(&root_path).expect("root fixture should be readable");
        let mut program = parse_source(&root_source, &root_label).expect("fixture should parse");
        let mut ctx = ImportResolutionContext {
            in_progress_imports: HashSet::new(),
            resolved_imports: HashMap::new(),
            manifest_deps: None,
            extra_pkg_path: None,
            locked_versions: None,
            package_name: None,
            project_dir: &fixture.path,
        };

        let module_graph = build_module_graph(
            &root_path,
            &mut program.items,
            program.module_doc.clone(),
            &mut ctx,
        )
        .expect("diamond fixture should build a module graph");
        program.module_graph = Some(module_graph);

        let left_import = find_file_import(&program.items, "diamond_left.hew");
        let left_items = left_import
            .resolved_items
            .as_ref()
            .expect("left import should be resolved");
        assert!(
            find_file_import(left_items, "diamond_base.hew")
                .resolved_items
                .is_some(),
            "left branch should reuse resolved diamond_base import items"
        );

        let right_import = find_file_import(&program.items, "diamond_right.hew");
        let right_items = right_import
            .resolved_items
            .as_ref()
            .expect("right import should be resolved");
        assert!(
            find_file_import(right_items, "diamond_base.hew")
                .resolved_items
                .is_some(),
            "right branch should reuse resolved diamond_base import items"
        );

        let mut checker = hew_types::Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);
        let unresolved: Vec<_> = output
            .errors
            .iter()
            .filter(|error| error.kind == TypeErrorKind::UnresolvedImport)
            .collect();
        assert!(
            unresolved.is_empty(),
            "diamond fixture should not produce unresolved import errors: {unresolved:?}"
        );
        assert!(
            output.errors.is_empty(),
            "diamond fixture should fully type-check: {:?}",
            output.errors
        );
    }

    #[test]
    fn build_module_graph_keeps_transitive_file_imports_visible_inside_imported_modules() {
        let fixture = TestFixtureDir::new(
            "transitive-file-import-cache",
            &[
                (
                    "module_transitive.hew",
                    r#"import "trans_mid.hew";

fn main() {
    let x = mid_fn();
    println(x);
}
"#,
                ),
                (
                    "trans_mid.hew",
                    "import \"trans_base.hew\";\npub fn mid_fn() -> i64 { base_fn() + 1 }\n",
                ),
                ("trans_base.hew", "pub fn base_fn() -> i64 { 42 }\n"),
            ],
        );
        let root_path = fixture.join("module_transitive.hew");
        let root_label = root_path.display().to_string();
        let root_source = fs::read_to_string(&root_path).expect("root fixture should be readable");
        let mut program = parse_source(&root_source, &root_label).expect("fixture should parse");
        let mut ctx = ImportResolutionContext {
            in_progress_imports: HashSet::new(),
            resolved_imports: HashMap::new(),
            manifest_deps: None,
            extra_pkg_path: None,
            locked_versions: None,
            package_name: None,
            project_dir: &fixture.path,
        };

        let module_graph = build_module_graph(
            &root_path,
            &mut program.items,
            program.module_doc.clone(),
            &mut ctx,
        )
        .expect("transitive fixture should build a module graph");
        program.module_graph = Some(module_graph);

        let mut checker = hew_types::Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);
        assert!(
            output.errors.is_empty(),
            "transitive fixture should fully type-check: {:?}",
            output.errors
        );
    }

    // ── multi-file diagnostic source routing ───────────────────────────────────
    //
    // These tests prove that type errors originating in non-root module bodies
    // carry the correct `source_module` tag AND that the shared diagnostic
    // module-source map correctly maps module names to their source files.

    /// A type error in an imported module body must have `source_module` set to
    /// the imported module's dotted name, not `None`.
    ///
    /// Regression guard for the checker→CLI diagnostic-envelope path surfacing
    /// non-root module body errors with the right origin tag.
    #[test]
    fn non_root_module_body_error_carries_source_module_tag() {
        let fixture = TestFixtureDir::new(
            "non-root-body-diag-source-module",
            &[
                ("main.hew", "import \"dep.hew\";\n\nfn main() {}\n"),
                // dep.hew: fn mistyped() -> i64 { true }  — body returns bool
                ("dep.hew", "pub fn mistyped() -> i64 { true }\n"),
            ],
        );

        let root_path = fixture.join("main.hew");
        let root_label = root_path.display().to_string();
        let root_source = fs::read_to_string(&root_path).expect("root fixture must be readable");

        let mut program = parse_source(&root_source, &root_label).expect("fixture must parse");
        let mut ctx = ImportResolutionContext {
            in_progress_imports: HashSet::new(),
            resolved_imports: HashMap::new(),
            manifest_deps: None,
            extra_pkg_path: None,
            locked_versions: None,
            package_name: None,
            project_dir: &fixture.path,
        };
        let module_graph = build_module_graph(
            &root_path,
            &mut program.items,
            program.module_doc.clone(),
            &mut ctx,
        )
        .expect("fixture must build a module graph");
        program.module_graph = Some(module_graph);

        let mut checker = hew_types::Checker::new(ModuleRegistry::new(vec![]));
        let tco = checker.check_program(&program);

        assert!(
            !tco.errors.is_empty(),
            "expected type errors from non-root module body; got none"
        );
        // Every error must be tagged with the dep module name, not root.
        // Module name is derived from the file path: "dep.hew" → "dep".
        for err in &tco.errors {
            assert!(
                err.source_module.is_some(),
                "error from non-root module body must carry source_module; got None for: {err:?}"
            );
            let mod_name = err.source_module.as_deref().unwrap();
            assert!(
                mod_name.contains("dep"),
                "error source_module should contain 'dep'; got {mod_name:?} for: {err:?}"
            );
        }
    }

    /// `diagnostic::build_module_source_map` must populate an entry for each non-root
    /// module that has a source file on disk.
    #[test]
    fn build_module_source_map_populates_non_root_entries() {
        let fixture = TestFixtureDir::new(
            "source-map-populate",
            &[
                ("main.hew", "import \"util.hew\";\nfn main() {}\n"),
                ("util.hew", "pub fn helper() -> i64 { 1 }\n"),
            ],
        );

        let root_path = fixture.join("main.hew");
        let root_label = root_path.display().to_string();
        let root_source = fs::read_to_string(&root_path).expect("root fixture must be readable");

        let mut program = parse_source(&root_source, &root_label).expect("fixture must parse");
        let mut ctx = ImportResolutionContext {
            in_progress_imports: HashSet::new(),
            resolved_imports: HashMap::new(),
            manifest_deps: None,
            extra_pkg_path: None,
            locked_versions: None,
            package_name: None,
            project_dir: &fixture.path,
        };
        let module_graph = build_module_graph(
            &root_path,
            &mut program.items,
            program.module_doc.clone(),
            &mut ctx,
        )
        .expect("fixture must build a module graph");
        program.module_graph = Some(module_graph);

        let source_map = crate::diagnostic::build_module_source_map(&program);

        assert!(
            !source_map.is_empty(),
            "source map must contain at least the util module entry"
        );
        let has_util = source_map.keys().any(|k| k.contains("util"));
        assert!(
            has_util,
            "source map must contain an entry for the 'util' module; keys: {:?}",
            source_map.keys().collect::<Vec<_>>()
        );

        let (util_key, (util_src, _)) = source_map
            .iter()
            .find(|(k, _)| k.contains("util"))
            .expect("util entry must exist");
        assert!(
            util_src.contains("helper"),
            "source map entry for {util_key} must contain 'helper'; got: {util_src:?}"
        );
    }

    /// Root module errors must still have `source_module = None` even in a
    /// multi-file program.
    #[test]
    fn root_module_error_source_module_is_none_in_multi_file_program() {
        let fixture = TestFixtureDir::new(
            "root-error-no-source-module",
            &[
                // Root has the type error; dep is fine.
                ("main.hew", "import \"ok.hew\";\nfn bad() -> i64 { true }\n"),
                ("ok.hew", "pub fn fine() -> i64 { 1 }\n"),
            ],
        );

        let root_path = fixture.join("main.hew");
        let root_label = root_path.display().to_string();
        let root_source = fs::read_to_string(&root_path).expect("root fixture must be readable");

        let mut program = parse_source(&root_source, &root_label).expect("fixture must parse");
        let mut ctx = ImportResolutionContext {
            in_progress_imports: HashSet::new(),
            resolved_imports: HashMap::new(),
            manifest_deps: None,
            extra_pkg_path: None,
            locked_versions: None,
            package_name: None,
            project_dir: &fixture.path,
        };
        let module_graph = build_module_graph(
            &root_path,
            &mut program.items,
            program.module_doc.clone(),
            &mut ctx,
        )
        .expect("fixture must build a module graph");
        program.module_graph = Some(module_graph);

        let mut checker = hew_types::Checker::new(ModuleRegistry::new(vec![]));
        let tco = checker.check_program(&program);

        assert!(
            !tco.errors.is_empty(),
            "expected type errors from root module; got none"
        );
        for err in &tco.errors {
            assert!(
                err.source_module.is_none(),
                "root module error must have source_module = None; got {:?}",
                err.source_module
            );
        }
    }

    /// Registration-phase errors (e.g. duplicate function definitions) emitted
    /// by `collect_functions` for a non-root module must be tagged with that
    /// module's name, not left as `None`.
    #[test]
    fn registration_phase_error_tagged_with_non_root_module() {
        let fixture = TestFixtureDir::new(
            "reg-phase-source-module",
            &[
                ("main.hew", "import \"dep.hew\";\nfn main() {}\n"),
                // dep.hew has two functions with the same name — duplicate_definition
                // error emitted during collect_functions.
                (
                    "dep.hew",
                    "pub fn dup() -> i64 { 1 }\npub fn dup() -> i64 { 2 }\n",
                ),
            ],
        );

        let root_path = fixture.join("main.hew");
        let root_label = root_path.display().to_string();
        let root_source = fs::read_to_string(&root_path).expect("root fixture must be readable");

        let mut program = parse_source(&root_source, &root_label).expect("fixture must parse");
        let mut ctx = ImportResolutionContext {
            in_progress_imports: HashSet::new(),
            resolved_imports: HashMap::new(),
            manifest_deps: None,
            extra_pkg_path: None,
            locked_versions: None,
            package_name: None,
            project_dir: &fixture.path,
        };
        let module_graph = build_module_graph(
            &root_path,
            &mut program.items,
            program.module_doc.clone(),
            &mut ctx,
        )
        .expect("fixture must build a module graph");
        program.module_graph = Some(module_graph);

        let mut checker = hew_types::Checker::new(ModuleRegistry::new(vec![]));
        let tco = checker.check_program(&program);

        assert!(
            !tco.errors.is_empty(),
            "expected duplicate-definition error from dep module; got none"
        );
        let dep_errors: Vec<_> = tco
            .errors
            .iter()
            .filter(|e| e.source_module.as_deref() == Some("dep"))
            .collect();
        assert!(
            !dep_errors.is_empty(),
            "registration-phase error from dep.hew must have source_module = Some(\"dep\"); \
             errors: {:?}",
            tco.errors
                .iter()
                .map(|e| &e.source_module)
                .collect::<Vec<_>>()
        );
    }
}

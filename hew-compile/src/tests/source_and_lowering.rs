use std::fs;
use std::path::Path;

use hew_parser::ast::Item;

use super::write_source;
use crate::diagnostics::display_path;
use crate::module_graph::attach_prelude_std_modules;
use crate::module_loader::module_id_from_file;
use crate::{
    module_membership, parse_source, run_document_frontend, run_file_frontend_to_typecheck,
    run_source_frontend, DiagnosticPolicy, FrontendDiagnosticKind, FrontendOptions, Session,
    SessionTarget, SourceSet,
};

/// Lower `input` through the shared session and report, for each named
/// function, the source file its body's sites index.
fn body_files(input: &str, names: &[&str]) -> Vec<Option<std::path::PathBuf>> {
    let state = run_document_frontend(input, &FrontendOptions::default());
    assert!(state.stopped.is_none(), "{:?}", state.diagnostics);
    let tco = state
        .typecheck_result
        .as_ref()
        .and_then(|result| result.tco.as_ref())
        .unwrap();
    let output = Session::new(SessionTarget::native(), DiagnosticPolicy::default())
        .lower_program(&state.program, tco)
        .expect("the program lowers");
    let module = &output.semantics().module;
    let display = hew_mir::physical::debug_names::display_names(module);
    names
        .iter()
        .map(|name| {
            let function = module.functions.iter().find(|function| {
                display
                    .get(&function.callable)
                    .is_some_and(|display| display.text == *name)
            });
            let Some(function) = function else {
                let known: Vec<_> = display.values().map(|name| &name.text).collect();
                panic!("no body named {name}; bodies: {known:?}");
            };
            let file = module.debug.item_file(function.id)?;
            Some(output.sources.get(&file)?.path.clone())
        })
        .collect()
}

#[test]
fn imported_bodies_index_their_own_source_file() {
    let dir = tempfile::tempdir().unwrap();
    write_source(
        dir.path(),
        "util.hew",
        "pub fn triple(n: i64) -> i64 {\n    n * 3\n}\n",
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        "import \"util.hew\";\nfn main() {\n    println(util.triple(2));\n}\n",
    );
    let files = body_files(&input, &["main", "util.triple"]);
    let root = dir.path().canonicalize().unwrap();
    assert_eq!(files[0].as_deref(), Some(root.join("main.hew").as_path()));
    assert_eq!(files[1].as_deref(), Some(root.join("util.hew").as_path()));
}

#[test]
fn directory_module_peers_keep_distinct_source_files() {
    let example = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../examples/multifile/02_geometry")
        .canonicalize()
        .unwrap();
    let files = body_files(
        &example.join("main.hew").display().to_string(),
        &["geo.distance.abs_int", "geo.area.bounding_area"],
    );
    assert_eq!(
        files[0].as_deref(),
        Some(example.join("geo/distance.hew").as_path())
    );
    assert_eq!(
        files[1].as_deref(),
        Some(example.join("geo/area.hew").as_path())
    );
}

#[test]
fn hygienic_macro_call_lowers_selectively_imported_intrinsic() {
    use hew_hir::{HirExprKind, HirItem, ResolvedRef};
    use hew_types::runtime_call::{MathIntrinsic, RuntimeCallFamily};
    use hew_types::CallTarget;

    let dir = tempfile::tempdir().unwrap();
    let input = write_source(
        dir.path(),
        "macro_intrinsic.hew",
        r"
        import std.math.{sqrt};
        macro square_root { ($x:expr) => expr { sqrt($x) } }
        pub fn calculate(x: f64) -> f64 { square_root!(x) }
        macro local_square_root {
            ($name:ident) => items {
                fn sqrt(x: f64) -> f64 { x + 1.0 }
                pub fn $name(x: f64) -> f64 { sqrt(x) }
            }
        }
        local_square_root!(calculate_local)
        ",
    );
    let state = run_file_frontend_to_typecheck(&input, &FrontendOptions::default()).unwrap();
    let checked = state.typecheck_result.tco.unwrap();
    let family = RuntimeCallFamily::MathIntrinsic(MathIntrinsic::Sqrt);
    let targets: Vec<_> = checked
        .direct_call_targets
        .iter()
        .filter_map(|(span, target)| (span.module_idx == 0).then_some(target))
        .collect();
    assert_eq!(targets.len(), 2, "{targets:?}");
    assert!(
        targets.contains(&&CallTarget::Runtime(family)),
        "{targets:?}"
    );
    assert!(
        targets
            .iter()
            .any(|target| matches!(target, CallTarget::User(id)
        if checked.defs.name(*id).as_str() == "sqrt")),
        "{targets:?}"
    );

    let hir = hew_hir::lower_program_host_target(&state.program, &checked, &hew_hir::ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let call = hir
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == "calculate" => {
                function.body.tail.as_deref()
            }
            _ => None,
        })
        .unwrap();
    let HirExprKind::Call { callee, .. } = &call.kind else {
        panic!("expected a lowered square root call, got {:?}", call.kind);
    };
    assert!(matches!(
        callee.kind,
        HirExprKind::BindingRef { resolved: ResolvedRef::Builtin(found), .. } if found == family
    ));
}

#[test]
fn embedded_prelude_lowers_vector_get_and_println_once() {
    let mut parsed = hew_parser::parse(
        "fn main() { let values = [1, 2]; println(values.get(0).expect(\"missing\")); \
         let text = \"a😀b\"; println(text.len()); println(text.find_from(\"b\", 1).expect(\"missing\")); }",
    );
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    attach_prelude_std_modules(&mut parsed.program);
    let mut checker =
        hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "{:?}", facts.errors);
    let lowered =
        hew_hir::lower_program_host_target(&parsed.program, &facts, &hew_hir::ResolutionCtx);
    assert!(lowered.diagnostics.is_empty(), "{:?}", lowered.diagnostics);
}

/// A quoted import above the root file's directory is named relative to
/// it, never by the absolute path of the checkout.
#[test]
fn file_module_ids_are_relative_to_the_root_directory() {
    let root = Path::new("/work/project/app");
    let id = |file: &str| module_id_from_file(root, Path::new(file)).dotted();
    assert_eq!(id("/work/project/app/main.hew"), "main");
    assert_eq!(id("/work/project/app/x/util.hew"), "x.util");
    assert_eq!(id("/work/project/a/util.hew"), "super.a.util");
    assert_eq!(id("/work/other/b/util.hew"), "super.super.other.b.util");
}

#[test]
fn file_module_ids_preserve_exceptional_components_and_relocation() {
    let root = Path::new("/work/project/app");
    let id = |file: &str| module_id_from_file(root, Path::new(file));
    assert_ne!(
        id("/work/project/util.hew"),
        id("/work/project/app/super/util.hew")
    );
    assert_ne!(
        id("/work/project/app/super/util.hew"),
        id("/work/project/app/__hew_file_name_7375706572/util.hew")
    );
    assert_ne!(
        id("/work/project/app/a.b/util.hew"),
        id("/work/project/app/a/b/util.hew")
    );
    assert_ne!(id("/work/project/app.hew"), id("/work/project/app/app.hew"));
    assert_eq!(id("/work/project/app.hew").dotted(), "super.app");
    assert_ne!(
        id("/work/project/app/util.hew"),
        id("/work/project/app/util.txt")
    );
    assert_ne!(
        id("/work/project/app/util.hew"),
        id("/work/project/app/util")
    );
    assert_eq!(
        id("/work/project/lib/util.hew"),
        module_id_from_file(
            Path::new("/another/checkout/app"),
            Path::new("/another/checkout/lib/util.hew")
        )
    );
}

#[cfg(unix)]
#[test]
fn file_module_ids_preserve_non_utf8_components() {
    use std::os::unix::ffi::OsStringExt;
    let root = Path::new("/work/app");
    let file = |byte| {
        root.join(std::ffi::OsString::from_vec(vec![byte]))
            .join("util.hew")
    };
    let first = module_id_from_file(root, &file(0xff));
    assert_ne!(first, module_id_from_file(root, &file(0xfe)));
    assert_ne!(first, module_id_from_file(root, &root.join("util.hew")));
}

#[cfg(windows)]
#[test]
fn file_module_ids_preserve_windows_volume_and_unc_prefixes() {
    let root = Path::new(r"C:\work\app");
    let id = |file: &str| module_id_from_file(root, Path::new(file));
    assert_ne!(id(r"D:\other\util.hew"), id(r"E:\other\util.hew"));
    assert_ne!(id(r"\\host\one\util.hew"), id(r"\\host\two\util.hew"));
    assert_ne!(
        id(r"D:\other\util.hew"),
        id(r"C:\work\app\D\other\util.hew")
    );
    assert_eq!(id(r"C:\work\lib\util.hew").dotted(), "super.lib.util");
}

/// A diagnostic label must name the file the way the user did, never
/// `canonicalize()`'s Windows extended-length verbatim form (#3416).
#[test]
fn display_path_strips_windows_verbatim_prefix() {
    assert_eq!(
        display_path(Path::new(r"\\?\D:\a\hew\hew\tests\token.hew")),
        r"D:\a\hew\hew\tests\token.hew",
    );
    assert_eq!(
        display_path(Path::new(r"\\?\UNC\server\share\token.hew")),
        r"\\server\share\token.hew",
    );
    // Negative control: a path never canonicalized on Windows, and every
    // POSIX path, must render unchanged.
    assert_eq!(
        display_path(Path::new("/a/hew/tests/token.hew")),
        "/a/hew/tests/token.hew",
    );
}

/// An unsaved buffer checks against its saved siblings: the driver reads
/// the open document for `lib.hew` and the file on disk for everything
/// else.
#[test]
fn an_open_buffer_overrides_the_file_on_disk() {
    let dir = tempfile::tempdir().expect("create document-overlay fixture");
    write_source(dir.path(), "lib.hew", "pub fn answer() -> i64 { 1 }\n");
    let input = write_source(
        dir.path(),
        "main.hew",
        "import \"lib.hew\";\n\nfn main() { println(answer() + bonus()); }\n",
    );

    // Negative control: the saved `lib.hew` has no `bonus`.
    let saved = run_document_frontend(&input, &FrontendOptions::default());
    assert!(
        saved.stopped.is_some(),
        "the saved sibling declares no `bonus`: {:#?}",
        saved.diagnostics
    );

    let mut documents = SourceSet::new();
    documents.insert(
        dir.path().join("lib.hew"),
        "pub fn answer() -> i64 { 1 }\npub fn bonus() -> i64 { 2 }\n",
    );
    let options = FrontendOptions {
        documents,
        ..FrontendOptions::default()
    };
    let open = run_document_frontend(&input, &options);
    assert!(
        open.stopped.is_none(),
        "the open buffer declares `bonus`: {:#?}",
        open.diagnostics
    );
}

#[test]
fn imported_external_impl_credits_direct_and_umbrella_import_routes() {
    let dir = tempfile::tempdir().unwrap();
    write_source(
        dir.path(),
        "methods.hew",
        "impl NodeConfig { pub fn answer() -> i64 { 42 } }",
    );
    write_source(dir.path(), "umbrella.hew", "import methods;");
    write_source(dir.path(), "idle.hew", "pub fn unrelated() -> i64 { 0 }");
    for import in [
        "import methods;",
        "import umbrella;",
        "import umbrella as tools;",
    ] {
        let source = format!("{import}\nimport idle;\nfn main() -> i64 {{ NodeConfig.answer() }}");
        let input = write_source(dir.path(), "main.hew", &source);
        let state = run_document_frontend(&input, &FrontendOptions::default());
        assert!(state.stopped.is_none(), "{:?}", state.diagnostics);
        let output = state
            .typecheck_result
            .as_ref()
            .and_then(|result| result.tco.as_ref())
            .unwrap();
        let unused: Vec<_> = output
            .warnings
            .iter()
            .filter(|warning| warning.kind == hew_types::error::TypeErrorKind::UnusedImport)
            .map(|warning| warning.message.as_str())
            .collect();
        assert_eq!(
            unused,
            ["unused import: `idle`"],
            "{import}: {:?}",
            output.warnings
        );
    }
    let input = write_source(
        dir.path(),
        "main.hew",
        "fn main() -> i64 { NodeConfig.answer() }",
    );
    let missing = run_document_frontend(&input, &FrontendOptions::default());
    assert!(
        missing.stopped.is_some(),
        "the supplying import is required"
    );
}

/// A buffer with no file behind it runs the same frontend, including the
/// implicit `std.text.regex` import a regex literal needs.
#[test]
fn a_buffer_without_a_file_gets_the_implicit_regex_import() {
    let source = "fn main() { let pattern = re\"a+\"; println(pattern.is_match(\"aaa\")); }\n";
    let state = run_source_frontend(source, "<buffer>", &FrontendOptions::default());
    assert!(
        state.stopped.is_none(),
        "the driver injects the regex import: {:#?}",
        state.diagnostics
    );
}

/// The editors need the checker output of a run that reported errors, not
/// only the failure.
#[test]
fn a_stopped_run_still_carries_its_checker_output() {
    let state = run_source_frontend(
        "fn main() { let x: i64 = \"text\"; println(x); }\n",
        "<buffer>",
        &FrontendOptions::default(),
    );
    let stopped = state.stopped.as_ref().expect("the assignment is ill-typed");
    assert_eq!(stopped.message, "type errors found");
    let tco = state
        .typecheck_result
        .as_ref()
        .and_then(|result| result.tco.as_ref())
        .expect("a type-checked run keeps its checker output");
    assert!(!tco.errors.is_empty());
    assert!(state.parse_result.is_some());
}

#[test]
fn selected_occurrence_never_falls_back_to_authored_main() {
    let dir = tempfile::tempdir().expect("create selected-entry fixture");
    let source = "fn main() {}\n\n#[test]\nfn selected_test() {}\n";
    let input = write_source(dir.path(), "entry_test.hew", source);
    let program = parse_source(source, &input).expect("parse selected-entry fixture");
    let selection = program
        .items
        .iter()
        .enumerate()
        .find_map(|(item_ordinal, (item, span))| match item {
            Item::Function(function)
                if function
                    .attributes
                    .iter()
                    .any(|attribute| attribute.name == "test") =>
            {
                Some(
                    hew_types::DeclarationOccurrence::new_with_synthetic_ordinal(
                        None,
                        span,
                        item_ordinal,
                        hew_types::DeclarationKind::Function,
                        0,
                    ),
                )
            }
            _ => None,
        })
        .expect("selected test occurrence");

    let options = FrontendOptions {
        entry_selection: Some(selection),
        ..FrontendOptions::default()
    };
    let state = run_file_frontend_to_typecheck(&input, &options)
        .expect("selected-entry fixture must type-check");

    let tco = state.typecheck_result.tco.expect("typecheck output");
    assert_eq!(
        tco.defs
            .display(tco.entry_exit_plan.expect("selected entry plan").entry),
        "selected_test",
        "a present selection must not fall back to authored main"
    );
}

#[test]
fn selected_occurrence_survives_directory_module_entry_import() {
    let dir = tempfile::tempdir().expect("create directory-module fixture");
    fs::write(dir.path().join("hew.toml"), "[package]\nname = \"app\"\n").expect("write manifest");
    let module_dir = dir.path().join("greeting");
    fs::create_dir(&module_dir).expect("create module directory");
    write_source(
        &module_dir,
        "greeting.hew",
        "pub trait Greeter {\n    fn greet(self);\n}\n",
    );
    let source = concat!(
        "type Dog {}\n",
        "impl Greeter for Dog {\n    fn greet(self) {}\n}\n",
        "#[test]\n",
        "fn selected_test() {}\n",
    );
    let input = write_source(&module_dir, "dog_test.hew", source);
    let program = parse_source(source, &input).expect("parse selected-entry fixture");
    let selection = program
        .items
        .iter()
        .enumerate()
        .find_map(|(item_ordinal, (item, span))| match item {
            Item::Function(function) if function.name.name.as_str() == "selected_test" => Some(
                hew_types::DeclarationOccurrence::new_with_synthetic_ordinal(
                    None,
                    span,
                    item_ordinal,
                    hew_types::DeclarationKind::Function,
                    0,
                ),
            ),
            _ => None,
        })
        .expect("selected test occurrence");

    let state = run_file_frontend_to_typecheck(
        &input,
        &FrontendOptions {
            project_dir: Some(dir.path().to_path_buf()),
            entry_selection: Some(selection),
            companion: module_membership(Path::new(&input), &FrontendOptions::default()).and_then(
                |membership| {
                    let input = Path::new(&input).canonicalize().ok()?;
                    membership.test_companion(&input).map(Path::to_path_buf)
                },
            ),
            ..FrontendOptions::default()
        },
    )
    .expect("selected occurrence must survive the module companion import");

    let tco = state.typecheck_result.tco.expect("typecheck output");
    assert_eq!(
        tco.defs
            .display(tco.entry_exit_plan.expect("selected entry plan").entry),
        "selected_test"
    );
}

#[test]
fn selected_stdlib_tests_keep_their_canonical_source_module() {
    let input = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("std/concurrency/lifecycle.hew")
        .display()
        .to_string();
    let source = fs::read_to_string(&input).unwrap();
    let program = parse_source(&source, &input).unwrap();
    let selection = program
        .items
        .iter()
        .enumerate()
        .find_map(|(ordinal, (item, span))| {
            let Item::Function(function) = item else {
                return None;
            };
            (function.name.name.as_str() == "lifecycle_i64_happy_path_state_names").then(|| {
                hew_types::DeclarationOccurrence::new_with_synthetic_ordinal(
                    None,
                    span,
                    ordinal,
                    hew_types::DeclarationKind::Function,
                    0,
                )
            })
        })
        .expect("inline lifecycle test fixture");
    let state = run_file_frontend_to_typecheck(
        &input,
        &FrontendOptions {
            test_entry_selections: vec![selection],
            host_admission: crate::HostAdmission::Tests(vec![selection]),
            ..FrontendOptions::default()
        },
    )
    .expect("inline std test must remain selectable after graph-root rewriting");
    let output = state.typecheck_result.tco.unwrap();
    assert_eq!(output.test_entry_plans.len(), 1);
    assert_eq!(
        output.defs.path(output.test_entry_plans[0].entry),
        "std.concurrency.lifecycle.lifecycle_i64_happy_path_state_names"
    );
    assert!(output.entry_exit_plan.is_none());
    Session::new(SessionTarget::native(), DiagnosticPolicy::default())
        .lower_program(&state.program, &output)
        .expect("selected private std test and its helpers must lower through SIR");
}

#[test]
fn missing_selected_occurrence_is_a_type_error() {
    let dir = tempfile::tempdir().expect("create selected-entry fixture");
    let source = "fn main() {}\n\n#[test]\nfn selected_test() {}\n";
    let input = write_source(dir.path(), "entry_test.hew", source);
    let missing = hew_types::DeclarationOccurrence::new(
        None,
        &(source.len() + 1..source.len() + 2),
        hew_types::DeclarationKind::Function,
        0,
    );

    let options = FrontendOptions {
        entry_selection: Some(missing),
        ..FrontendOptions::default()
    };
    let Err(failure) = run_file_frontend_to_typecheck(&input, &options) else {
        panic!("a missing selected entry must fail closed");
    };

    assert!(failure.diagnostics.iter().any(|diagnostic| {
        matches!(
            &diagnostic.kind,
            FrontendDiagnosticKind::Type(error)
                if error.message.contains("selected process entry occurrence")
        )
    }));
}

#[test]
fn selected_occurrence_from_another_module_is_rejected() {
    let dir = tempfile::tempdir().expect("create selected-entry fixture");
    write_source(dir.path(), "helper.hew", "pub fn value() -> i64 { 1 }\n");
    let source =
        "import helper;\n\nfn main() {}\n\n#[test]\nfn selected_test() { assert(true); }\n";
    let input = write_source(dir.path(), "entry_test.hew", source);
    let initial = run_file_frontend_to_typecheck(&input, &FrontendOptions::default())
        .expect("fixture must establish module identities");
    let helper_module = initial
        .typecheck_result
        .tco
        .as_ref()
        .expect("typecheck output")
        .defs
        .module_for_path("helper")
        .expect("imported helper module identity");
    let program = parse_source(source, &input).expect("parse selected-entry fixture");
    let foreign_selection = program
        .items
        .iter()
        .enumerate()
        .find_map(|(item_ordinal, (item, span))| match item {
            Item::Function(function) if function.name.name.as_str() == "selected_test" => Some(
                hew_types::DeclarationOccurrence::new_with_synthetic_ordinal(
                    Some(helper_module),
                    span,
                    item_ordinal,
                    hew_types::DeclarationKind::Function,
                    0,
                ),
            ),
            _ => None,
        })
        .expect("selected test occurrence");
    let options = FrontendOptions {
        entry_selection: Some(foreign_selection),
        ..FrontendOptions::default()
    };

    let Err(failure) = run_file_frontend_to_typecheck(&input, &options) else {
        panic!("a foreign-module occurrence must not select a root function");
    };

    assert!(failure.diagnostics.iter().any(|diagnostic| {
        matches!(
            &diagnostic.kind,
            FrontendDiagnosticKind::Type(error)
                if error.message.contains("selected process entry occurrence")
        )
    }));
}

/// A `std/` beside the source file is not the standard library: a source
/// inside a directory shaped like a Hew checkout still resolves std from
/// the toolchain's root, never from that directory.
#[test]
fn default_std_roots_ignore_a_std_beside_the_source() {
    let root = tempfile::tempdir().expect("create lookalike checkout root");
    fs::create_dir_all(root.path().join("std")).expect("create std dir");
    fs::write(root.path().join("std/builtins.hew"), "// marker\n").expect("write builtins marker");
    write_source(root.path(), "main.hew", "fn main() {}\n");

    let paths = FrontendOptions::default().std_roots();

    assert!(
        !paths.contains(&root.path().to_path_buf()),
        "a std beside the source must not be selected: {paths:?}"
    );
}

/// A diamond of module imports (`main` -> `left`, `right` -> `base`)
/// leaves the module DFS free to visit `left` or `right` first. HIR
/// lowering walks `topo_order` to order function bodies and to mint
/// `BindingId`s, so that choice must be the same on every compile: the
/// raw MIR dump (function order, binding and site ids) must be
/// byte-identical across repeated in-process compiles of one program.
#[test]
fn diamond_module_imports_lower_identically_across_repeated_compiles() {
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-compile has a workspace parent")
        .to_path_buf();
    let dir = tempfile::tempdir().expect("create temp dir");
    write_source(
        dir.path(),
        "base.hew",
        "pub fn base_value() -> i64 {\n    let x = 100;\n    let y = x + 1;\n    y\n}\n",
    );
    write_source(
        dir.path(),
        "left.hew",
        "import base;\n\npub fn left_value() -> i64 {\n    let a = base.base_value();\n    let b = a + 1;\n    b\n}\n",
    );
    write_source(
        dir.path(),
        "right.hew",
        "import base;\n\npub fn right_value() -> i64 {\n    let c = base.base_value();\n    let d = c + 2;\n    d\n}\n",
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        "import left;\nimport right;\n\nfn main() {\n    let l = left.left_value();\n    let r = right.right_value();\n    let total = l + r;\n    println(f\"{total}\");\n}\n",
    );

    let session = Session::new(SessionTarget::native(), DiagnosticPolicy::default());
    let dumps: Vec<String> = (0..20)
        .map(|_| {
            let state = run_file_frontend_to_typecheck(
                &input,
                &FrontendOptions {
                    module_search_paths: Some(vec![repo_root.clone()]),
                    ..FrontendOptions::default()
                },
            )
            .expect("diamond fixture must type-check");
            let tco = state
                .typecheck_result
                .tco
                .as_ref()
                .expect("type checking was enabled");
            let hir = hew_hir::lower_program(
                &state.program,
                tco,
                &hew_hir::ResolutionCtx,
                hew_hir::TargetArch::host(),
            );
            assert!(
                hir.diagnostics.is_empty(),
                "diamond fixture must lower without HIR diagnostics: {:#?}",
                hir.diagnostics
            );
            let output = session
                .lower_hir_module(&hir.module, tco, &[])
                .expect("diamond fixture must verify");
            hew_sir::dump_lowering(output.semantics())
        })
        .collect();
    assert!(
        dumps[0].contains("fn __hew_fn_left$left_value")
            && dumps[0].contains("fn __hew_fn_right$right_value"),
        "dump must contain both imported functions:\n{}",
        dumps[0]
    );
    for (run, dump) in dumps.iter().enumerate().skip(1) {
        assert_eq!(
            dump, &dumps[0],
            "raw MIR dump of run {run} differs from run 0"
        );
    }
}

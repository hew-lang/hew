use std::fs;
use std::path::Path;

use hew_parser::ast::Item;

use super::write_source;
use crate::{
    check_file, run_file_frontend_to_typecheck, DiagnosticPolicy, FrontendDiagnosticKind,
    FrontendOptions, Session, SessionTarget,
};

#[test]
fn source_roots_keep_authored_exports_beside_file_imports() {
    let dir = tempfile::tempdir().expect("create source-root fixture");
    write_source(
        dir.path(),
        "helper.hew",
        "fn hidden() -> string { \"owned\".to_upper() } pub fn imported() -> string { hidden() }",
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        "import \"helper.hew\"; pub fn exported() -> string { imported() } fn main() {}",
    );
    let state = run_file_frontend_to_typecheck(&input, &FrontendOptions::default()).unwrap();
    let tco = state.typecheck_result.tco.as_ref().unwrap();
    let roots = Session::source_roots(&state.program, tco).unwrap();
    assert_eq!(roots.len(), 1, "file imports are not implicit root exports");
    assert_eq!(
        tco.defs.name(roots[0]),
        hew_types::Symbol::intern("exported")
    );
    let output = Session::new(SessionTarget::native(), DiagnosticPolicy::default())
        .lower_program(&state.program, tco)
        .expect("an uncalled root export must retain its imported helper closure");
    let module = &output.semantics().module;
    for leaf in ["exported", "imported", "hidden"] {
        let declaration = tco
            .defs
            .declarations()
            .map(|(_, declaration)| declaration)
            .find(|declaration| tco.defs.name(*declaration) == hew_types::Symbol::intern(leaf))
            .unwrap();
        let callable = module
            .callable_for_declaration(&declaration)
            .expect("export helper must be retained");
        assert!(module.function_index().function(callable.id).is_some());
    }
}

/// A file-imported item lives in its file's module-graph entry only. The
/// checker mints ONE declaration for it, and that identity is the one the
/// call site and the lowered HIR item carry.
#[test]
fn file_import_declares_one_identity_per_item() {
    let dir = tempfile::tempdir().expect("create file-import fixture dir");
    write_source(
        dir.path(),
        "helper.hew",
        "pub fn helper_value() -> i64 { 7 }\n",
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        "import \"helper.hew\";\n\nfn main() -> i64 { helper_value() }\n",
    );
    let state = run_file_frontend_to_typecheck(&input, &FrontendOptions::default())
        .expect("file-import fixture must type-check");
    let tco = state
        .typecheck_result
        .tco
        .as_ref()
        .expect("type checking was enabled");

    let is_helper =
        |item: &Item| matches!(item, Item::Function(f) if f.name.name.as_str() == "helper_value");
    let in_graph = state.program.module_graph.as_ref().is_some_and(|graph| {
        graph
            .modules
            .values()
            .any(|module| module.items.iter().any(|(item, _)| is_helper(item)))
    });
    let in_root_items = state.program.items.iter().any(|(item, _)| is_helper(item));
    assert!(
        in_graph && !in_root_items,
        "the helper belongs to its file's module, not the root \
         (graph: {in_graph}, root items: {in_root_items})"
    );

    let minted: Vec<hew_types::DefId> = tco
        .defs
        .declarations()
        .map(|(_, declaration)| declaration)
        .filter(|declaration| {
            tco.defs.name(*declaration) == hew_types::Symbol::intern("helper_value")
        })
        .collect();
    assert_eq!(
        minted.len(),
        1,
        "one physical declaration must mint exactly one DefId: {minted:?}"
    );

    let call_target = tco
        .direct_call_targets
        .values()
        .find_map(|target| match target {
            hew_types::check::CallTarget::User(declaration)
                if tco.defs.name(*declaration) == hew_types::Symbol::intern("helper_value") =>
            {
                Some(*declaration)
            }
            _ => None,
        })
        .expect("main's call must resolve to the helper's user target");
    assert_eq!(call_target, minted[0]);

    let hir = hew_hir::lower_program(
        &state.program,
        tco,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "file-import fixture must lower without HIR diagnostics: {:#?}",
        hir.diagnostics
    );
    let lowered: Vec<_> = hir
        .module
        .items
        .iter()
        .filter_map(|item| match item {
            hew_hir::HirItem::Function(function) if function.declaration == minted[0] => {
                Some(function)
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        lowered.len(),
        1,
        "HIR must lower the helper once, under the checker-minted identity"
    );
    assert_eq!(lowered[0].name, "helper$helper_value");
}

#[test]
fn selective_machine_import_tracks_bare_state_references() {
    let dir = tempfile::tempdir().expect("create machine import fixture");
    write_source(
        dir.path(),
        "m.hew",
        "pub machine Light { events { Flip; } state On; state Off; \
         on Flip: On => Off; on Flip: Off => On; }",
    );
    for (body, expects_unused) in [
        ("var s = Light.On; s = Light.Off; print(\"ok\");", false),
        ("print(\"ok\");", true),
    ] {
        let input = write_source(
            dir.path(),
            "main.hew",
            &format!("import m.{{ Light }};\nfn main() {{ {body} }}\n"),
        );
        let state = run_file_frontend_to_typecheck(&input, &FrontendOptions::default())
            .expect("machine import fixture must type-check");
        let facts = state.typecheck_result.tco.as_ref().expect("type facts");
        assert!(facts.errors.is_empty(), "{:#?}", facts.errors);
        assert_eq!(
            facts
                .warnings
                .iter()
                .any(|warning| { warning.kind == hew_types::error::TypeErrorKind::UnusedImport }),
            expects_unused,
            "bare state references must credit only a used import: {:#?}",
            facts.warnings
        );
    }
}

#[test]
fn quoted_file_imports_keep_private_function_symbols_by_source() {
    for (imports, main_body, expected) in [
        (
            "import \"a.hew\";",
            "fn main() { assert(f() == 1); }",
            vec!["a$f", "a$main", "main"],
        ),
        (
            "import \"a.hew\"; import \"b.hew\";",
            "fn main() { assert(a_api() + b_api() == 3); }",
            vec!["a$helper", "b$helper", "a$a_api", "b$b_api", "main"],
        ),
    ] {
        let dir = tempfile::tempdir().expect("create quoted import fixture");
        if expected.len() == 3 {
            write_source(
                dir.path(),
                "a.hew",
                "pub fn f() -> i64 { main(); 1 } fn main() {}",
            );
        } else {
            write_source(
                dir.path(),
                "a.hew",
                "fn helper() -> i64 { 1 } pub fn a_api() -> i64 { helper() }",
            );
            write_source(
                dir.path(),
                "b.hew",
                "fn helper() -> i64 { 2 } pub fn b_api() -> i64 { helper() }",
            );
        }
        let input = write_source(dir.path(), "main.hew", &format!("{imports}\n{main_body}\n"));
        let state = run_file_frontend_to_typecheck(&input, &FrontendOptions::default())
            .expect("quoted import fixture must type-check");
        let tco = state.typecheck_result.tco.as_ref().expect("type facts");
        let hir = hew_hir::lower_program(
            &state.program,
            tco,
            &hew_hir::ResolutionCtx,
            hew_hir::TargetArch::host(),
        );
        assert!(hir.diagnostics.is_empty(), "{:#?}", hir.diagnostics);
        let names: Vec<_> = hir
            .module
            .items
            .iter()
            .filter_map(|item| match item {
                hew_hir::HirItem::Function(function) => Some(function.name.as_str()),
                _ => None,
            })
            .collect();
        for symbol in expected {
            assert!(names.contains(&symbol), "missing `{symbol}` in {names:?}");
        }
    }
}

#[test]
fn dotted_imported_impl_with_iterator_has_a_callable_body() {
    let dir = tempfile::tempdir().expect("create imported method fixture");
    write_source(
        dir.path(),
        "modp3.hew",
        "pub type T { xs: Vec<f64>; }\n\
         impl T { pub fn a(self) -> f64 { var sum = 0.0;\
             for i in (0..self.xs.len()).rev() { sum += self.xs[i]; } sum } }\n\
         pub fn mk() -> T { T { xs: [1.0, 2.0] } }\n",
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        "import modp3; fn main() { let value = modp3.mk(); assert(value.a() == 3.0); }",
    );
    let state = run_file_frontend_to_typecheck(&input, &FrontendOptions::default())
        .expect("imported method fixture must type-check");
    let tco = state.typecheck_result.tco.as_ref().expect("type facts");
    let declaration = tco
        .defs
        .declarations()
        .map(|(_, declaration)| declaration)
        .find(|declaration| {
            tco.defs.kind(*declaration) == hew_types::DeclarationKind::ImplMethod
                && tco.defs.name(*declaration) == hew_types::Symbol::intern("a")
                && tco.defs.path(*declaration).starts_with("modp3.")
        })
        .expect("checked method identity");
    assert!(
        tco.imported_impl_body_facts.contains_key(&declaration),
        "a Range adapter has no runtime callee and cannot suppress its enclosing body"
    );
    let output = Session::new(SessionTarget::native(), DiagnosticPolicy::default())
        .lower_program(&state.program, tco)
        .expect("selected imported method must have a callable body");
    let callable = output
        .semantics()
        .module
        .callable_for_declaration(&declaration)
        .expect("method body is emitted");
    assert!(output
        .semantics()
        .module
        .function_index()
        .function(callable.id)
        .is_some());
}

#[test]
fn imported_private_externs_publish_exact_direct_call_symbols() {
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-compile has a workspace parent");
    let input = repo_root.join("tests/pkg-import/imported_actor_ask_i32.hew");
    let state = run_file_frontend_to_typecheck(
        input.to_str().expect("fixture path is utf-8"),
        &FrontendOptions {
            pkg_path: Some(repo_root.join("tests/pkg-import/pkgs")),
            ..FrontendOptions::default()
        },
    )
    .expect("imported actor fixture must type-check");
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
        "imported actor fixture must lower without HIR diagnostics: {:#?}",
        hir.diagnostics
    );
    let symbols = hew_hir::dispatch::build_direct_call_symbol_index(&hir.module.items);
    for name in [
        "hew_testffi_count32",
        "hew_testffi_count64",
        "hew_testffi_name",
        "hew_testffi_query",
    ] {
        let declaration = tco
            .defs
            .lookup_path(&format!("hew.testffi.{name}"))
            .expect("declared extern");
        assert_eq!(
            symbols.get(&declaration),
            Some(&name.to_string()),
            "imported private extern `{declaration:?}` must have a canonical HIR direct-call symbol"
        );
    }
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "the import-order regression keeps both declaration-owner permutations in one proof"
)]
fn mixed_file_and_package_impls_keep_declaration_owned_dispatch_in_both_import_orders() {
    // A file import shares the root checker declaration namespace, while
    // its emitted HIR body retains the source-file symbol owner. A package
    // import retains a package-qualified owner at both boundaries. The two
    // sources intentionally declare same-leaf `TestResult` / `TestResultMethods`
    // impls. The checker must select the root declaration for `local.tag()`
    // and the package declaration for `r.rows()`; HIR's direct-call index
    // must then project each declaration to its distinct emitted body.
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-compile has a workspace parent");
    let fixture_dir = repo_root.join("tests/pkg-import");
    let input = fixture_dir.join("mixed_import_impl_collision.hew");
    let package_path = fixture_dir.join("pkgs");

    let source = fs::read_to_string(&input).expect("read mixed-import fixture");
    let reversed_source = source.replacen(
        "import hew.testffi;\n\nimport \"mixed_import_impl_collision_lib.hew\";",
        "import \"mixed_import_impl_collision_lib.hew\";\n\nimport hew.testffi;",
        1,
    );
    assert_ne!(
        reversed_source, source,
        "fixture imports must be reversed for the second frontend pass"
    );
    let reversed_dir = tempfile::tempdir().expect("create reversed-import fixture dir");
    let reversed_input = write_source(
        reversed_dir.path(),
        "mixed_import_impl_collision.hew",
        &reversed_source,
    );
    fs::copy(
        fixture_dir.join("mixed_import_impl_collision_lib.hew"),
        reversed_dir
            .path()
            .join("mixed_import_impl_collision_lib.hew"),
    )
    .expect("copy mixed-import library");

    for fixture in [
        input.to_str().expect("fixture path is utf-8"),
        reversed_input.as_str(),
    ] {
        let state = run_file_frontend_to_typecheck(
            fixture,
            &FrontendOptions {
                pkg_path: Some(package_path.clone()),
                ..FrontendOptions::default()
            },
        )
        .expect("mixed-import fixture must type-check");
        let tco = state
            .typecheck_result
            .tco
            .as_ref()
            .expect("type checking was enabled");
        let root_tag = tco
            .defs
            .lookup_path(
                "mixed_import_impl_collision_lib.TestResult::<impl \
                 mixed_import_impl_collision_lib.TestResultMethods for \
                 mixed_import_impl_collision_lib.TestResult>::tag",
            )
            .expect("declared root impl method");
        let package_rows = tco
            .defs
            .lookup_path(
                "hew.testffi.TestResult::<impl hew.testffi.TestResultMethods for hew.testffi.TestResult>::rows",
            )
            .expect("declared package impl method");

        assert_eq!(
            tco.impl_method_declaration_ids.get("TestResult::tag"),
            Some(&root_tag),
            "the flattened file-import implementation must retain a root declaration ID"
        );
        assert_eq!(
            tco.impl_method_declaration_ids
                .get("hew.testffi.TestResult::rows"),
            Some(&package_rows),
            "the package implementation must retain its package-qualified declaration ID"
        );
        assert!(
            tco.method_call_rewrites.values().any(|rewrite| matches!(
                rewrite,
                hew_types::check::MethodCallRewrite::RewriteToFunction {
                    target: hew_types::check::CallTarget::ImplMethod(declaration),
                    ..
                } if declaration == &root_tag
            )),
            "local TestResult.tag() must select the root file-import declaration: {:#?}",
            tco.method_call_rewrites
        );
        assert!(
            tco.method_call_rewrites.values().any(|rewrite| matches!(
                rewrite,
                hew_types::check::MethodCallRewrite::RewriteToFunction {
                    target: hew_types::check::CallTarget::ImplMethod(declaration),
                    ..
                } if declaration == &package_rows
            )),
            "package TestResult.rows() must select the package declaration: {:#?}",
            tco.method_call_rewrites
        );

        let hir = hew_hir::lower_program(
            &state.program,
            tco,
            &hew_hir::ResolutionCtx,
            hew_hir::TargetArch::host(),
        );
        assert!(
            hir.diagnostics.is_empty(),
            "mixed imports must lower without declaration/body lookup diagnostics: {:#?}",
            hir.diagnostics
        );
        let symbols = hew_hir::dispatch::build_direct_call_symbol_index(&hir.module.items);
        assert_eq!(
            symbols.get(&root_tag),
            Some(&"mixed_import_impl_collision_lib.TestResult::tag".to_string())
        );
        assert_eq!(
            symbols.get(&package_rows),
            Some(&"hew.testffi.TestResult::rows".to_string())
        );
    }
}

#[test]
fn imported_generic_impl_bodies_publish_each_checker_owned_declaration() {
    // `privslot` deliberately combines all three conditions that
    // make a body lookup tempting to recover from a leaf spelling: its
    // module-private generic `Slot<T>` is nested in a public `Store<T>`,
    // and the consumer dispatches two inherent methods after the root body
    // was lowered.  The checker declaration is the sole identity handoff;
    // every emitted impl function must retain that exact declaration and
    // appear in the direct-body index before MIR begins.
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-compile has a workspace parent");
    let input = repo_root.join("tests/pkg-import/private_generic_record_vec_element.hew");
    let state = run_file_frontend_to_typecheck(
        input.to_str().expect("fixture path is utf-8"),
        &FrontendOptions {
            pkg_path: Some(repo_root.join("tests/pkg-import/pkgs")),
            ..FrontendOptions::default()
        },
    )
    .expect("private generic-record fixture must type-check");
    let tco = state
        .typecheck_result
        .tco
        .as_ref()
        .expect("type checking was enabled");
    let expected = [
        (
            "hew.privslot.Store::add",
            tco.defs
                .lookup_path("hew.privslot.Store::<impl inherent for hew.privslot.Store<T>>::add")
                .expect("declared impl method"),
        ),
        (
            "hew.privslot.Store::generation_at",
            tco.defs
                .lookup_path(
                    "hew.privslot.Store::<impl inherent for hew.privslot.Store<T>>::generation_at",
                )
                .expect("declared impl method"),
        ),
    ];
    for (symbol, declaration) in &expected {
        assert_eq!(
            tco.impl_method_declaration_ids.get(*symbol),
            Some(declaration),
            "checker must publish the canonical full-source symbol key `{symbol}`"
        );
    }

    let hir = hew_hir::lower_program(
        &state.program,
        tco,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "generic imported impl methods must lower without body-plan diagnostics: {:#?}",
        hir.diagnostics
    );
    let direct_symbols = hew_hir::dispatch::build_direct_call_symbol_index(&hir.module.items);
    for (symbol, declaration) in &expected {
        assert_eq!(
            direct_symbols.get(declaration),
            Some(&(*symbol).to_string()),
            "every emitted imported impl body must carry its checker-owned declaration"
        );
    }

    let roots = Session::source_roots(&state.program, tco).unwrap();
    let output = Session::new(SessionTarget::native(), DiagnosticPolicy::default())
        .lower_hir_module(&hir.module, tco, &roots)
        .expect("imported generic impl calls must complete shared semantic lowering");
    let module = &output.semantics().module;
    for (_, declaration) in expected {
        let key = hew_sir::SirInstanceKey {
            template: hew_sir::GenericTemplateId { declaration },
            type_args: vec![hew_types::ResolvedTy::String],
        };
        let callable = module
            .callable_for_instance(&key)
            .expect("each imported method must retain its exact string specialization");
        assert!(
            module.function_index().function(callable.id).is_some(),
            "the requested imported specialization must have a semantic body"
        );
    }
}

#[test]
fn local_generic_impl_calls_reuse_exact_semantic_specializations() {
    let dir = tempfile::tempdir().unwrap();
    let input = write_source(
        dir.path(),
        "main.hew",
        r#"type Holder<T> {
value: T;
}

impl<T> Holder<T> {
fn get(self) -> T {
    self.value
}
}

fn main() -> i64 {
let numbers = Holder { value: 7 };
let words = Holder { value: "kept" };
numbers.get() + numbers.get() + words.get().len() + words.get().len()
}
"#,
    );
    let state = run_file_frontend_to_typecheck(&input, &FrontendOptions::default()).unwrap();
    let tco = state.typecheck_result.tco.as_ref().unwrap();
    let declaration = tco.impl_method_declaration_ids["Holder::get"];
    let output = Session::new(SessionTarget::native(), DiagnosticPolicy::default())
        .lower_program(&state.program, tco)
        .expect("local generic impl calls must complete shared semantic lowering");
    let module = &output.semantics().module;
    let index = module.function_index();
    let entry = index.function(module.entry_callable.unwrap()).unwrap();
    assert_eq!(
        module
            .callables
            .iter()
            .filter(|callable| callable.declaration == declaration)
            .count(),
        2,
        "one method declaration must have exactly its two demanded specializations"
    );
    for argument in [hew_types::ResolvedTy::I64, hew_types::ResolvedTy::String] {
        let key = hew_sir::SirInstanceKey {
            template: hew_sir::GenericTemplateId { declaration },
            type_args: vec![argument.clone()],
        };
        let callable = module
            .callable_for_instance(&key)
            .expect("the instance key must retain the checker declaration and concrete argument");
        assert_eq!(callable.signature.return_ty, argument);
        assert!(
            index.function(callable.id).is_some(),
            "the exact specialization must have a body"
        );
        let calls = entry.blocks.iter().filter(|block| matches!(
            block.terminator, hew_sir::SemTerminator::Call { callee, .. } if callee == callable.id
        )).count();
        assert_eq!(
            calls, 2,
            "repeated source calls must reuse the same semantic callable"
        );
    }
}

#[test]
fn nested_generic_free_calls_keep_exact_direct_symbols_across_all_module_origins() {
    // Every invocation sits in a closure body, which lowers through a child
    // MIR builder.  Exercise all body origins that may be the selected
    // generic declaration: root, file import, package import,
    // and two modules with the same final path component.  The same-leaf
    // pair makes a linker-name or leaf-name recovery observably unsound.
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-compile has a workspace parent");
    let dir = tempfile::tempdir().expect("create generic-free-call fixture dir");
    write_source(
        dir.path(),
        "file_helpers.hew",
        "pub fn file_first<T>(xs: [T]) -> T { xs[0] }\n",
    );
    write_source(
        dir.path(),
        "alpha.hew",
        "pub fn first<T>(xs: [T]) -> T { xs[0] }\n",
    );
    fs::create_dir_all(dir.path().join("beta")).expect("create same-leaf module directory");
    write_source(
        dir.path(),
        "beta/alpha.hew",
        "pub fn first<T>(xs: [T]) -> T { xs[0] }\n",
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        r#"
import "file_helpers.hew";
import hew.genhelpers;
import alpha as flat_alpha;
import beta.alpha as nested_alpha;

fn root_first<T>(xs: [T]) -> T { xs[0] }

fn main() {
let root = || root_first([1, 2]);
let file = || file_helpers.file_first([3, 4]);
let imported_pkg = || genhelpers.first([5, 6]);
let flat = || flat_alpha.first([7, 8]);
let nested = || nested_alpha.first([9, 10]);
println(root());
println(file());
println(imported_pkg());
println(flat());
println(nested());
}
"#,
    );
    let state = run_file_frontend_to_typecheck(
        &input,
        &FrontendOptions {
            pkg_path: Some(repo_root.join("tests/pkg-import/pkgs")),
            ..FrontendOptions::default()
        },
    )
    .expect("generic free-call fixture must type-check");
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
        "all generic free-call origins must lower cleanly: {:#?}",
        hir.diagnostics
    );
    let symbols = hew_hir::dispatch::build_direct_call_symbol_index(&hir.module.items);
    // The root unit's declarations carry its own module identity — the
    // entry file's stem — not a bare leaf; only the EMITTED symbol stays
    // bare. Pinning the identity here is what keeps a same-leaf pair from
    // sharing a body symbol.
    let expected = [
        ("main.root_first", "root_first"),
        // A file import is a module: its body symbol is qualified by it.
        ("file_helpers.file_first", "file_helpers$file_first"),
        ("hew.genhelpers.first", "hew$genhelpers$first"),
        ("alpha.first", "alpha$first"),
        ("beta.alpha.first", "beta$alpha$first"),
    ];
    let declared = |path: &str| tco.defs.lookup_path(path).expect("declared");
    for (path, symbol) in expected {
        assert_eq!(
            symbols.get(&declared(path)),
            Some(&symbol.to_string()),
            "generic declaration `{path}` must retain its exact emitted body symbol"
        );
    }
    assert_ne!(
        symbols.get(&declared("alpha.first")),
        symbols.get(&declared("beta.alpha.first")),
        "same-leaf generic functions must not share a direct-call symbol"
    );
}

#[test]
fn self_qualified_module_type_keeps_its_full_owner_through_typed_hir() {
    // The package fixture names Meter both bare and through its own
    // lexical leaf (`selfqualtype.Meter`) while its real owner is the
    // full module-graph path `hew.selfqualtype`. This checks every
    // handoff: checker signature, HIR declaration/parameter, and HIR field access must carry that same exact owner. A short-name fallback would
    // falsely pass the fixture only until a same-leaf package is present.
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-compile has a workspace parent");
    let input = repo_root.join("tests/pkg-import/self_qualified_type_identity.hew");
    let state = run_file_frontend_to_typecheck(
        input.to_str().expect("fixture path is utf-8"),
        &FrontendOptions {
            pkg_path: Some(repo_root.join("tests/pkg-import/pkgs")),
            ..FrontendOptions::default()
        },
    )
    .expect("self-qualified package fixture must type-check");
    let tco = state
        .typecheck_result
        .tco
        .as_ref()
        .expect("type checking was enabled");
    let expected = "hew.selfqualtype.Meter";
    assert!(
        matches!(
            tco.sigs()
                .test_entry("hew.selfqualtype.read")
                .expect("checker must retain imported read signature")
                .params
                .as_slice(),
            [hew_types::CallableParameter { ty: hew_types::Ty::Named { head, .. }, .. }] if head.spelling() == expected
        ),
        "checker parameter type must be the complete module owner: {:#?}",
        tco.sigs().test_entry("hew.selfqualtype.read")
    );

    let hir = hew_hir::lower_program(
        &state.program,
        tco,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "self-qualified package fixture must lower without HIR diagnostics: {:#?}",
        hir.diagnostics
    );
    let meter_decl = hir
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::HirItem::TypeDecl(decl)
                if decl.qualified_name(&hir.module.defs) == expected =>
            {
                Some(decl)
            }
            _ => None,
        })
        .expect("HIR must retain Meter under its full module owner");
    assert!(
        matches!(meter_decl.fields.as_slice(), [field] if field.name == "v" && field.ty == hew_types::ResolvedTy::I64),
        "HIR Meter field must retain its declared shape: {meter_decl:#?}"
    );
    let read = hir
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::HirItem::Function(function)
                if tco.defs.path(function.declaration) == "hew.selfqualtype.read" =>
            {
                Some(function)
            }
            _ => None,
        })
        .expect("HIR must emit the imported read body");
    assert!(
        matches!(read.params.as_slice(), [param] if param.name == "m" && matches!(&param.ty, hew_types::ResolvedTy::Named { head, .. } if head.registry_key() == expected)),
        "HIR read parameter must retain the full self-qualified owner: {read:#?}"
    );
}

#[test]
fn same_named_actor_replies_use_their_exact_import_owner_in_either_order() {
    // `replysend.Reply` has an i64 field; `replynonsend.Reply` carries
    // Rc<i64>. The actor method signatures spell both replies bare, so the
    // ask Send gate must translate the actor's lexical module binding to
    // the full source owner before marker lookup. Reversing imports proves
    // the result is not a last-writer-wins bare marker row.
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-compile has a workspace parent");
    let fixture_dir = repo_root.join("tests/pkg-import");
    let input = fixture_dir.join("samename_reply_reject.hew");
    let source = fs::read_to_string(&input).expect("read same-name reply fixture");
    let reversed = source.replacen(
        "import hew.replysend;\n\nimport hew.replynonsend;",
        "import hew.replynonsend;\n\nimport hew.replysend;",
        1,
    );
    assert_ne!(reversed, source, "fixture must contain both imports");
    let temp = tempfile::tempdir().expect("create reversed reply fixture dir");
    let reversed_input = write_source(temp.path(), "samename_reply_reject.hew", &reversed);

    for fixture in [input.to_string_lossy().into_owned(), reversed_input] {
        let failure = check_file(
            &fixture,
            &FrontendOptions {
                pkg_path: Some(fixture_dir.join("pkgs")),
                ..FrontendOptions::default()
            },
        )
        .expect_err("the Rc-backed reply must be rejected at the Send gate");
        let invalid_send: Vec<_> = failure
            .diagnostics
            .iter()
            .filter(|diagnostic| {
                matches!(
                    &diagnostic.kind,
                    FrontendDiagnosticKind::Type(error)
                        if error.kind == hew_types::error::TypeErrorKind::InvalidSend
                            && error.message.contains("E_DUPLEX_NON_SEND")
                )
            })
            .collect();
        assert_eq!(
            invalid_send.len(),
            1,
            "only the non-Send reply must fail regardless of import order: {:#?}",
            failure.diagnostics
        );
        assert!(
            failure
                .diagnostics
                .iter()
                .all(|diagnostic| !format!("{diagnostic:#?}").contains("D10 violation")),
            "the checker-owned Send gate must reject before any codegen D10 fallback: {:#?}",
            failure.diagnostics
        );
    }
}

#[test]
fn same_leaf_package_functions_publish_distinct_direct_body_symbols() {
    // `left::render` and `right::render` intentionally share the final
    // module component and the generic free-function leaves
    // `render_value`/`default_value`.  The checker-selected declaration
    // IDs must each project to an emitted HIR body; a linker-name lookup
    // or a partial impl-only projection drops these User calls before MIR.
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-compile has a workspace parent");
    let input = repo_root.join("tests/pkg-import/canonical_same_leaf_nested.hew");
    let state = run_file_frontend_to_typecheck(
        input.to_str().expect("fixture path is utf-8"),
        &FrontendOptions {
            pkg_path: Some(repo_root.join("tests/pkg-import/pkgs")),
            ..FrontendOptions::default()
        },
    )
    .expect("same-leaf fixture must type-check");
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
        "same-leaf fixture must lower without HIR diagnostics: {:#?}",
        hir.diagnostics
    );
    let symbols = hew_hir::dispatch::build_direct_call_symbol_index(&hir.module.items);
    let ids = [
        "left.render.render_value",
        "right.render.render_value",
        "left.render.default_value",
        "right.render.default_value",
    ]
    .map(|path| tco.defs.lookup_path(path).expect("declared"));
    let projected: Vec<_> = ids
        .iter()
        .map(|id| {
            symbols.get(id).cloned().unwrap_or_else(|| {
                panic!(
                    "checker declaration `{}` has no emitted HIR body symbol; \n                         render symbols: {:#?}",
                    tco.defs.path(*id),
                    symbols
                        .iter()
                        .filter(|(candidate, _)| tco.defs.path(**candidate).ends_with("render_value")
                            || tco.defs.path(**candidate).ends_with("default_value"))
                        .collect::<Vec<_>>()
                )
            })
        })
        .collect();
    assert_ne!(projected[0], projected[1]);
    assert_ne!(projected[2], projected[3]);

    // Each trait's `provided` body is materialised for a generic `Box<T>`
    // impl. Its trait-method ID is the static lookup key, but its concrete
    // default-body declaration must drive the monomorphisation and remain
    // distinct for these same-leaf packages.
    let lowered = hew_sir::lower_module(&hir.module, tco);
    assert!(
        hew_sir::verify_module(&lowered.module).is_empty(),
        "{:#?}",
        lowered.statuses
    );
    let defaults: Vec<_> = lowered
        .module
        .callables
        .iter()
        .filter(|callable| {
            tco.defs
                .path(callable.declaration)
                .contains("::<default impl ")
                && tco.defs.path(callable.declaration).ends_with("::provided")
        })
        .collect();
    assert_eq!(
        defaults.len(),
        2,
        "each same-leaf default must have a concrete SIR callable: {defaults:#?}"
    );
    for owner in ["left", "right"] {
        assert!(defaults.iter().any(|callable| tco.defs.path(callable.declaration) == format!("{owner}.render.Box::<default impl {owner}.render.Render for {owner}.render.Box<T>>::provided")));
    }
    assert_ne!(defaults[0].symbol, defaults[1].symbol);
    for callable in defaults {
        let hew_sir::CallableInstance::Generic(key) = &callable.instance else {
            panic!("default requires an exact generic instance");
        };
        let template = lowered
            .module
            .generic_templates
            .iter()
            .find(|template| template.id == key.template)
            .expect("selected generic template");
        assert_eq!(
            callable.symbol,
            hew_hir::monomorph::function_monomorph_symbol(&template.symbol, &key.type_args)
        );
    }
}

#[test]
fn imported_string_length_uses_the_shared_semantic_contract() {
    let dir = tempfile::tempdir().unwrap();
    write_source(
        dir.path(),
        "library.hew",
        "pub fn echo_len(value: string) -> i64 { value.len() }",
    );
    let input = write_source(
        dir.path(),
        "main.hew",
        "import library; fn main() -> i64 { library.echo_len(\"hello\") }",
    );
    let state = run_file_frontend_to_typecheck(&input, &FrontendOptions::default()).unwrap();
    let output = Session::new(SessionTarget::native(), DiagnosticPolicy::default())
        .lower_program(&state.program, state.typecheck_result.tco.as_ref().unwrap())
        .unwrap();
    let module = &output.semantics().module;
    let entry = module
        .function_index()
        .function(module.entry_callable.unwrap())
        .unwrap();
    let callee = entry
        .blocks
        .iter()
        .find_map(|block| match block.terminator {
            hew_sir::SemTerminator::Call { callee, .. } => Some(callee),
            _ => None,
        })
        .expect("entry must call the imported body");
    let body = module.function_index().function(callee).unwrap();
    assert_eq!(module.defs.path(body.declaration), "library.echo_len");
    assert!(
        body.blocks
            .iter()
            .any(|block| matches!(block.terminator, hew_sir::SemTerminator::RtCall { .. })),
        "the imported body must use the verified runtime-call contract"
    );
}

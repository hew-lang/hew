//! End-to-end integration tests for the Hew module system.
//!
//! These tests exercise the full parse → type-check pipeline, verifying that
//! the module graph, name resolution, visibility rules, and import aliasing
//! all behave correctly.

use hew_parser::ast::{
    ActorDecl, Block, Expr, FnDecl, ImportDecl, ImportName, ImportSpec, IntRadix, Item, Literal,
    Param, Program, ReceiveFnDecl, Spanned, TypeBodyItem, TypeDecl, TypeDeclKind, TypeExpr,
    Visibility,
};
use hew_parser::module::{Module, ModuleGraph, ModuleId, ModuleImport};
use hew_types::check::{SpanKey, TypeDefKind};
mod common;

use common::isolated_checker;
use hew_types::Ty;

// ── helpers ──────────────────────────────────────────────────────────────────

fn make_pub_fn(name: &str) -> FnDecl {
    FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        name: name.to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new((
                Expr::Literal(Literal::Integer {
                    value: 0,
                    radix: IntRadix::Decimal,
                }),
                0..1,
            ))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    }
}

fn make_user_import(
    path: &[&str],
    spec: Option<ImportSpec>,
    items: Vec<Spanned<Item>>,
) -> ImportDecl {
    ImportDecl {
        path: path.iter().map(std::string::ToString::to_string).collect(),
        spec,
        module_alias: None,
        file_path: None,
        resolved_items: Some(items),
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    }
}

fn module_node(id: &str, deps: &[&str]) -> Module {
    Module {
        id: ModuleId::new(vec![id.to_string()]),
        items: vec![],
        imports: deps
            .iter()
            .map(|d| ModuleImport {
                target: ModuleId::new(vec![d.to_string()]),
                spec: None,
                span: 0..0,
            })
            .collect(),
        source_paths: Vec::new(),
        doc: None,
    }
}

// ── module graph pipeline tests ───────────────────────────────────────────────

#[test]
fn test_module_graph_preserved_through_pipeline() {
    // Build a program with an attached module graph (two modules: root + lib)
    let root_id = ModuleId::new(vec!["root".to_string()]);
    let _lib_id = ModuleId::new(vec!["lib".to_string()]);

    let mut graph = ModuleGraph::new(root_id.clone());
    graph.add_module(module_node("root", &["lib"])).unwrap();
    graph.add_module(module_node("lib", &[])).unwrap();
    graph.compute_topo_order().expect("no cycles");

    // Verify graph topo order: lib before root
    let pos_lib = graph
        .topo_order
        .iter()
        .position(|id| id.path[0] == "lib")
        .unwrap();
    let pos_root = graph
        .topo_order
        .iter()
        .position(|id| id.path[0] == "root")
        .unwrap();
    assert!(
        pos_lib < pos_root,
        "lib must come before root in topo order"
    );

    // Attach graph to a program and run type checker — must not panic
    let program = Program {
        items: vec![],
        module_doc: None,
        module_graph: Some(graph),
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "program with module graph should typecheck without errors: {:?}",
        output.errors
    );
}

// ── qualified name resolution tests ──────────────────────────────────────────

#[test]
fn test_qualified_name_resolution() {
    // Bare `import utils;` → only qualified access `utils.helper` should be registered.
    let fn_helper = make_pub_fn("helper");
    let import = make_user_import(
        &["myapp", "utils"],
        None, // bare import — no glob, no named spec
        vec![(Item::Function(fn_helper), 0..0)],
    );

    let program = Program {
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        output.fn_sigs.contains_key("utils.helper"),
        "bare import should register qualified name 'utils.helper'"
    );
    assert!(
        !output.fn_sigs.contains_key("helper"),
        "bare import must NOT register unqualified 'helper'"
    );
}

#[test]
fn test_glob_import_resolution() {
    // `import utils::*;` → both qualified and unqualified should be accessible.
    let fn_helper = make_pub_fn("helper");
    let fn_other = make_pub_fn("other");
    let import = make_user_import(
        &["myapp", "utils"],
        Some(ImportSpec::Glob),
        vec![
            (Item::Function(fn_helper), 0..0),
            (Item::Function(fn_other), 0..0),
        ],
    );

    let program = Program {
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        output.fn_sigs.contains_key("utils.helper"),
        "glob import should register qualified 'utils.helper'"
    );
    assert!(
        output.fn_sigs.contains_key("helper"),
        "glob import should register unqualified 'helper'"
    );
    assert!(
        output.fn_sigs.contains_key("utils.other"),
        "glob import should register qualified 'utils.other'"
    );
    assert!(
        output.fn_sigs.contains_key("other"),
        "glob import should register unqualified 'other'"
    );
}

// ── named import (selective) ──────────────────────────────────────────────────

#[test]
fn test_named_import_selective_resolution() {
    // `import utils::{helper}` → only "helper" is unqualified, "other" is not.
    let fn_helper = make_pub_fn("helper");
    let fn_other = make_pub_fn("other");
    let import = make_user_import(
        &["myapp", "utils"],
        Some(ImportSpec::Names(vec![ImportName {
            name: "helper".to_string(),
            alias: None,
        }])),
        vec![
            (Item::Function(fn_helper), 0..0),
            (Item::Function(fn_other), 0..0),
        ],
    );

    let program = Program {
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        output.fn_sigs.contains_key("helper"),
        "named import of 'helper' should make it unqualified"
    );
    assert!(
        !output.fn_sigs.contains_key("other"),
        "non-imported 'other' must NOT be unqualified"
    );
    // Both should still be available qualified
    assert!(output.fn_sigs.contains_key("utils.helper"));
    assert!(output.fn_sigs.contains_key("utils.other"));
}

// ── generic bounds across module imports ─────────────────────────────────────

#[test]
fn test_imported_generic_fn_records_inferred_type_args_and_uses_imported_trait_impl() {
    let root_source = r#"
        import myapp::widgets::*;

        fn main() -> string {
            describe(Label { text: "hello" })
        }
    "#;
    let module_source = r"
        pub trait Describable {
            fn describe(val: Self) -> string;
        }

        pub type Label {
            text: string;
        }

        impl Describable for Label {
            fn describe(label: Label) -> string {
                label.text
            }
        }

        pub fn describe<T: Describable>(item: T) -> string {
            item.describe()
        }
    ";

    let mut root = hew_parser::parse(root_source);
    assert!(
        root.errors.is_empty(),
        "root parse errors: {:?}",
        root.errors
    );

    let call_span = root
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(fd) if fd.name == "main" => {
                fd.body.trailing_expr.as_ref().map(|expr| expr.1.clone())
            }
            _ => None,
        })
        .expect("main trailing call should exist");

    let module = hew_parser::parse(module_source);
    assert!(
        module.errors.is_empty(),
        "module parse errors: {:?}",
        module.errors
    );

    let import_decl = root
        .program
        .items
        .iter_mut()
        .find_map(|(item, _)| match item {
            Item::Import(import) => Some(import),
            _ => None,
        })
        .expect("root import should exist");
    import_decl.resolved_items = Some(module.program.items.clone());

    let mut checker = isolated_checker();
    let output = checker.check_program(&root.program);

    assert!(
        output.errors.is_empty(),
        "imported trait impl should satisfy imported generic bounds: {:?}",
        output.errors
    );
    assert!(
        output.fn_sigs.contains_key("widgets.describe"),
        "module-qualified imported generic should be registered"
    );
    assert!(
        output.fn_sigs.contains_key("describe"),
        "glob import should register imported generic unqualified"
    );

    let inferred = output
        .call_type_args
        .get(&SpanKey::from(&call_span))
        .expect("imported generic call should record inferred type args");
    // The glob import PUBLISHES `Label` bare, so a bare `Label` binds to its
    // owner's QUALIFIED identity (`widgets.Label`) — the per-module-type-identity
    // discipline that keeps a sibling module's same-bare-name `Label` from
    // colliding on one last-write-wins key downstream. The C1 layout authority
    // shortens the qualifier back to bare on the non-colliding layout lookup.
    assert_eq!(
        inferred,
        &vec![Ty::Named {
            builtin: None,
            name: "widgets.Label".to_string(),
            args: vec![],
        }]
    );
}

// ── pub visibility across modules ─────────────────────────────────────────────

#[test]
fn test_private_items_not_visible() {
    use hew_parser::ast::Block;

    let private_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private, // private
        name: "private_fn".to_string(),
        type_params: None,
        params: vec![],
        return_type: None,
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: None,
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    };
    let public_fn = make_pub_fn("public_fn");

    let import = make_user_import(
        &["mod_a"],
        Some(ImportSpec::Glob), // even glob should not expose private items
        vec![
            (Item::Function(private_fn), 0..0),
            (Item::Function(public_fn), 0..0),
        ],
    );

    let program = Program {
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        !output.fn_sigs.contains_key("private_fn"),
        "private fn must not appear unqualified (no bare binding)"
    );
    // Private functions ARE registered under their qualified name so the
    // reference-site enforcement check can emit E_VISIBILITY instead of a
    // generic "unknown function" error.
    assert!(
        output.fn_sigs.contains_key("mod_a.private_fn"),
        "private fn must be registered under its qualified name for enforcement"
    );
    assert!(
        output.fn_sigs.contains_key("public_fn"),
        "public fn should be accessible unqualified via glob"
    );
    assert!(output.fn_sigs.contains_key("mod_a.public_fn"));
}

// ── type visibility ───────────────────────────────────────────────────────────

#[test]
fn test_pub_type_accessible_qualified() {
    let pub_type = TypeDecl {
        visibility: Visibility::Pub,
        kind: TypeDeclKind::Struct,
        name: "Config".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![],
        doc_comment: None,
        wire: None,
        is_indirect: false,
        resource_marker: hew_parser::ast::ResourceMarker::None,
        is_opaque: false,
        consuming_methods: Vec::new(),
    };
    let import = make_user_import(
        &["myapp", "config"],
        None, // bare import
        vec![(Item::TypeDecl(pub_type), 0..0)],
    );

    let program = Program {
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        output.type_defs.contains_key("Config"),
        "pub type should be accessible as 'Config'"
    );
    assert!(
        output.type_defs.contains_key("config.Config"),
        "pub type should also be accessible as 'config.Config'"
    );
}

#[test]
fn test_pub_type_import_coexists_with_local_same_name() {
    // Per-module type namespacing (R313): a local `Config` and an imported
    // `config.Config` are distinct types keyed by their defining module, so
    // they coexist. The local def is reachable bare; the imported def is
    // reachable through its qualifier. This is NOT a duplicate definition —
    // the collision is only between two declarations *in the same module*.
    let local_type = TypeDecl {
        visibility: Visibility::Pub,
        kind: TypeDeclKind::Struct,
        name: "Config".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![],
        doc_comment: None,
        wire: None,
        is_indirect: false,
        resource_marker: hew_parser::ast::ResourceMarker::None,
        is_opaque: false,
        consuming_methods: Vec::new(),
    };
    let imported_type = TypeDecl {
        visibility: Visibility::Pub,
        kind: TypeDeclKind::Struct,
        name: "Config".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![],
        doc_comment: None,
        wire: None,
        is_indirect: false,
        resource_marker: hew_parser::ast::ResourceMarker::None,
        is_opaque: false,
        consuming_methods: Vec::new(),
    };
    let import = make_user_import(
        &["myapp", "config"],
        None,
        vec![(Item::TypeDecl(imported_type), 0..0)],
    );

    let program = Program {
        items: vec![
            (Item::TypeDecl(local_type), 0..0),
            (Item::Import(import), 0..0),
        ],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::DuplicateDefinition),
        "local and imported same-named types must coexist, got: {:?}",
        output.errors
    );
    assert!(
        output.type_defs.contains_key("Config"),
        "local type must be reachable bare as `Config`"
    );
    assert!(
        output.type_defs.contains_key("config.Config"),
        "imported type must be reachable qualified as `config.Config`"
    );
}

// ── per-module type namespacing (R313) ────────────────────────────────────────

fn pub_struct(name: &str) -> TypeDecl {
    TypeDecl {
        visibility: Visibility::Pub,
        kind: TypeDeclKind::Struct,
        name: name.to_string(),
        type_params: None,
        where_clause: None,
        body: vec![],
        doc_comment: None,
        wire: None,
        is_indirect: false,
        resource_marker: hew_parser::ast::ResourceMarker::None,
        is_opaque: false,
        consuming_methods: Vec::new(),
    }
}

#[test]
fn two_modules_export_same_type_name_coexist() {
    // VC4: importing two modules that each export a `Value` must not produce a
    // duplicate-definition error; both defs are reachable under their qualifier.
    let import_a = make_user_import(
        &["pkg", "alpha"],
        None,
        vec![(Item::TypeDecl(pub_struct("Value")), 0..0)],
    );
    let import_b = make_user_import(
        &["pkg", "beta"],
        None,
        vec![(Item::TypeDecl(pub_struct("Value")), 0..0)],
    );
    let program = Program {
        items: vec![
            (Item::Import(import_a), 0..0),
            (Item::Import(import_b), 0..0),
        ],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::DuplicateDefinition),
        "same-named types from different modules must coexist, got: {:?}",
        output.errors
    );
    assert!(
        output.type_defs.contains_key("alpha.Value"),
        "alpha.Value must be registered"
    );
    assert!(
        output.type_defs.contains_key("beta.Value"),
        "beta.Value must be registered"
    );
}

#[test]
fn same_module_duplicate_type_name_still_errors() {
    // VC3: the per-module scoping must NOT weaken same-module uniqueness. Two
    // `Value` declarations *in one module* are still a duplicate definition.
    let import = make_user_import(
        &["pkg", "alpha"],
        None,
        vec![
            (Item::TypeDecl(pub_struct("Value")), 0..0),
            (Item::TypeDecl(pub_struct("Value")), 10..20),
        ],
    );
    let program = Program {
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::DuplicateDefinition),
        "two `Value` decls in one module must still be a DuplicateDefinition, got: {:?}",
        output.errors
    );
}

#[test]
fn qualified_same_name_types_resolve_to_own_module_def() {
    // VC5: `alpha.Value` and `beta.Value` must each carry their own module's
    // export entry, guarding against first-wins mis-resolution.
    let import_a = make_user_import(
        &["pkg", "alpha"],
        None,
        vec![(Item::TypeDecl(pub_struct("Value")), 0..0)],
    );
    let import_b = make_user_import(
        &["pkg", "beta"],
        None,
        vec![(Item::TypeDecl(pub_struct("Value")), 0..0)],
    );
    let program = Program {
        items: vec![
            (Item::Import(import_a), 0..0),
            (Item::Import(import_b), 0..0),
        ],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    // Both qualified keys resolve to distinct, present type defs.
    assert!(
        output.type_defs.contains_key("alpha.Value") && output.type_defs.contains_key("beta.Value"),
        "both qualified defs must resolve, got keys: {:?}",
        output.type_defs.keys().collect::<Vec<_>>()
    );
}

#[test]
fn qualified_param_type_carries_module_into_resolved_sig() {
    // Slice 4 (downstream identity): a parameter typed `alpha.Value` must
    // resolve to `Ty::Named { name: "alpha.Value" }`, so the qualified name —
    // not the last-write-wins bare `Value` — is the identity carried into
    // HIR/MIR layout, mangle, and vtable keys. Two co-imported `Value`s would
    // otherwise collide under one bare key downstream.
    let import_a = make_user_import(
        &["pkg", "alpha"],
        None,
        vec![(Item::TypeDecl(pub_struct("Value")), 0..0)],
    );
    let import_b = make_user_import(
        &["pkg", "beta"],
        None,
        vec![(Item::TypeDecl(pub_struct("Value")), 0..0)],
    );
    let consumer = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        name: "take_alpha".to_string(),
        type_params: None,
        params: vec![Param {
            name: "v".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "alpha.Value".to_string(),
                    type_args: None,
                },
                0..0,
            ),
            is_mutable: false,
        }],
        return_type: None,
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: None,
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    };
    let program = Program {
        items: vec![
            (Item::Import(import_a), 0..0),
            (Item::Import(import_b), 0..0),
            (Item::Function(consumer), 0..0),
        ],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    let sig = output
        .fn_sigs
        .get("take_alpha")
        .expect("take_alpha sig must be registered");
    let param = sig.params.first().expect("take_alpha has one param");
    match param {
        Ty::Named { name, .. } => assert_eq!(
            name, "alpha.Value",
            "param type must carry the module qualifier into the resolved sig"
        ),
        other => panic!("expected Ty::Named, got {other:?}"),
    }
}

/// A `pub type` with a single scalar field of the named primitive type.
/// Used to build two same-bare-name types with divergent layouts.
fn pub_struct_with_scalar_field(name: &str, field: &str, scalar: &str) -> TypeDecl {
    TypeDecl {
        visibility: Visibility::Pub,
        kind: TypeDeclKind::Struct,
        name: name.to_string(),
        type_params: None,
        where_clause: None,
        body: vec![TypeBodyItem::Field {
            name: field.to_string(),
            ty: (
                TypeExpr::Named {
                    name: scalar.to_string(),
                    type_args: None,
                },
                0..0,
            ),
            attributes: vec![],
            doc_comment: None,
            span: 0..0,
        }],
        doc_comment: None,
        wire: None,
        is_indirect: false,
        resource_marker: hew_parser::ast::ResourceMarker::None,
        is_opaque: false,
        consuming_methods: Vec::new(),
    }
}

#[test]
fn same_bare_name_types_register_independent_divergent_field_layouts() {
    // C1 authority: two co-imported modules each export a `Widget` with a
    // DIVERGENT layout (`v: i8` vs `v: i64`). Their qualified `type_defs` keys
    // must hold INDEPENDENT field sets — each its own module's declared field
    // type — not two aliases of whichever bare `Widget` won the last-write-wins
    // race. The assertion is exact-value (`== I8` / `== I64`), the form with
    // teeth: an alias-of-one-winner bug makes both keys carry the same type and
    // fails here, where a `contains_key` check would pass on the collision.
    let import_narrow = make_user_import(
        &["pkg", "widgeti8"],
        None,
        vec![(
            Item::TypeDecl(pub_struct_with_scalar_field("Widget", "v", "i8")),
            0..0,
        )],
    );
    let import_wide = make_user_import(
        &["pkg", "widgeti64"],
        None,
        vec![(
            Item::TypeDecl(pub_struct_with_scalar_field("Widget", "v", "i64")),
            0..0,
        )],
    );
    let program = Program {
        items: vec![
            (Item::Import(import_narrow), 0..0),
            (Item::Import(import_wide), 0..0),
        ],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    let narrow = output
        .type_defs
        .get("widgeti8.Widget")
        .expect("widgeti8.Widget must register its own qualified def");
    let wide = output
        .type_defs
        .get("widgeti64.Widget")
        .expect("widgeti64.Widget must register its own qualified def");

    assert_eq!(
        narrow.fields.get("v"),
        Some(&Ty::I8),
        "widgeti8.Widget must keep its own i8 field, not the wide module's i64"
    );
    assert_eq!(
        wide.fields.get("v"),
        Some(&Ty::I64),
        "widgeti64.Widget must keep its own i64 field, not the narrow module's i8"
    );
}

#[test]
fn unqualified_ambiguous_type_is_typed_error() {
    // VC6: a bare reference to a type PUBLISHED bare by two imported modules
    // (each via an explicit `::{ Value }` opt-in) must be a typed AmbiguousType
    // error, not a silent first-wins pick. Ambiguity is decided over published
    // bare bindings — two plain imports publish nothing and are "not in scope"
    // instead (asserted separately below); two opt-ins are the genuine
    // ambiguity.
    let value_spec = || {
        Some(ImportSpec::Names(vec![ImportName {
            name: "Value".to_string(),
            alias: None,
        }]))
    };
    let import_a = make_user_import(
        &["pkg", "alpha"],
        value_spec(),
        vec![(Item::TypeDecl(pub_struct("Value")), 0..0)],
    );
    let import_b = make_user_import(
        &["pkg", "beta"],
        value_spec(),
        vec![(Item::TypeDecl(pub_struct("Value")), 0..0)],
    );
    // A function signature referencing bare `Value`.
    let consumer = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        name: "use_value".to_string(),
        type_params: None,
        params: vec![Param {
            name: "v".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "Value".to_string(),
                    type_args: None,
                },
                0..0,
            ),
            is_mutable: false,
        }],
        return_type: None,
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: None,
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    };
    let program = Program {
        items: vec![
            (Item::Import(import_a), 0..0),
            (Item::Import(import_b), 0..0),
            (Item::Function(consumer), 0..0),
        ],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::AmbiguousType),
        "bare `Value` with two exporters must be AmbiguousType, got: {:?}",
        output.errors
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::DuplicateDefinition),
        "ambiguity must not be reported as DuplicateDefinition, got: {:?}",
        output.errors
    );
}

#[test]
fn unqualified_unpublished_type_is_not_in_scope_not_ambiguous() {
    // Finding 3: two PLAIN imports each EXPORT `Value` but neither PUBLISHES it
    // bare, so a bare reference is "not in scope" under qualified-by-default —
    // NOT ambiguous. A plain import that did not publish the name must not
    // contribute to the ambiguity set (so it cannot poison an opt-in elsewhere).
    let import_a = make_user_import(
        &["pkg", "alpha"],
        None,
        vec![(Item::TypeDecl(pub_struct("Value")), 0..0)],
    );
    let import_b = make_user_import(
        &["pkg", "beta"],
        None,
        vec![(Item::TypeDecl(pub_struct("Value")), 0..0)],
    );
    let consumer = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        name: "use_value".to_string(),
        type_params: None,
        params: vec![Param {
            name: "v".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "Value".to_string(),
                    type_args: None,
                },
                0..0,
            ),
            is_mutable: false,
        }],
        return_type: None,
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: None,
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    };
    let program = Program {
        items: vec![
            (Item::Import(import_a), 0..0),
            (Item::Import(import_b), 0..0),
            (Item::Function(consumer), 0..0),
        ],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::UndefinedType),
        "bare `Value` with two plain (non-publishing) exporters must be UndefinedType \
         (not in scope), got: {:?}",
        output.errors
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::AmbiguousType),
        "plain imports publish nothing, so the bare ref must NOT be ambiguous, got: {:?}",
        output.errors
    );
}

// ── diamond dependency via module graph ───────────────────────────────────────

#[test]
fn test_diamond_dependency_topo_order() {
    // A imports B and C; B and C both import D.
    // Topo order must have D before B and C, both before A.
    let mut g = ModuleGraph::new(ModuleId::new(vec!["a".to_string()]));
    g.add_module(module_node("a", &["b", "c"])).unwrap();
    g.add_module(module_node("b", &["d"])).unwrap();
    g.add_module(module_node("c", &["d"])).unwrap();
    g.add_module(module_node("d", &[])).unwrap();
    g.compute_topo_order().expect("diamond has no cycles");

    let pos = |name: &str| {
        g.topo_order
            .iter()
            .position(|id| id.path[0] == name)
            .unwrap()
    };
    assert!(pos("d") < pos("b"), "d must precede b");
    assert!(pos("d") < pos("c"), "d must precede c");
    assert!(pos("b") < pos("a"), "b must precede a");
    assert!(pos("c") < pos("a"), "c must precede a");
}

// ── cycle detection ───────────────────────────────────────────────────────────

#[test]
fn test_cycle_detection() {
    // A imports B, B imports A → CycleError
    let mut g = ModuleGraph::new(ModuleId::new(vec!["a".to_string()]));
    g.add_module(module_node("a", &["b"])).unwrap();
    g.add_module(module_node("b", &["a"])).unwrap();
    let err = g
        .compute_topo_order()
        .expect_err("cycle should be detected");
    assert!(
        err.to_string().contains("import cycle detected"),
        "error message should mention cycle: {err}"
    );
}

// ── multi-module collision resistance ─────────────────────────────────────────

#[test]
fn test_two_modules_same_fn_no_collision() {
    // Two modules each expose `run()` — qualified names must differ.
    let fn_run_a = make_pub_fn("run");
    let fn_run_b = make_pub_fn("run");

    let import_a = make_user_import(
        &["pkg", "alpha"],
        None,
        vec![(Item::Function(fn_run_a), 0..0)],
    );
    let import_b = make_user_import(
        &["pkg", "beta"],
        None,
        vec![(Item::Function(fn_run_b), 0..0)],
    );

    let program = Program {
        items: vec![
            (Item::Import(import_a), 0..0),
            (Item::Import(import_b), 0..0),
        ],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(output.fn_sigs.contains_key("alpha.run"));
    assert!(output.fn_sigs.contains_key("beta.run"));
    assert!(
        output.errors.is_empty(),
        "no errors expected: {:?}",
        output.errors
    );
}

// ── actor import helpers ─────────────────────────────────────────────────────

/// Create a minimal actor declaration with optional receive functions.
fn make_actor(name: &str, receive_fns: Vec<ReceiveFnDecl>) -> ActorDecl {
    ActorDecl {
        visibility: Visibility::Pub,
        name: name.to_string(),
        type_params: vec![],
        super_traits: None,
        init: None,
        fields: vec![],
        receive_fns,
        methods: vec![],
        mailbox_capacity: None,
        overflow_policy: None,
        is_isolated: false,
        doc_comment: None,
        max_heap_bytes: None,
    }
}

/// Create a minimal receive fn declaration.
fn make_receive_fn(name: &str, params: &[(&str, &str)], ret: Option<&str>) -> ReceiveFnDecl {
    ReceiveFnDecl {
        is_generator: false,
        name: name.to_string(),
        type_params: None,
        params: params
            .iter()
            .map(|(pname, ptype)| Param {
                name: pname.to_string(),
                ty: (
                    TypeExpr::Named {
                        name: ptype.to_string(),
                        type_args: None,
                    },
                    0..0,
                ),
                is_mutable: false,
            })
            .collect(),
        return_type: ret.map(|r| {
            (
                TypeExpr::Named {
                    name: r.to_string(),
                    type_args: None,
                },
                0..0,
            )
        }),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: None,
        },
        span: 0..0,
        attributes: vec![],
        doc_comment: None,
    }
}

// ── actor in module tests ────────────────────────────────────────────────────

#[test]
fn test_actor_bare_import_registers_type_and_methods() {
    // `import mymod;` with an actor → should register qualified type + methods
    let recv_ping = make_receive_fn("ping", &[("msg", "string")], Some("string"));
    let actor = make_actor("MyActor", vec![recv_ping]);

    let import = make_user_import(
        &["app", "mymod"],
        None, // bare import
        vec![(Item::Actor(actor), 0..0)],
    );

    let program = Program {
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "actor import should not produce errors: {:?}",
        output.errors
    );

    // The dotted `{module}.{name}` key is the actor's primary identity; the
    // bare key is never written for module actors so a second same-named
    // import cannot clobber another module's actor.
    assert!(
        output.type_defs.contains_key("mymod.MyActor"),
        "qualified 'mymod.MyActor' should be registered"
    );
    assert!(
        !output.type_defs.contains_key("MyActor"),
        "bare 'MyActor' must not be registered for a module actor"
    );

    // Actor type should have the Actor kind under its qualified identity
    let def = output.type_defs.get("mymod.MyActor").unwrap();
    assert!(
        matches!(def.kind, TypeDefKind::Actor),
        "mymod.MyActor should be TypeDefKind::Actor, got {:?}",
        def.kind
    );

    // Receive fn should be registered under the dotted actor identity
    assert!(
        output.fn_sigs.contains_key("mymod.MyActor::ping"),
        "receive fn should be registered as 'mymod.MyActor::ping'"
    );
    assert!(
        !output.fn_sigs.contains_key("MyActor::ping"),
        "bare 'MyActor::ping' must not be registered for a module actor"
    );
}

#[test]
fn test_actor_glob_import_registers_unqualified() {
    // `import mymod::*;` → actor should be accessible unqualified
    let recv_greet = make_receive_fn("greet", &[("name", "string")], Some("string"));
    let actor = make_actor("Greeter", vec![recv_greet]);

    let import = make_user_import(
        &["app", "mymod"],
        Some(ImportSpec::Glob),
        vec![(Item::Actor(actor), 0..0)],
    );

    let program = Program {
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "glob actor import should not produce errors: {:?}",
        output.errors
    );

    // The dotted key is the actor's identity even under a glob import;
    // unqualified ACCESS resolves through the local-first bare-name
    // resolution at spawn/annotation sites, not a bare registry copy.
    assert!(output.type_defs.contains_key("mymod.Greeter"));
    assert!(!output.type_defs.contains_key("Greeter"));
    assert!(output.fn_sigs.contains_key("mymod.Greeter::greet"));
    assert!(!output.fn_sigs.contains_key("Greeter::greet"));
}

#[test]
fn test_actor_named_import_selective() {
    // `import mymod::{Counter};` → only Counter accessible unqualified
    let recv_inc = make_receive_fn("increment", &[], Some("i32"));
    let actor_counter = make_actor("Counter", vec![recv_inc]);
    let actor_timer = make_actor("Timer", vec![]);

    let import = make_user_import(
        &["app", "mymod"],
        Some(ImportSpec::Names(vec![ImportName {
            name: "Counter".to_string(),
            alias: None,
        }])),
        vec![
            (Item::Actor(actor_counter), 0..0),
            (Item::Actor(actor_timer), 0..0),
        ],
    );

    let program = Program {
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "named actor import should not produce errors: {:?}",
        output.errors
    );

    // Both actors register under their dotted identity only; the named
    // import binding ("Counter") resolves through `unqualified_to_module`
    // at reference sites rather than a bare registry copy.
    assert!(output.type_defs.contains_key("mymod.Counter"));
    assert!(!output.type_defs.contains_key("Counter"));
    assert!(output.fn_sigs.contains_key("mymod.Counter::increment"));
    assert!(!output.fn_sigs.contains_key("Counter::increment"));

    assert!(output.type_defs.contains_key("mymod.Timer"));
    assert!(!output.type_defs.contains_key("Timer"));
}

#[test]
fn test_actor_multiple_receive_fns() {
    // Actor with multiple receive fns — all should be registered
    let recv_get = make_receive_fn("get", &[("key", "string")], Some("string"));
    let recv_set = make_receive_fn("set", &[("key", "string"), ("val", "string")], None);
    let recv_del = make_receive_fn("delete", &[("key", "string")], Some("bool"));
    let actor = make_actor("Cache", vec![recv_get, recv_set, recv_del]);

    let import = make_user_import(
        &["app", "cache"],
        Some(ImportSpec::Glob),
        vec![(Item::Actor(actor), 0..0)],
    );

    let program = Program {
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "multi-receive actor import should not produce errors: {:?}",
        output.errors
    );

    assert!(output.fn_sigs.contains_key("cache.Cache::get"));
    assert!(output.fn_sigs.contains_key("cache.Cache::set"));
    assert!(output.fn_sigs.contains_key("cache.Cache::delete"));
}

#[test]
fn test_actor_and_function_coexist_in_module() {
    // Module with both actors and functions — both should register
    let recv_run = make_receive_fn("run", &[], None);
    let actor = make_actor("Worker", vec![recv_run]);
    let func = make_pub_fn("create_worker");

    let import = make_user_import(
        &["app", "workers"],
        Some(ImportSpec::Glob),
        vec![(Item::Actor(actor), 0..0), (Item::Function(func), 0..0)],
    );

    let program = Program {
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "mixed actor+function import should not produce errors: {:?}",
        output.errors
    );

    // Actor registered under its dotted identity
    assert!(output.type_defs.contains_key("workers.Worker"));
    assert!(output.fn_sigs.contains_key("workers.Worker::run"));

    // Function registered (functions keep their bare glob-import binding)
    assert!(output.fn_sigs.contains_key("create_worker"));
    assert!(output.fn_sigs.contains_key("workers.create_worker"));
}

#[test]
fn test_module_graph_same_fn_different_modules_no_collision() {
    // Two modules in the module graph each define `foo()`.
    // The scoped names ("alpha.foo", "beta.foo") should not collide.
    let fn_foo_a = make_pub_fn("foo");
    let fn_foo_b = make_pub_fn("foo");

    let alpha_id = ModuleId::new(vec!["alpha".to_string()]);

    let mut graph = ModuleGraph::new(alpha_id.clone());
    let mut alpha_mod = module_node("alpha", &[]);
    alpha_mod.items = vec![(Item::Function(fn_foo_a), 0..10)];
    let mut beta_mod = module_node("beta", &[]);
    beta_mod.items = vec![(Item::Function(fn_foo_b), 10..20)];

    graph.add_module(alpha_mod).unwrap();
    graph.add_module(beta_mod).unwrap();
    graph.compute_topo_order().expect("no cycles");

    let program = Program {
        items: vec![],
        module_doc: None,
        module_graph: Some(graph),
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "same fn name in different modules should not collide: {:?}",
        output.errors
    );
}

// ── Missing-module coverage ───────────────────────────────────────────────────

/// An import with no `resolved_items` and an empty registry (simulating the LSP
/// before search-path fix) must produce an `UnresolvedImport` error rather than
/// silently succeeding.
#[test]
fn test_unresolved_import_fail_closed() {
    use hew_types::error::TypeErrorKind;

    let import = ImportDecl {
        path: vec!["no_such_pkg".to_string(), "missing".to_string()],
        spec: None,
        module_alias: None,
        file_path: None,
        resolved_items: None,
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };

    let program = Program {
        items: vec![(Item::Import(import), 0..20)],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UnresolvedImport),
        "expected UnresolvedImport diagnostic, got errors: {errors:?}",
        errors = output.errors
    );
}

/// An import whose `resolved_items` is populated must NOT produce an
/// `UnresolvedImport` error — the user-module path should be taken.
#[test]
fn test_import_with_resolved_items_is_not_unresolved() {
    use hew_types::error::TypeErrorKind;

    let import = make_user_import(&["myapp", "utils"], None, vec![]);

    let program = Program {
        items: vec![(Item::Import(import), 0..20)],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UnresolvedImport),
        "unexpected UnresolvedImport diagnostic for a user module that has resolved_items"
    );
}

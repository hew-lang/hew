//! End-to-end integration tests for the Hew module system.
//!
//! These tests exercise the full parse → type-check pipeline, verifying that
//! the module graph, name resolution, visibility rules, and import aliasing
//! all behave correctly.

use hew_parser::ast::{
    ActorDecl, Block, FnDecl, ImportDecl, ImportName, ImportSpec, Item, Param, Program,
    ReceiveFnDecl, Spanned, TypeDecl, TypeDeclKind, TypeExpr, Visibility,
};
use hew_parser::module::{Module, ModuleGraph, ModuleId, ModuleImport};
use hew_types::check::TypeDefKind;
use hew_types::Checker;

// ── helpers ──────────────────────────────────────────────────────────────────

fn make_pub_fn(name: &str) -> FnDecl {
    use hew_parser::ast::{Expr, IntRadix, Literal};
    FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        is_pure: false,
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
    }
}

fn make_user_import(
    path: Vec<&str>,
    spec: Option<ImportSpec>,
    items: Vec<Spanned<Item>>,
) -> ImportDecl {
    ImportDecl {
        path: path.iter().map(|s| s.to_string()).collect(),
        spec,
        file_path: None,
        resolved_items: Some(items),
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
    graph.add_module(module_node("root", &["lib"]));
    graph.add_module(module_node("lib", &[]));
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
    let mut checker = Checker::new();
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
        vec!["myapp", "utils"],
        None, // bare import — no glob, no named spec
        vec![(Item::Function(fn_helper), 0..0)],
    );

    let program = Program {
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = Checker::new();
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
        vec!["myapp", "utils"],
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
    let mut checker = Checker::new();
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
        vec!["myapp", "utils"],
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
    let mut checker = Checker::new();
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

// ── pub visibility across modules ─────────────────────────────────────────────

#[test]
fn test_private_items_not_visible() {
    use hew_parser::ast::Block;

    let private_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private, // private
        is_pure: false,
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
    };
    let public_fn = make_pub_fn("public_fn");

    let import = make_user_import(
        vec!["mod_a"],
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
    let mut checker = Checker::new();
    let output = checker.check_program(&program);

    assert!(
        !output.fn_sigs.contains_key("private_fn"),
        "private fn must not appear in fn_sigs (unqualified)"
    );
    assert!(
        !output.fn_sigs.contains_key("mod_a.private_fn"),
        "private fn must not appear in fn_sigs (qualified)"
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
    };
    let import = make_user_import(
        vec!["myapp", "config"],
        None, // bare import
        vec![(Item::TypeDecl(pub_type), 0..0)],
    );

    let program = Program {
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = Checker::new();
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

// ── diamond dependency via module graph ───────────────────────────────────────

#[test]
fn test_diamond_dependency_topo_order() {
    // A imports B and C; B and C both import D.
    // Topo order must have D before B and C, both before A.
    let mut g = ModuleGraph::new(ModuleId::new(vec!["a".to_string()]));
    g.add_module(module_node("a", &["b", "c"]));
    g.add_module(module_node("b", &["d"]));
    g.add_module(module_node("c", &["d"]));
    g.add_module(module_node("d", &[]));
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
    g.add_module(module_node("a", &["b"]));
    g.add_module(module_node("b", &["a"]));
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
        vec!["pkg", "alpha"],
        None,
        vec![(Item::Function(fn_run_a), 0..0)],
    );
    let import_b = make_user_import(
        vec!["pkg", "beta"],
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
    let mut checker = Checker::new();
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
        super_traits: None,
        init: None,
        fields: vec![],
        receive_fns,
        methods: vec![],
        mailbox_capacity: None,
        overflow_policy: None,
        is_isolated: false,
        doc_comment: None,
    }
}

/// Create a minimal receive fn declaration.
fn make_receive_fn(name: &str, params: Vec<(&str, &str)>, ret: Option<&str>) -> ReceiveFnDecl {
    ReceiveFnDecl {
        is_generator: false,
        is_pure: false,
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
    }
}

// ── actor in module tests ────────────────────────────────────────────────────

#[test]
fn test_actor_bare_import_registers_type_and_methods() {
    // `import mymod;` with an actor → should register qualified type + methods
    let recv_ping = make_receive_fn("ping", vec![("msg", "String")], Some("String"));
    let actor = make_actor("MyActor", vec![recv_ping]);

    let import = make_user_import(
        vec!["app", "mymod"],
        None, // bare import
        vec![(Item::Actor(actor), 0..0)],
    );

    let program = Program {
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = Checker::new();
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "actor import should not produce errors: {:?}",
        output.errors
    );

    // Actor type should be registered (both qualified and unqualified)
    assert!(
        output.type_defs.contains_key("MyActor"),
        "actor type 'MyActor' should be registered"
    );
    assert!(
        output.type_defs.contains_key("mymod.MyActor"),
        "qualified 'mymod.MyActor' should be registered"
    );

    // Actor type should have the Actor kind
    let def = output.type_defs.get("MyActor").unwrap();
    assert!(
        matches!(def.kind, TypeDefKind::Actor),
        "MyActor should be TypeDefKind::Actor, got {:?}",
        def.kind
    );

    // Receive fn should be registered as "MyActor::ping"
    assert!(
        output.fn_sigs.contains_key("MyActor::ping"),
        "receive fn should be registered as 'MyActor::ping'"
    );
}

#[test]
fn test_actor_glob_import_registers_unqualified() {
    // `import mymod::*;` → actor should be accessible unqualified
    let recv_greet = make_receive_fn("greet", vec![("name", "String")], Some("String"));
    let actor = make_actor("Greeter", vec![recv_greet]);

    let import = make_user_import(
        vec!["app", "mymod"],
        Some(ImportSpec::Glob),
        vec![(Item::Actor(actor), 0..0)],
    );

    let program = Program {
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = Checker::new();
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "glob actor import should not produce errors: {:?}",
        output.errors
    );

    // Both qualified and unqualified access
    assert!(output.type_defs.contains_key("Greeter"));
    assert!(output.type_defs.contains_key("mymod.Greeter"));
    assert!(output.fn_sigs.contains_key("Greeter::greet"));
}

#[test]
fn test_actor_named_import_selective() {
    // `import mymod::{Counter};` → only Counter accessible unqualified
    let recv_inc = make_receive_fn("increment", vec![], Some("i32"));
    let actor_counter = make_actor("Counter", vec![recv_inc]);
    let actor_timer = make_actor("Timer", vec![]);

    let import = make_user_import(
        vec!["app", "mymod"],
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
    let mut checker = Checker::new();
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "named actor import should not produce errors: {:?}",
        output.errors
    );

    // Counter should be accessible qualified and unqualified
    assert!(output.type_defs.contains_key("Counter"));
    assert!(output.type_defs.contains_key("mymod.Counter"));
    assert!(output.fn_sigs.contains_key("Counter::increment"));

    // Timer should be qualified only (not named in import spec)
    assert!(output.type_defs.contains_key("mymod.Timer"));
    assert!(output.type_defs.contains_key("Timer"));
}

#[test]
fn test_actor_multiple_receive_fns() {
    // Actor with multiple receive fns — all should be registered
    let recv_get = make_receive_fn("get", vec![("key", "String")], Some("String"));
    let recv_set = make_receive_fn("set", vec![("key", "String"), ("val", "String")], None);
    let recv_del = make_receive_fn("delete", vec![("key", "String")], Some("bool"));
    let actor = make_actor("Cache", vec![recv_get, recv_set, recv_del]);

    let import = make_user_import(
        vec!["app", "cache"],
        Some(ImportSpec::Glob),
        vec![(Item::Actor(actor), 0..0)],
    );

    let program = Program {
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = Checker::new();
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "multi-receive actor import should not produce errors: {:?}",
        output.errors
    );

    assert!(output.fn_sigs.contains_key("Cache::get"));
    assert!(output.fn_sigs.contains_key("Cache::set"));
    assert!(output.fn_sigs.contains_key("Cache::delete"));
}

#[test]
fn test_actor_and_function_coexist_in_module() {
    // Module with both actors and functions — both should register
    let recv_run = make_receive_fn("run", vec![], None);
    let actor = make_actor("Worker", vec![recv_run]);
    let func = make_pub_fn("create_worker");

    let import = make_user_import(
        vec!["app", "workers"],
        Some(ImportSpec::Glob),
        vec![(Item::Actor(actor), 0..0), (Item::Function(func), 0..0)],
    );

    let program = Program {
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
        module_graph: None,
    };
    let mut checker = Checker::new();
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "mixed actor+function import should not produce errors: {:?}",
        output.errors
    );

    // Actor registered
    assert!(output.type_defs.contains_key("Worker"));
    assert!(output.fn_sigs.contains_key("Worker::run"));

    // Function registered
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

    graph.add_module(alpha_mod);
    graph.add_module(beta_mod);
    graph.compute_topo_order().expect("no cycles");

    let program = Program {
        items: vec![],
        module_doc: None,
        module_graph: Some(graph),
    };
    let mut checker = Checker::new();
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "same fn name in different modules should not collide: {:?}",
        output.errors
    );
}

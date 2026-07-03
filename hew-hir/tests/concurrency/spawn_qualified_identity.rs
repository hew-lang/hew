//! Tests for qualified actor identity through `lower_spawn`.
//!
//! A module-qualified spawn (`spawn bank.Account(...)`) lowers a
//! `HirExprKind::Spawn` whose `actor_name` is the checker-resolved dotted
//! identity (`bank.Account`), and the expression type is the dotted
//! `LocalPid<bank.Account>` — the same key the MIR actor-layout registry
//! uses. Root-actor spawns keep the bare name (zero churn).

use hew_hir::{
    lower_program_host_target, HirExpr, HirExprKind, HirItem, HirStmtKind, ResolutionCtx,
};
use hew_parser::ast::{ImportDecl, Item, Program, Spanned};
use hew_parser::module::{Module, ModuleGraph, ModuleId};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy, TypeCheckOutput};

const BANK_SRC: &str = "pub actor Account {\n\
                        \x20   var balance: i64 = 0;\n\
                        \x20   receive fn deposit(n: i64) -> i64 { balance = balance + n; balance }\n\
                        }\n";

/// Build a program whose root resolves `import hew::bank` (resolved items
/// attached) and whose module graph carries the `bank` module, mirroring the
/// shape `hew-compile` hands to the checker and HIR for package imports.
fn build_program(root_src: &str) -> Program {
    let imported = hew_parser::parse(BANK_SRC);
    assert!(
        imported.errors.is_empty(),
        "imported parse errors: {:?}",
        imported.errors
    );
    let root = hew_parser::parse(root_src);
    assert!(
        root.errors.is_empty(),
        "root parse errors: {:?}",
        root.errors
    );

    let import_item: Spanned<Item> = (
        Item::Import(ImportDecl {
            path: vec!["hew".to_string(), "bank".to_string()],
            spec: None,
            module_alias: None,
            file_path: None,
            resolved_items: Some(imported.program.items.clone()),
            resolved_item_source_paths: Vec::new(),
            resolved_source_paths: Vec::new(),
        }),
        0..0,
    );

    let bank_id = ModuleId::new(vec!["bank".to_string()]);
    let root_id = ModuleId::root();
    let bank_module = Module {
        id: bank_id.clone(),
        items: imported.program.items,
        imports: Vec::new(),
        source_paths: Vec::new(),
        doc: None,
    };
    let mut root_items = vec![import_item];
    root_items.extend(root.program.items.clone());
    let root_module = Module {
        id: root_id.clone(),
        items: root_items.clone(),
        imports: Vec::new(),
        source_paths: Vec::new(),
        doc: None,
    };

    let mut graph = ModuleGraph::new(root_id.clone());
    graph.add_module(bank_module).expect("add bank");
    graph.add_module(root_module).expect("add root");
    graph.topo_order = vec![bank_id, root_id];

    Program {
        items: root_items,
        module_graph: Some(graph),
        ..root.program
    }
}

fn lower_with_checker(program: &Program) -> (hew_hir::LowerOutput, TypeCheckOutput) {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(program);
    let output = lower_program_host_target(program, &tco, &ResolutionCtx);
    (output, tco)
}

/// Collect every `HirExprKind::Spawn` in `main`'s body as
/// `(actor_name, expr_ty)` pairs.
fn spawns_in_main(output: &hew_hir::LowerOutput) -> Vec<(String, ResolvedTy)> {
    let main = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(f) if f.name == "main" => Some(f),
            _ => None,
        })
        .expect("main fn must lower");
    let mut spawns = Vec::new();
    for stmt in &main.body.statements {
        let HirStmtKind::Let(_, Some(expr)) = &stmt.kind else {
            continue;
        };
        collect_spawns(expr, &mut spawns);
    }
    spawns
}

fn collect_spawns(expr: &HirExpr, spawns: &mut Vec<(String, ResolvedTy)>) {
    if let HirExprKind::Spawn { actor_name, .. } = &expr.kind {
        spawns.push((actor_name.clone(), expr.ty.clone()));
    }
}

fn local_pid_inner(ty: &ResolvedTy) -> Option<&str> {
    match ty {
        ResolvedTy::Named { name, args, .. } if name == "LocalPid" && args.len() == 1 => {
            match &args[0] {
                ResolvedTy::Named { name: inner, .. } => Some(inner.as_str()),
                _ => None,
            }
        }
        _ => None,
    }
}

/// `spawn bank.Account()` carries the dotted identity on both the lowered
/// `Spawn` node and its `LocalPid<T>` type.
#[test]
fn qualified_spawn_lowers_dotted_actor_name_and_pid_type() {
    let program = build_program(
        "fn main() {\n\
         \x20   let a = spawn bank.Account();\n\
         }\n",
    );
    let (output, tco) = lower_with_checker(&program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);

    let spawns = spawns_in_main(&output);
    assert_eq!(spawns.len(), 1, "expected one spawn, got {spawns:?}");
    let (actor_name, ty) = &spawns[0];
    assert_eq!(
        actor_name, "bank.Account",
        "lowered spawn must carry the dotted actor identity"
    );
    assert_eq!(
        local_pid_inner(ty),
        Some("bank.Account"),
        "spawn result type must be LocalPid<bank.Account>, got {ty:?}"
    );
}

/// A root actor spawn stays bare — no qualification churn for the
/// single-module common case.
#[test]
fn root_spawn_keeps_bare_actor_name_and_pid_type() {
    let program = build_program(
        "actor Local {\n\
         \x20   var total: i64 = 0;\n\
         \x20   receive fn poke() { total = total + 1; }\n\
         }\n\
         fn main() {\n\
         \x20   let l = spawn Local();\n\
         }\n",
    );
    let (output, tco) = lower_with_checker(&program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);

    let spawns = spawns_in_main(&output);
    assert_eq!(spawns.len(), 1, "expected one spawn, got {spawns:?}");
    let (actor_name, ty) = &spawns[0];
    assert_eq!(actor_name, "Local");
    assert_eq!(local_pid_inner(ty), Some("Local"));
}

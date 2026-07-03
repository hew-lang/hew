//! Local `HashMap` / `HashSet` scope-exit drop CODEGEN EMISSION tests.
//!
//! Companion to `hew-mir/tests/hashmap_hashset_local_drop.rs` (which pins the
//! drop-PLAN). These tests prove the emitted LLVM IR actually carries the free
//! CALL for a local, non-escaping collection handle, and — the no-double-free
//! half — that a handle moved into an actor's initial state is NOT freed inside
//! the spawning function (`main`); the actor's synthesised `__hew_state_drop_*`
//! is the sole owner of that free.
//!
//! Per `substrate-tests-the-substrate` the oracle is the substrate itself: we
//! count `call void @hew_hashmap_free_layout` / `@hew_hashset_free_layout`
//! occurrences inside a specific function body, not a shape-only "a symbol is
//! mentioned somewhere" check.
//!
//! LESSONS: cleanup-all-exits (P0), boundary-fail-closed (P0),
//! container-ingress-ownership-is-per-container (P0).

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;
use std::path::Path;

/// Full pipeline (parse → typecheck → HIR → MIR → emit) returning the textual
/// LLVM IR. Type-checking is required so the `HashMap` / `HashSet` builtins
/// resolve and the drop classifier sees the `builtin` discriminant.
fn emit_ll(source: &str, module_name: &str) -> String {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = hew_hir::lower_program(
        &parsed.program,
        &tc_output,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "hir diagnostics: {:#?}",
        output.diagnostics
    );
    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "mir diagnostics: {:#?}",
        pipeline.diagnostics
    );
    let out_dir = std::env::temp_dir().join(format!("hew-hashmap-drop-{module_name}"));
    std::fs::create_dir_all(&out_dir).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &out_dir,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts = emit_module(&pipeline, &options).expect("module must emit successfully");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

/// The body text of `define ... @<name>(...) { ... }`, from the `define` line up
/// to (and excluding) the terminating `}` line. Lets a test scope its call
/// count to ONE function instead of the whole module.
fn function_body<'a>(ll: &'a str, name: &str) -> &'a str {
    let needle = format!("@{name}(");
    let define_off = ll
        .lines()
        .scan(0usize, |off, line| {
            let start = *off;
            *off += line.len() + 1; // +1 for the '\n' stripped by lines()
            Some((start, line))
        })
        .find(|(_, line)| line.starts_with("define ") && line.contains(&needle))
        .map(|(start, _)| start)
        .unwrap_or_else(|| panic!("function @{name} must be defined in the emitted IR"));
    let rest = &ll[define_off..];
    // The first line that is exactly "}" closes the define.
    let end = rest.find("\n}").map_or(rest.len(), |i| i);
    &rest[..end]
}

fn count(haystack: &str, needle: &str) -> usize {
    haystack.matches(needle).count()
}

// ---------------------------------------------------------------------------
// Emission — a local, non-escaping handle emits its free CALL in `main`.
// ---------------------------------------------------------------------------

#[test]
fn hashmap_hashset_local_drop_emission_local_map_frees_in_main() {
    let ll = emit_ll(
        r"
        fn main() -> i64 {
            let m: HashMap<i64, i64> = HashMap::new();
            m.insert(1, 10);
            let _n = m.len();
            0
        }
        ",
        "local_map_free",
    );
    let main_body = function_body(&ll, "main");
    assert_eq!(
        count(main_body, "call void @hew_hashmap_free_layout("),
        1,
        "a local non-escaping HashMap must emit exactly one \
         hew_hashmap_free_layout call in main; main body:\n{main_body}"
    );
}

#[test]
fn hashmap_hashset_local_drop_emission_local_set_frees_in_main() {
    let ll = emit_ll(
        r"
        fn main() -> i64 {
            let s: HashSet<i64> = HashSet::new();
            s.insert(1);
            0
        }
        ",
        "local_set_free",
    );
    let main_body = function_body(&ll, "main");
    assert_eq!(
        count(main_body, "call void @hew_hashset_free_layout("),
        1,
        "a local non-escaping HashSet must emit exactly one \
         hew_hashset_free_layout call in main; main body:\n{main_body}"
    );
}

/// A map AND a set in the same scope each emit their free in main.
#[test]
fn hashmap_hashset_local_drop_emission_map_and_set_both_free_in_main() {
    let ll = emit_ll(
        r"
        fn main() -> i64 {
            let m: HashMap<i64, i64> = HashMap::new();
            m.insert(1, 10);
            let s: HashSet<i64> = HashSet::new();
            s.insert(2);
            0
        }
        ",
        "local_map_and_set_free",
    );
    let main_body = function_body(&ll, "main");
    assert_eq!(
        count(main_body, "call void @hew_hashmap_free_layout("),
        1,
        "map free missing in main; body:\n{main_body}"
    );
    assert_eq!(
        count(main_body, "call void @hew_hashset_free_layout("),
        1,
        "set free missing in main; body:\n{main_body}"
    );
}

// ---------------------------------------------------------------------------
// No double free — an escaped handle is NOT freed in the spawning function.
// ---------------------------------------------------------------------------

/// A map moved into an actor's initial state must NOT be freed inside `main`:
/// the actor's `__hew_state_drop_*` thunk is the sole owner of that free. A free
/// emitted in main too would double-free the layout storage (the W4.045 class).
/// The module overall still references the symbol (the state-drop thunk frees
/// it) — so this is a per-function-body count, not a module-wide one.
/// LESSONS: container-ingress-ownership-is-per-container, boundary-fail-closed.
#[test]
fn hashmap_hashset_local_drop_emission_spawn_escaped_map_not_freed_in_main() {
    let ll = emit_ll(
        r"
        actor Holder {
            let counts: HashMap<i64, i64>;
            receive fn ping() -> i64 { 1 }
        }
        fn main() -> i64 {
            let counts: HashMap<i64, i64> = HashMap::new();
            counts.insert(1, 10);
            let _h = spawn Holder(counts: counts);
            0
        }
        ",
        "spawn_escaped_map",
    );
    let main_body = function_body(&ll, "main");
    assert_eq!(
        count(main_body, "call void @hew_hashmap_free_layout("),
        0,
        "a map moved into actor state must NOT be freed in main — the actor's \
         __hew_state_drop thunk owns the free; a second free here double-frees \
         the layout storage. main body:\n{main_body}"
    );
    // Positive half: the free DOES exist in the module — in the actor's
    // state-drop thunk — so the storage is not leaked either.
    assert!(
        count(&ll, "call void @hew_hashmap_free_layout(") >= 1,
        "the spawned map's free must still exist in the actor's state-drop \
         thunk (not leaked); module:\n{ll}"
    );
}

/// `let counts2 = counts; spawn Holder(counts: counts2)` — the escape happens
/// through a whole-value alias. `main` must still emit ZERO `hew_hashmap_free_
/// layout` calls (neither the moved-from `counts` nor the escaping `counts2`),
/// while the actor's state-drop thunk keeps the single owning free. Exercises
/// the alias-root admission fix end-to-end: a final admission that tested the
/// candidate's own local rather than its alias root would re-admit `counts2`
/// here and emit a double free.
#[test]
fn hashmap_hashset_local_drop_emission_alias_then_spawn_escaped_map_not_freed_in_main() {
    let ll = emit_ll(
        r"
        actor Holder {
            let counts: HashMap<i64, i64>;
            receive fn ping() -> i64 { 1 }
        }
        fn main() -> i64 {
            let counts: HashMap<i64, i64> = HashMap::new();
            counts.insert(1, 10);
            let counts2 = counts;
            let _h = spawn Holder(counts: counts2);
            0
        }
        ",
        "alias_spawn_escaped_map",
    );
    let main_body = function_body(&ll, "main");
    assert_eq!(
        count(main_body, "call void @hew_hashmap_free_layout("),
        0,
        "an aliased map (`let counts2 = counts`) moved into actor state must \
         NOT be freed in main — admitting the alias member double-frees the \
         handle the actor owns. main body:\n{main_body}"
    );
    assert!(
        count(&ll, "call void @hew_hashmap_free_layout(") >= 1,
        "the spawned map's free must still exist in the actor's state-drop \
         thunk (not leaked); module:\n{ll}"
    );
}

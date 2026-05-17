//! Tests for HIR supervisor declaration lowering, specifically the slot-index
//! assignment pass introduced in Phase 2.0.c.
//!
//! Slot indices must match the runtime layout: static children index into
//! `HewSupervisor.children[]`, pool children index into `pool_slots[]`. The
//! two spaces are disjoint (both start at 0).

use hew_hir::{lower_program, HirItem, HirSupervisorChild, HirSupervisorDecl, ResolutionCtx};
use hew_types::TypeCheckOutput;

fn lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx)
}

fn find_supervisor<'a>(output: &'a hew_hir::LowerOutput, name: &str) -> &'a HirSupervisorDecl {
    output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Supervisor(s) if s.name == name => Some(s),
            _ => None,
        })
        .unwrap_or_else(|| panic!("expected supervisor `{name}` in lowered module"))
}

fn find_child<'a>(sup: &'a HirSupervisorDecl, name: &str) -> &'a HirSupervisorChild {
    sup.children
        .iter()
        .find(|c| c.name == name)
        .unwrap_or_else(|| panic!("expected child `{name}` in supervisor `{}`", sup.name))
}

/// Static-only supervisor: children get sequential indices starting at 0.
#[test]
fn static_children_get_sequential_slot_indices() {
    let output = lower(
        r"
        actor Cache { receive fn query() {} }
        actor Log   { receive fn write() {} }
        actor Db    { receive fn exec() {}  }

        supervisor App {
            child cache: Cache
            child log:   Log
            child db:    Db
        }
        ",
    );
    let sup = find_supervisor(&output, "App");
    let cache = find_child(sup, "cache");
    let log = find_child(sup, "log");
    let db = find_child(sup, "db");

    assert!(!cache.is_pool);
    assert_eq!(cache.slot_index, 0, "first static child is slot 0");
    assert!(!log.is_pool);
    assert_eq!(log.slot_index, 1, "second static child is slot 1");
    assert!(!db.is_pool);
    assert_eq!(db.slot_index, 2, "third static child is slot 2");
}

/// Pool-only supervisor: the pool child gets index 0 in the pool space.
#[test]
fn pool_child_gets_pool_slot_index_zero() {
    let output = lower(
        r"
        actor Worker { receive fn ping() {} }

        supervisor Pool {
            strategy: simple_one_for_one,
            pool worker: Worker
        }
        ",
    );
    let sup = find_supervisor(&output, "Pool");
    let worker = find_child(sup, "worker");

    assert!(worker.is_pool);
    assert_eq!(worker.slot_index, 0, "pool child slot index is 0");
}

/// Mixed supervisor: static and pool indices are disjoint -- both start at 0
/// in their own space.
#[test]
fn static_and_pool_indices_are_disjoint() {
    let output = lower(
        r"
        actor Cache  { receive fn query() {} }
        actor Worker { receive fn ping()  {} }

        supervisor App {
            child cache:  Cache
            pool  worker: Worker
        }
        ",
    );
    let sup = find_supervisor(&output, "App");
    let cache = find_child(sup, "cache");
    let worker = find_child(sup, "worker");

    assert!(!cache.is_pool);
    assert_eq!(cache.slot_index, 0, "static child starts at 0");

    assert!(worker.is_pool);
    assert_eq!(
        worker.slot_index, 0,
        "pool child also starts at 0 (disjoint space)"
    );
}

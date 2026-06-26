//! Tests for HIR supervisor declaration lowering, specifically the slot-index
//! assignment pass introduced in Phase 2.0.c.
//!
//! Slot indices must match the runtime layout: static children index into
//! `HewSupervisor.children[]`, pool children index into `pool_slots[]`. The
//! two spaces are disjoint (both start at 0).

use hew_hir::{HirItem, HirSupervisorChild, HirSupervisorDecl};

#[path = "support/mod.rs"]
mod support;

fn lower(source: &str) -> hew_hir::LowerOutput {
    support::checker_pipeline::lower_through_checker(source)
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
            pool worker: Worker(count: 2)
        }
        ",
    );
    let sup = find_supervisor(&output, "Pool");
    let worker = find_child(sup, "worker");

    assert!(worker.is_pool);
    assert_eq!(worker.slot_index, 0, "pool child slot index is 0");
}

/// A `pool worker: Worker(count: 3)` declaration splits the reserved `count`
/// arg into `pool_count` (the pool size) and leaves it OUT of `init_args` (the
/// per-member init template). The discriminator carries the count expr so the
/// checker and codegen can read it (verify-ast-carries-discriminator).
#[test]
fn pool_count_arg_is_extracted_into_pool_count() {
    let output = lower(
        r"
        actor Worker { receive fn ping() {} }

        supervisor Pool {
            strategy: simple_one_for_one,
            pool worker: Worker(count: 3)
        }
        ",
    );
    let sup = find_supervisor(&output, "Pool");
    let worker = find_child(sup, "worker");

    assert!(worker.is_pool);
    assert!(
        worker.pool_count.is_some(),
        "pool count expr must be carried in pool_count"
    );
    assert!(
        !worker.init_args.iter().any(|(name, _)| name == "count"),
        "the reserved `count` arg must NOT appear in the per-member init template"
    );
}

/// A pool with `count:` plus a per-member init arg keeps the init arg in
/// `init_args` while the count goes to `pool_count`.
#[test]
fn pool_count_and_member_init_arg_are_separated() {
    let output = lower(
        r"
        actor Worker {
            var id: i64;
            receive fn ping() {}
        }

        supervisor Pool {
            strategy: simple_one_for_one,
            pool worker: Worker(count: 4, id: 7)
        }
        ",
    );
    let sup = find_supervisor(&output, "Pool");
    let worker = find_child(sup, "worker");

    assert!(worker.pool_count.is_some(), "count → pool_count");
    assert_eq!(
        worker.init_args.len(),
        1,
        "only the per-member `id` arg remains in init_args"
    );
    assert_eq!(worker.init_args[0].0, "id");
}

/// A static child keeps a `count:` arg as an ordinary init field — `count` is
/// reserved only on POOL declarations (so `spawn Counter(count: 0)` stays a
/// state field).
#[test]
fn static_child_keeps_count_as_init_field() {
    let output = lower(
        r"
        actor Counter {
            var count: i64;
            receive fn tick() {}
        }

        supervisor App {
            child counter: Counter(count: 0)
        }
        ",
    );
    let sup = find_supervisor(&output, "App");
    let counter = find_child(sup, "counter");

    assert!(!counter.is_pool);
    assert!(
        counter.pool_count.is_none(),
        "static child has no pool_count"
    );
    assert_eq!(
        counter.init_args.len(),
        1,
        "the `count` arg stays a per-child init field on a static child"
    );
    assert_eq!(counter.init_args[0].0, "count");
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

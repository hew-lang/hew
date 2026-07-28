//! Leak oracle for `std::arena::Arena<T>` with a heap-owning composite value.
//!
//! Each cycle creates a fresh arena, inserts a `Holder` whose `Vec<string>`
//! owns dynamically allocated strings, removes it, and consumes the returned
//! value. A leaked COPY-IN source temporary in `Arena::insert` therefore grows
//! the leak-node count with the cycle count. The shared slope harness compares
//! low and high cycle counts under the poisoned allocator so the clean path
//! stays flat while that regression scales.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

fn arena_holder_insert_remove_source(cycles: usize) -> String {
    format!(
        "import std::arena::{{ Arena, Key }};\n\
         \n\
         type Holder {{ items: Vec<string> }}\n\
         \n\
         fn runCycle(i: i64) -> i64 {{\n\
         \x20   let store: Arena<Holder> = arena.new();\n\
         \x20   let key = store.insert(Holder {{\n\
         \x20       items: [f\"item-{{i}}\", f\"value-{{i}}\"],\n\
         \x20   }});\n\
         \x20   match store.remove(key) {{\n\
         \x20       Some(holder) => holder.items[0].len() + holder.items[1].len(),\n\
         \x20       None => -1000,\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{cycles} {{\n\
         \x20       total = total + runCycle(i);\n\
         \x20   }}\n\
         \x20   if total <= 0 {{ return 61; }}\n\
         \x20   0\n\
         }}\n"
    )
}

#[test]
fn stdlib_arena_holder_insert_remove_has_flat_leak_slope() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("arena-stdlib-holder-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &arena_holder_insert_remove_source(3),
        dir.path(),
        "arena_holder_scribble",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "Arena<Holder> insert/remove must release every owner exactly once:\n{}",
        describe_output(&output)
    );

    assert_frame_slope_below_tolerance(
        "arena_stdlib_holder_insert_remove",
        arena_holder_insert_remove_source,
    );
}

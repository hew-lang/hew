//! Invariant guard for the fail-closed caret attribution set (#2091).
//!
//! `HirModule::root_item_ids` is the positive authority the codegen fail-closed
//! renderer uses to decide whether a `^^^` caret may point at the user's root
//! source. It must contain EXACTLY the functions whose spans index that root
//! source. Items INJECTED at root module index 0 that actually index a library
//! file — the `std/builtins.hew` receiver impls (Display/duration/instant and
//! the Vec iterator harness) — must be EXCLUDED. Otherwise a codegen fail-closed
//! reachable in a builtin method would carry `SourceOrigin::RootUnit` and render
//! a false caret against unrelated root code.
//!
//! The builtin receiver impls lower at `current_module_idx == 0` (they are
//! injected out-of-band, not through `module_graph`), so the module-index gate
//! alone is insufficient; the `lowering_injected_items` flag excludes them. This
//! test lowers a program that uses Vec iteration (forcing the builtin Vec
//! iterator harness to be lowered) plus a genuine root free function and a root
//! impl method, then asserts:
//!   - builtin receiver impls are injected + tagged `std.builtins` (non-vacuous),
//!   - none of those builtin items leak into `root_item_ids` (the fix), and
//!   - the genuine root free fn AND root impl method ARE recorded (the fix does
//!     not over-suppress real root-source functions).
//!
//! Revert-repro: dropping the `lowering_injected_items` guard on the
//! `root_item_ids` inserts makes the builtin items leak, failing the middle
//! assertion.

mod support;

use hew_hir::HirItem;
use support::checker_pipeline::lower_through_checker;

const SOURCE: &str = r"
type Counter { n: i64 }

impl Counter {
    fn get(self) -> i64 {
        return self.n;
    }
}

fn sum(v: Vec<i64>) -> i64 {
    var total: i64 = 0;
    for x in v {
        total = total + x;
    }
    return total;
}

fn main() {
    let c = Counter { n: 5 };
    let v: Vec<i64> = Vec::new();
    println(sum(v) + c.get());
}
";

/// The positive root set must never contain an injected `std/builtins.hew`
/// receiver-impl item, or a builtin fail-closed would render a false root caret.
#[test]
fn root_item_ids_excludes_injected_builtin_impls() {
    let module = lower_through_checker(SOURCE).module;

    // Items tagged as originating in `std/builtins.hew`. Non-empty means the
    // builtin receiver impls were actually injected+lowered, so the exclusion
    // assertion below is not vacuous.
    let builtin_ids: Vec<_> = module
        .diagnostic_source_modules
        .iter()
        .filter(|(_, src)| src.as_str() == "std.builtins")
        .map(|(id, _)| *id)
        .collect();
    assert!(
        !builtin_ids.is_empty(),
        "expected std/builtins receiver impls to be injected and tagged \
         `std.builtins`; diagnostic_source_modules = {:?}",
        module.diagnostic_source_modules
    );

    // THE INVARIANT: no injected builtin item is recorded as root-origin.
    // Pre-fix, builtin receiver-impl methods lowered at module index 0 leaked
    // into `root_item_ids` and would resolve to `SourceOrigin::RootUnit`, so a
    // fail-closed in a builtin method would point a caret at the user's root
    // source.
    let leaked: Vec<_> = builtin_ids
        .iter()
        .filter(|id| module.root_item_ids.contains(id))
        .collect();
    assert!(
        leaked.is_empty(),
        "std/builtins items leaked into root_item_ids (would render a false \
         root caret on a builtin fail-closed): {leaked:?}"
    );

    // Positive control (free-fn path): a genuine root free function IS recorded,
    // proving the recording path still fires and the test does not pass
    // vacuously.
    let root_free_fn_recorded = module.items.iter().any(|item| {
        matches!(item, HirItem::Function(f)
            if f.name == "sum" && module.root_item_ids.contains(&f.id))
    });
    assert!(
        root_free_fn_recorded,
        "root free function `sum` must be recorded in root_item_ids"
    );

    // Positive control (impl-method path): the fix gates the exact insert that
    // recorded builtin methods, so confirm a genuine ROOT impl method is still
    // recorded. `Counter::get` is emitted as a qualified `HirItem::Function`.
    let root_impl_method_recorded = module.items.iter().any(|item| {
        matches!(item, HirItem::Function(f)
            if f.name == "Counter::get" && module.root_item_ids.contains(&f.id))
    });
    assert!(
        root_impl_method_recorded,
        "root impl method `Counter::get` must be recorded in root_item_ids; \
         the injected-items guard must not over-suppress genuine root methods"
    );
}

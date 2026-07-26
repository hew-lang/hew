//! `var` on a by-value parameter: which parameter types make a mutation
//! invisible to the caller, and which do not.
//!
//! Regression cover for #2810. The guard used to key on "not `Copy`-layout",
//! so `fn withdraw(var acc: Account, ..)` — a record of a single `i64` — was
//! admitted and silently debited a throwaway copy, while the identical shape
//! behind a type parameter was a hard error. The property that actually
//! matters is whether the parameter binding owns a private copy of the whole
//! aggregate, which is orthogonal to `Copy`.

use super::*;

const INEFFECTIVE: &str = "has no caller-visible effect";

fn ineffective_var_errors(source: &str) -> Vec<String> {
    let (errors, _) = parse_and_check(source);
    errors
        .iter()
        .filter(|e| e.message.contains(INEFFECTIVE))
        .map(|e| e.message.clone())
        .collect()
}

fn assert_ineffective(source: &str, expected: &str) {
    let found = ineffective_var_errors(source);
    assert!(
        found.iter().any(|m| m == expected),
        "expected `{expected}`, got: {found:?}"
    );
}

fn assert_no_ineffective_diagnostic(source: &str) {
    let found = ineffective_var_errors(source);
    assert!(
        found.is_empty(),
        "mutation through this parameter is caller-visible; \
         it must not be flagged, got: {found:?}"
    );
}

// ── Admitted: the parameter owns a private copy ───────────────────────────

/// The reproducer from #2810 verbatim. Before the fix this compiled clean,
/// `hew check` reported OK, and the withdrawal returned 60 while the caller's
/// account still held 100.
#[test]
fn issue_2810_reproducer_is_rejected() {
    assert_ineffective(
        concat!(
            "type Account { balance: i64; }\n",
            "fn withdraw(var acc: Account, amount: i64) -> i64 {\n",
            "    acc.balance = acc.balance - amount;\n",
            "    return acc.balance;\n",
            "}\n",
        ),
        "`var acc` on a by-value parameter of type `Account` has no caller-visible effect",
    );
}

#[test]
fn record_of_scalars_is_rejected() {
    assert_ineffective(
        concat!(
            "type Point { x: i64; y: i64; }\n",
            "fn shift(var p: Point) { p.x = p.x + 1; }\n",
        ),
        "`var p` on a by-value parameter of type `Point` has no caller-visible effect",
    );
}

/// The pre-existing case: a `string` field already made the record non-`Copy`,
/// so this was caught before the fix and must stay caught after it.
#[test]
fn record_with_owned_field_is_still_rejected() {
    assert_ineffective(
        concat!(
            "type Counter { count: i64; label: string; }\n",
            "fn bump(var c: Counter) -> Counter { c.count = c.count + 1; return c; }\n",
        ),
        "`var c` on a by-value parameter of type `Counter` has no caller-visible effect",
    );
}

#[test]
fn nested_record_of_scalars_is_rejected() {
    assert_ineffective(
        concat!(
            "type Inner { x: i64; }\n",
            "type Outer { inner: Inner; }\n",
            "fn shift(var o: Outer) { o.inner.x = 9; }\n",
        ),
        "`var o` on a by-value parameter of type `Outer` has no caller-visible effect",
    );
}

#[test]
fn enum_with_payload_is_rejected() {
    assert_ineffective(
        concat!(
            "enum Shape {\n",
            "    Circle(i64);\n",
            "    Square(i64);\n",
            "}\n",
            "fn resize(var s: Shape) { s = Shape::Square(9); }\n",
        ),
        "`var s` on a by-value parameter of type `Shape` has no caller-visible effect",
    );
}

#[test]
fn tuple_of_scalars_is_rejected() {
    assert_ineffective(
        "fn shift(var t: (i64, i64)) { t.0 = 9; }\n",
        "`var t` on a by-value parameter of type `(i64, i64)` has no caller-visible effect",
    );
}

#[test]
fn tuple_with_owned_element_is_still_rejected() {
    assert_ineffective(
        "fn shift(var t: (i64, string)) { t.0 = 9; }\n",
        "`var t` on a by-value parameter of type `(i64, string)` has no caller-visible effect",
    );
}

#[test]
fn fixed_array_of_scalars_is_rejected() {
    assert_ineffective(
        "fn shift(var a: [i64; 3]) { a[0] = 9; }\n",
        "`var a` on a by-value parameter of type `[i64; 3]` has no caller-visible effect",
    );
}

/// A generic aggregate instantiated at a concrete type is the same case as the
/// uninstantiated one: the parameter carries the aggregate's own storage
/// either way. Only the uninstantiated form used to be caught.
#[test]
fn generic_aggregate_at_a_concrete_type_is_rejected() {
    assert_ineffective(
        concat!(
            "type Pair<T> { a: T; b: T; }\n",
            "fn set(var p: Pair<i64>) { p.a = 9; }\n",
        ),
        "`var p` on a by-value parameter of type `Pair<i64>` has no caller-visible effect",
    );
}

#[test]
fn generic_aggregate_over_a_type_param_is_still_rejected() {
    assert_ineffective(
        concat!(
            "type Pair<T> { a: T; b: T; }\n",
            "fn set<T>(var p: Pair<T>, v: T) { p.a = v; }\n",
        ),
        "`var p` on a by-value parameter of type `Pair<T>` has no caller-visible effect",
    );
}

/// A `#[resource]` record is still a private copy at the call boundary — the
/// caller keeps a usable value of its own — so `var` on one is the same trap.
#[test]
fn resource_record_is_rejected() {
    assert_ineffective(
        concat!(
            "#[resource]\n",
            "type Conn { fd: i64; }\n",
            "impl Conn { fn close(c: Conn) { println(c.fd); } }\n",
            "fn retag(var c: Conn) { c.fd = 9; }\n",
        ),
        "`var c` on a by-value parameter of type `Conn` has no caller-visible effect",
    );
}

// ── Rejected: mutation through the parameter reaches the caller ───────────

/// `Vec` is a handle to storage the caller still references: `v[0] = 9`
/// through a by-value parameter is observed by the caller, and `var` is
/// required to write it. Flagging this would refuse correct code.
#[test]
fn vec_param_is_not_flagged() {
    assert_no_ineffective_diagnostic("fn set(var v: Vec<i64>) { v[0] = 9; }\n");
}

#[test]
fn hashmap_param_is_not_flagged() {
    assert_no_ineffective_diagnostic(
        "fn put(var m: HashMap<string, i64>) { m.insert(\"k\", 9); }\n",
    );
}

#[test]
fn hashset_param_is_not_flagged() {
    assert_no_ineffective_diagnostic("fn put(var s: HashSet<i64>) { s.insert(9); }\n");
}

#[test]
fn actor_handle_param_is_not_flagged() {
    assert_no_ineffective_diagnostic(concat!(
        "actor Probe {\n",
        "    var n: i64 = 0;\n",
        "    receive fn bump() { self.n = self.n + 1; }\n",
        "}\n",
        "fn poke(var p: LocalPid<Probe>) { p.bump(); }\n",
    ));
}

// ── Rejected: not aggregates this predicate classifies ────────────────────

#[test]
fn scalar_param_is_not_flagged() {
    assert_no_ineffective_diagnostic("fn add(var n: i64) -> i64 { n = n + 1; return n; }\n");
}

#[test]
fn string_param_is_not_flagged() {
    assert_no_ineffective_diagnostic(
        "fn shout(var s: string) -> string { s = s + \"!\"; return s; }\n",
    );
}

/// A mutable receiver writes back to the caller's binding, so it is exempt —
/// it is the sanctioned form the diagnostic's own help text points at.
#[test]
fn mutable_receiver_is_not_flagged() {
    assert_no_ineffective_diagnostic(concat!(
        "type Counter { count: i64; }\n",
        "impl Counter { fn bump(var self) -> i64 { self.count = self.count + 1; return self.count; } }\n",
    ));
}

/// An immutable by-value aggregate parameter is fine; only `var` is a trap.
#[test]
fn immutable_aggregate_param_is_not_flagged() {
    assert_no_ineffective_diagnostic(concat!(
        "type Account { balance: i64; }\n",
        "fn peek(acc: Account) -> i64 { return acc.balance; }\n",
    ));
}

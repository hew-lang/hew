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

fn assert_check_clean(source: &str) {
    let (errors, _) = parse_and_check(source);
    assert!(errors.is_empty(), "expected clean check, got: {errors:?}");
}

// ── Admitted: the parameter owns a private copy ───────────────────────────

/// The reproducer from #2810 verbatim. Before the fix this compiled clean,
/// `hew check` reported OK, and the withdrawal returned 60 while the caller's
/// account still held 100.
#[test]
fn issue_2810_reproducer_is_rejected() {
    assert_ineffective(
        concat!(
            "type Account { balance: i64, }\n",
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
            "type Point { x: i64, y: i64, }\n",
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
            "type Counter { count: i64, label: string, }\n",
            "fn bump(var c: Counter) -> Counter { c.count = c.count + 1; return c; }\n",
        ),
        "`var c` on a by-value parameter of type `Counter` has no caller-visible effect",
    );
}

#[test]
fn nested_record_of_scalars_is_rejected() {
    assert_ineffective(
        concat!(
            "type Inner { x: i64, }\n",
            "type Outer { inner: Inner, }\n",
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
            "    Circle(i64),\n",
            "    Square(i64),\n",
            "}\n",
            "fn resize(var s: Shape) { s = Shape.Square(9); }\n",
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
            "type Pair<T> { a: T, b: T, }\n",
            "fn set(var p: Pair<i64>) { p.a = 9; }\n",
        ),
        "`var p` on a by-value parameter of type `Pair<i64>` has no caller-visible effect",
    );
}

#[test]
fn generic_aggregate_over_a_type_param_is_still_rejected() {
    assert_ineffective(
        concat!(
            "type Pair<T> { a: T, b: T, }\n",
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
            "type Conn { fd: i64, }\n",
            "impl Conn { fn close(c: Conn) { println(c.fd); } }\n",
            "fn retag(var c: Conn) { c.fd = 9; }\n",
        ),
        "`var c` on a by-value parameter of type `Conn` has no caller-visible effect",
    );
}

/// #2821: builtin sum wrappers carry their payload inline. They are not
/// caller-visible handles merely because they have a `builtin` discriminator.
#[test]
fn issue_2821_option_of_value_aggregate_is_rejected() {
    assert_ineffective(
        concat!(
            "type Account { balance: i64, }\n",
            "fn withdraw(var acc: Option<Account>, amount: i64) -> i64 {\n",
            "    let current = acc.unwrap();\n",
            "    acc = Some(Account { balance: current.balance - amount });\n",
            "    return acc.unwrap().balance;\n",
            "}\n",
        ),
        "`var acc` on a by-value parameter of type `Option<Account>` has no caller-visible effect",
    );
}

#[test]
fn result_of_value_aggregate_is_rejected() {
    assert_ineffective(
        concat!(
            "type Account { balance: i64, }\n",
            "fn replace(var acc: Result<Account, string>) {\n",
            "    acc = Ok(Account { balance: 60 });\n",
            "}\n",
        ),
        "`var acc` on a by-value parameter of type `Result<Account, string>` has no caller-visible effect",
    );
}

#[test]
fn nested_option_result_value_aggregate_is_rejected() {
    assert_ineffective(
        concat!(
            "type Account { balance: i64, }\n",
            "fn replace(var acc: Option<Result<(Account, i64), string>>) {\n",
            "    acc = Some(Ok((Account { balance: 60 }, 1)));\n",
            "}\n",
        ),
        "`var acc` on a by-value parameter of type `Option<Result<(Account, i64), string>>` has no caller-visible effect",
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
        "    var n: i64 = 0,\n",
        "    receive fn bump() { self.n = self.n + 1; }\n",
        "}\n",
        "fn poke(var p: LocalPid<Probe>) { p.bump(); }\n",
    ));
}

#[test]
fn record_local_pid_field_projection_is_not_flagged() {
    assert_check_clean(concat!(
        "actor Probe {\n",
        "    var n: i64 = 0,\n",
        "    receive fn bump() { n = n + 1; }\n",
        "}\n",
        "type Holder { pid: LocalPid<Probe>, }\n",
        "fn poke(var holder: Holder) { holder.pid.bump(); }\n",
    ));
}

#[test]
fn record_sender_field_projection_is_not_flagged() {
    assert_check_clean(concat!(
        "type Holder { tx: channel.Sender<i64>, }\n",
        "fn send(var holder: Holder) { holder.tx.send(7); }\n",
    ));
}

#[test]
fn record_receiver_field_projection_is_not_flagged() {
    assert_check_clean(concat!(
        "type Holder { rx: channel.Receiver<i64>, }\n",
        "fn poll(var holder: Holder) { let _ = holder.rx.try_recv(); }\n",
    ));
}

#[test]
fn record_vec_field_index_projection_is_not_flagged() {
    assert_check_clean(concat!(
        "type Holder { items: Vec<i64>, }\n",
        "fn set_first(var holder: Holder) { holder.items[0] = 9; }\n",
    ));
}

#[test]
fn record_hashmap_field_mutation_is_not_flagged() {
    assert_check_clean(concat!(
        "type Holder { items: HashMap<string, i64>, }\n",
        "fn put(var holder: Holder) { holder.items.insert(\"k\", 9); }\n",
    ));
}

/// The root has a valid shared projection, so it cannot be rejected wholesale.
/// This concrete assignment never crosses that boundary and must still fail.
#[test]
fn record_handle_sibling_value_projection_is_rejected() {
    let (errors, _) = parse_and_check(concat!(
        "type Holder { items: Vec<i64>, count: i64, }\n",
        "fn retag(var holder: Holder) { holder.count = 9; }\n",
    ));
    assert!(
        errors.iter().any(|error| error.message
            == "`var holder` on a by-value parameter of type `Holder` has no caller-visible \
                effect"),
        "expected private-projection diagnostic, got: {errors:?}"
    );
}

/// Mutable receiver dispatch stores the returned receiver back into the
/// binding. On a non-receiver by-value parameter that binding is private, even
/// when another field happens to contain shared collection storage.
#[test]
fn record_handle_mutable_receiver_call_is_rejected_fail_closed() {
    let (errors, _) = parse_and_check(concat!(
        "trait Retag { fn retag(var self); }\n",
        "type Holder { items: Vec<i64>, count: i64, }\n",
        "impl Retag for Holder {\n",
        "    fn retag(var self) { self.count = 9; }\n",
        "}\n",
        "fn retag_param(var holder: Holder) { holder.retag(); }\n",
    ));
    assert!(
        errors.iter().any(|error| error.message
            == "`holder` is a by-value parameter; method `retag` writes back only to its private \
                copy, so the mutation is not proven caller-visible"),
        "expected mutable-receiver private-copy diagnostic, got: {errors:?}"
    );
}

/// A wrapper can contain a shared handle, but replacing the wrapper itself is
/// still a write to the callee's private wrapper storage.
#[test]
fn option_handle_root_replacement_is_rejected() {
    let (errors, _) =
        parse_and_check("fn replace(var items: Option<Vec<i64>>) { items = Some([1, 2]); }\n");
    assert!(
        errors.iter().any(|error| error.message
            == "`var items` on a by-value parameter of type `Option<Vec<i64>>` has no \
                caller-visible effect"),
        "expected private-wrapper diagnostic, got: {errors:?}"
    );
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

#[test]
fn bytes_param_root_replacement_is_not_flagged() {
    assert_no_ineffective_diagnostic(
        "fn replace(var data: bytes) -> bytes { data = \"x\".to_bytes(); return data; }\n",
    );
}

/// A mutable receiver writes back to the caller's binding, so it is exempt —
/// it is the sanctioned form the diagnostic's own help text points at.
#[test]
fn mutable_receiver_is_not_flagged() {
    assert_no_ineffective_diagnostic(concat!(
        "type Counter { count: i64, }\n",
        "impl Counter { fn bump(var self) -> i64 { self.count = self.count + 1; return self.count; } }\n",
    ));
}

/// An immutable by-value aggregate parameter is fine; only `var` is a trap.
#[test]
fn immutable_aggregate_param_is_not_flagged() {
    assert_no_ineffective_diagnostic(concat!(
        "type Account { balance: i64, }\n",
        "fn peek(acc: Account) -> i64 { return acc.balance; }\n",
    ));
}

// ── The help text that created the trap ───────────────────────────────────

const VAR_SUGGESTION: &str = "consider changing this to `var ";

fn mutability_suggestions(source: &str, name: &str) -> Vec<String> {
    let (errors, _) = parse_and_check(source);
    errors
        .iter()
        .find(|e| e.message == format!("cannot assign to immutable variable `{name}`"))
        .unwrap_or_else(|| panic!("expected a mutability error for `{name}`, got: {errors:?}"))
        .suggestions
        .clone()
}

/// The first half of #2810: following this help is how a user reached the
/// silent wrong answer. `var` must never be offered for a parameter whose
/// `var` form the compiler then rejects.
#[test]
fn value_param_assignment_does_not_suggest_var() {
    let suggestions = mutability_suggestions(
        concat!(
            "type Account { balance: i64, }\n",
            "fn withdraw(acc: Account, amount: i64) -> i64 {\n",
            "    acc.balance = acc.balance - amount;\n",
            "    return acc.balance;\n",
            "}\n",
        ),
        "acc",
    );
    assert!(
        !suggestions.iter().any(|s| s.contains(VAR_SUGGESTION)),
        "must not steer a by-value aggregate parameter into `var`, got: {suggestions:?}"
    );
    assert_eq!(
        suggestions,
        vec![
            "`acc` is a by-value parameter of type `Account`; mutating it has no caller-visible \
             effect"
                .to_string(),
            "return the modified value to the caller".to_string(),
            "move the mutation into an actor or a mutable receiver method".to_string(),
        ]
    );
}

#[test]
fn local_assignment_still_suggests_var() {
    let suggestions = mutability_suggestions("fn main() { let x = 1; x = 2; println(x); }\n", "x");
    assert_eq!(
        suggestions,
        vec!["consider changing this to `var x`".to_string()]
    );
}

/// A local of the very same record type keeps the `var` suggestion: `var p`
/// on a local is accepted, so offering it is correct there.
#[test]
fn local_of_aggregate_type_still_suggests_var() {
    let suggestions = mutability_suggestions(
        concat!(
            "type Account { balance: i64, }\n",
            "fn main() { let a = Account { balance: 1 }; a.balance = 2; println(a.balance); }\n",
        ),
        "a",
    );
    assert_eq!(
        suggestions,
        vec!["consider changing this to `var a`".to_string()]
    );
}

/// A `Vec` parameter genuinely needs `var` to write through it, and that write
/// is caller-visible — so this parameter keeps the `var` suggestion.
#[test]
fn handle_param_assignment_still_suggests_var() {
    let suggestions = mutability_suggestions("fn set(v: Vec<i64>) { v[0] = 9; }\n", "v");
    assert_eq!(
        suggestions,
        vec!["consider changing this to `var v`".to_string()]
    );
}

#[test]
fn option_value_param_assignment_does_not_suggest_var() {
    let suggestions = mutability_suggestions(
        concat!(
            "type Account { balance: i64, }\n",
            "fn replace(acc: Option<Account>) {\n",
            "    acc = Some(Account { balance: 60 });\n",
            "}\n",
        ),
        "acc",
    );
    assert!(
        !suggestions
            .iter()
            .any(|suggestion| suggestion.contains(VAR_SUGGESTION)),
        "must not steer an Option value parameter into `var`, got: {suggestions:?}"
    );
    assert_eq!(
        suggestions[0],
        "`acc` is a by-value parameter of type `Option<Account>`; mutating it has no \
         caller-visible effect"
    );
}

#[test]
fn record_handle_private_projection_does_not_suggest_var() {
    let suggestions = mutability_suggestions(
        concat!(
            "type Holder { items: Vec<i64>, count: i64, }\n",
            "fn retag(holder: Holder) { holder.count = 9; }\n",
        ),
        "holder",
    );
    assert!(
        !suggestions
            .iter()
            .any(|suggestion| suggestion.contains(VAR_SUGGESTION)),
        "must not steer a private field write into `var`, got: {suggestions:?}"
    );
}

#[test]
fn record_handle_shared_projection_still_suggests_var() {
    let suggestions = mutability_suggestions(
        concat!(
            "type Holder { items: Vec<i64>, }\n",
            "fn set_first(holder: Holder) { holder.items[0] = 9; }\n",
        ),
        "holder",
    );
    assert_eq!(
        suggestions,
        vec!["consider changing this to `var holder`".to_string()]
    );
}

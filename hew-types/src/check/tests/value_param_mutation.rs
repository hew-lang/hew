//! Mutable value parameters permit private local updates. Source checking
//! must not require those updates to change the caller's value.

use super::*;

fn assert_check_clean(source: &str) {
    let (errors, _) = parse_and_check(source);
    assert!(errors.is_empty(), "expected clean check, got: {errors:?}");
}

/// A withdrawal returns the updated balance from its private parameter.
#[test]
fn account_parameter_supports_private_mutation() {
    assert_check_clean(concat!(
        "type Account { balance: i64, }\n",
        "fn withdraw(var acc: Account, amount: i64) -> i64 {\n",
        "    acc.balance = acc.balance - amount;\n",
        "    return acc.balance;\n",
        "}\n",
    ));
}

#[test]
fn record_of_scalars_supports_private_mutation() {
    assert_check_clean(concat!(
        "type Point { x: i64, y: i64, }\n",
        "fn shift(var p: Point) { p.x = p.x + 1; }\n",
    ));
}

#[test]
fn record_with_owned_field_supports_private_mutation() {
    assert_check_clean(concat!(
        "type Counter { count: i64, label: string, }\n",
        "fn bump(var c: Counter) -> Counter { c.count = c.count + 1; return c; }\n",
    ));
}

#[test]
fn nested_record_of_scalars_supports_private_mutation() {
    assert_check_clean(concat!(
        "type Inner { x: i64, }\n",
        "type Outer { inner: Inner, }\n",
        "fn shift(var o: Outer) { o.inner.x = 9; }\n",
    ));
}

#[test]
fn enum_with_payload_supports_private_replacement() {
    assert_check_clean(concat!(
        "enum Shape {\n",
        "    Circle(i64),\n",
        "    Square(i64),\n",
        "}\n",
        "fn resize(var s: Shape) { s = Shape.Square(9); }\n",
    ));
}

#[test]
fn tuple_of_scalars_supports_private_mutation() {
    assert_check_clean("fn shift(var t: (i64, i64)) { t.0 = 9; }\n");
}

#[test]
fn tuple_with_owned_element_supports_private_mutation() {
    assert_check_clean("fn shift(var t: (i64, string)) { t.0 = 9; }\n");
}

#[test]
fn fixed_array_of_scalars_supports_private_mutation() {
    assert_check_clean("fn shift(var a: [i64; 3]) { a[0] = 9; }\n");
}

#[test]
fn generic_aggregate_at_a_concrete_type_supports_private_mutation() {
    assert_check_clean(concat!(
        "type Pair<T> { a: T, b: T, }\n",
        "fn set(var p: Pair<i64>) { p.a = 9; }\n",
    ));
}

#[test]
fn generic_aggregate_over_a_type_param_supports_private_mutation() {
    assert_check_clean(concat!(
        "type Pair<T> { a: T, b: T, }\n",
        "fn set<T>(var p: Pair<T>, v: T) { p.a = v; }\n",
    ));
}

/// Explicit ownership transfer permits mutation without an independent clone.
#[test]
fn consumed_resource_record_supports_mutation() {
    assert_check_clean(concat!(
        "#[resource]\n",
        "type Conn { fd: i64, }\n",
        "impl Conn { fn close(c: Conn) { println(c.fd); } }\n",
        "fn retag(consume var c: Conn) { c.fd = 9; }\n",
    ));
}

#[test]
fn borrowed_resource_mutation_requires_ownership() {
    for body in ["c.fd = 9;", "c.retag();"] {
        let (errors, _) = parse_and_check(&format!(
            "#[resource] type Conn {{ fd: i64 }}
             impl Conn {{ fn close(c: Conn) {{ println(c.fd); }} }}
             trait Retag {{ fn retag(var self); }}
             impl Retag for Conn {{ fn retag(var self) {{ self.fd = 9; }} }}
             fn retag(var c: Conn) {{ {body} }}"
        ));
        assert!(
            errors
                .iter()
                .any(|error| error.kind == TypeErrorKind::OwnMutateBorrowed),
            "{body}: {errors:?}"
        );
    }
    assert_check_clean(
        "#[resource] type Conn { fd: i64 }
         impl Conn { fn close(c: Conn) { println(c.fd); } }
         fn retag(var c: Conn) { c = Conn { fd: 1 }; c.fd = 9; }",
    );
}

#[test]
fn option_of_value_aggregate_supports_private_replacement() {
    assert_check_clean(concat!(
        "type Account { balance: i64, }\n",
        "fn withdraw(var acc: Option<Account>, amount: i64) -> i64 {\n",
        "    let current = acc.expect(\"the account is present\");\n",
        "    acc = Some(Account { balance: current.balance - amount });\n",
        "    return acc.expect(\"the account is present\").balance;\n",
        "}\n",
    ));
}

#[test]
fn result_of_value_aggregate_supports_private_replacement() {
    assert_check_clean(concat!(
        "type Account { balance: i64, }\n",
        "fn replace(var acc: Result<Account, string>) {\n",
        "    acc = Ok(Account { balance: 60 });\n",
        "}\n",
    ));
}

#[test]
fn nested_option_result_value_aggregate_supports_private_replacement() {
    assert_check_clean(concat!(
        "type Account { balance: i64, }\n",
        "fn replace(var acc: Option<Result<(Account, i64), string>>) {\n",
        "    acc = Some(Ok((Account { balance: 60 }, 1)));\n",
        "}\n",
    ));
}

#[test]
fn vec_param_is_not_flagged() {
    assert_check_clean("fn set(var v: Vec<i64>) { v[0] = 9; }\n");
}

#[test]
fn hashmap_param_is_not_flagged() {
    assert_check_clean("fn put(var m: HashMap<string, i64>) { m.insert(\"k\", 9); }\n");
}

#[test]
fn hashset_param_is_not_flagged() {
    assert_check_clean("fn put(var s: HashSet<i64>) { s.insert(9); }\n");
}

#[test]
fn actor_handle_param_is_not_flagged() {
    assert_check_clean(concat!(
        "actor Probe {\n",
        "    var n: i64 = 0,\n",
        "    receive fn bump() { self.n = self.n + 1; }\n",
        "}\n",
        "fn poke(var p: LocalPid<Probe>) { let _ = p.bump(); }\n",
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
        "fn poke(var holder: Holder) { let _ = holder.pid.bump(); }\n",
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
fn record_collection_field_supports_private_mutation() {
    for (ty, mutation) in [
        ("Vec<i64>", "push(9)"),
        ("HashMap<string, i64>", "insert(\"k\", 9)"),
        ("HashSet<i64>", "insert(9)"),
    ] {
        assert_check_clean(&format!(
            "type Holder {{ items: {ty} }} fn put(var holder: Holder) {{ holder.items.{mutation}; }}"
        ));
    }
}

#[test]
fn record_handle_sibling_value_supports_private_mutation() {
    assert_check_clean(concat!(
        "type Holder { items: Vec<i64>, count: i64, }\n",
        "fn retag(var holder: Holder) { holder.count = 9; }\n",
    ));
}

/// Mutable receiver dispatch stores the returned receiver back into the
/// binding, including a private mutable parameter binding.
#[test]
fn record_handle_mutable_receiver_call_supports_private_mutation() {
    assert_check_clean(concat!(
        "trait Retag { fn retag(var self); }\n",
        "type Holder { items: Vec<i64>, count: i64, }\n",
        "impl Retag for Holder {\n",
        "    fn retag(var self) { self.count = 9; }\n",
        "}\n",
        "fn retag_param(var holder: Holder) { holder.retag(); }\n",
    ));
}

/// A wrapper can contain a shared handle, but replacing the wrapper itself is
/// still a write to the callee's private wrapper storage.
#[test]
fn option_handle_root_supports_private_replacement() {
    assert_check_clean("fn replace(var items: Option<Vec<i64>>) { items = Some([1, 2]); }\n");
}

#[test]
fn scalar_param_is_not_flagged() {
    assert_check_clean("fn add(var n: i64) -> i64 { n = n + 1; return n; }\n");
}

#[test]
fn string_param_is_not_flagged() {
    assert_check_clean("fn shout(var s: string) -> string { s = s + \"!\"; return s; }\n");
}

#[test]
fn bytes_param_root_replacement_is_not_flagged() {
    assert_check_clean(
        "fn replace(var data: bytes) -> bytes { data = \"x\".to_bytes(); return data; }\n",
    );
}

#[test]
fn inherent_mutable_receiver_is_accepted() {
    // One receiver token carries every mode: `var self` mutates on an
    // inherent impl exactly as it does through a trait contract.
    assert_check_clean(concat!(
        "type Counter { count: i64, }\n",
        "impl Counter { fn bump(var self) -> i64 { self.count = self.count + 1; return self.count; } }\n",
    ));
}

#[test]
fn immutable_aggregate_param_is_not_flagged() {
    assert_check_clean(concat!(
        "type Account { balance: i64, }\n",
        "fn peek(acc: Account) -> i64 { return acc.balance; }\n",
    ));
}

fn mutability_suggestions(source: &str, name: &str) -> Vec<String> {
    let (errors, _) = parse_and_check(source);
    errors
        .iter()
        .find(|e| e.message == format!("cannot assign to immutable variable `{name}`"))
        .unwrap_or_else(|| panic!("expected a mutability error for `{name}`, got: {errors:?}"))
        .suggestions
        .clone()
}

#[test]
fn immutable_value_param_assignment_suggests_var() {
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
    assert_eq!(
        suggestions,
        vec!["consider changing this to `var acc`".to_string()]
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

#[test]
fn handle_param_assignment_still_suggests_var() {
    let suggestions = mutability_suggestions("fn set(v: Vec<i64>) { v[0] = 9; }\n", "v");
    assert_eq!(
        suggestions,
        vec!["consider changing this to `var v`".to_string()]
    );
}

#[test]
fn immutable_option_value_param_assignment_suggests_var() {
    let suggestions = mutability_suggestions(
        concat!(
            "type Account { balance: i64, }\n",
            "fn replace(acc: Option<Account>) {\n",
            "    acc = Some(Account { balance: 60 });\n",
            "}\n",
        ),
        "acc",
    );
    assert_eq!(
        suggestions,
        vec!["consider changing this to `var acc`".to_string()]
    );
}

#[test]
fn immutable_record_handle_private_projection_suggests_var() {
    let suggestions = mutability_suggestions(
        concat!(
            "type Holder { items: Vec<i64>, count: i64, }\n",
            "fn retag(holder: Holder) { holder.count = 9; }\n",
        ),
        "holder",
    );
    assert_eq!(
        suggestions,
        vec!["consider changing this to `var holder`".to_string()]
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

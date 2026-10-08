use crate::common;

use common::typecheck_isolated as typecheck;

#[test]
fn test_actor_init_type_mismatch_detected() {
    let output = typecheck(
        r"actor Worker {
    let count: i32;
    init() {
        let x: string = 123;
    }
}

fn main() {}
",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("type mismatch")),
        "init body type mismatch should be caught: {:?}",
        output.errors
    );
}

#[test]
fn test_actor_init_undefined_var_detected() {
    let output = typecheck(
        r"actor Worker {
    let id: i32;
    init() {
        nope = 1;
    }
}

fn main() {}
",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("undefined")),
        "init body undefined variable should be caught: {:?}",
        output.errors
    );
}

#[test]
fn test_actor_init_valid_field_access() {
    let output = typecheck(
        r"actor Worker {
    let id: i32;
    init() {
        println(id);
    }
}

fn main() {
    let _w = spawn Worker(id: 1);
}
",
    );
    assert!(
        output.errors.is_empty(),
        "valid init body should have no errors: {:?}",
        output.errors
    );
}

#[test]
fn test_actor_init_params_in_scope() {
    let output = typecheck(
        r"actor Greeter {
    let name: string;
    init(prefix: string) {
        println(prefix);
    }
}

fn main() {}
",
    );
    assert!(
        output.errors.is_empty(),
        "init params should be accessible: {:?}",
        output.errors
    );
}

#[test]
fn test_actor_no_init_still_works() {
    let output = typecheck(
        r"actor Counter {
    var count: i32;
    receive fn inc() {
        count = count + 1;
    }
}

fn main() {
    let _c = spawn Counter(count: 0);
}
",
    );
    assert!(
        output.errors.is_empty(),
        "actor without init should still work: {:?}",
        output.errors
    );
}

#[test]
fn test_actor_method_valid_field_access() {
    let output = typecheck(
        r"actor Counter {
    let count: i32;

    fn current() -> i32 {
        count
    }
}

fn main() {}
",
    );
    assert!(
        output.errors.is_empty(),
        "actor method should be able to read bare field names: {:?}",
        output.errors
    );
}

#[test]
fn test_actor_receive_self_field_reads_state() {
    let output = typecheck(
        r"actor Counter {
    let count: i32;

    receive fn current() -> i32 {
        self.count
    }
}

fn main() {}
",
    );
    assert!(
        output.errors.is_empty(),
        "`self.count` should read the actor's own state: {:?}",
        output.errors
    );
}

#[test]
fn test_actor_self_unknown_field_reports_against_state() {
    let output = typecheck(
        r"actor Counter {
    let count: i32;

    receive fn current() -> i32 {
        self.counts
    }
}

fn main() {}
",
    );
    let error = output
        .errors
        .iter()
        .find(|e| e.message.contains("`counts`"))
        .expect("expected an error naming the missing field");
    assert!(
        error.message.contains("actor state has no field"),
        "a missing field should be reported against the actor's state: {:?}",
        output.errors
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("not a valid identifier")),
        "the receiver must not also be reported as an undefined name: {:?}",
        output.errors
    );
}

#[test]
fn test_actor_bare_self_is_the_actor_handle() {
    let output = typecheck(
        r"actor Counter {
    let count: i32;

    receive fn current() -> i32 {
        let held = self;
        count
    }
}

fn main() {}
",
    );
    assert!(
        output
            .errors
            .iter()
            .all(|error| !error.message.contains("`self`")),
        "bare `self` names the actor handle, not an error: {:?}",
        output.errors
    );
}

#[test]
fn test_actor_init_and_method_self_field_reads_state() {
    // The `terminate { }` block surface was retired; the equivalent
    // `#[on(stop)]` fn is exercised by the dedicated lifecycle-hook
    // fixtures (see `hew-types/tests/actor_lifecycle_hooks.rs`).
    //
    // The receiver reaches state from every body that binds the fields, not
    // just from `receive fn`: init and plain actor methods included.
    let output = typecheck(
        r"actor Counter {
    var count: i32;

    init() {
        self.count = 1;
    }

    fn current() -> i32 {
        self.count
    }
}

fn main() {}
",
    );
    assert!(
        output.errors.is_empty(),
        "`self.count` should reach state from init and actor methods: {:?}",
        output.errors
    );
}

#[test]
fn test_actor_self_field_write_obeys_field_mutability() {
    let output = typecheck(
        r"actor Counter {
    let count: i32;

    receive fn bump() {
        self.count = 1;
    }
}

fn main() {}
",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("immutable field `count`")),
        "a write through the receiver should hit the same mutability rule as \
         a bare write: {:?}",
        output.errors
    );
}

#[test]
fn test_actor_self_nested_write_obeys_field_mutability() {
    // The mutability rule is keyed on the binding the target is rooted in, and
    // the receiver is not a binding. A target that only reaches the receiver
    // below an index or a further projection must still root in the state
    // field, or the write passes on a `let` field.
    let output = typecheck(
        r"actor Bag {
    let items: Vec<i64>;

    receive fn poke() {
        self.items[0] = 5;
    }
}

fn main() {}
",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("immutable field `items`")),
        "an indexed write through the receiver should reject like the bare \
         spelling: {:?}",
        output.errors
    );
}

#[test]
fn test_actor_on_stop_hook_valid_field_access() {
    let output = typecheck(
        r"actor Worker {
    let id: i32;

    #[on(stop)]
    fn flush() {
        println(id);
    }
}

fn main() {}
",
    );
    assert!(
        output.errors.is_empty(),
        "`#[on(stop)]` hook should be able to read bare field names: {:?}",
        output.errors
    );
}

// The `.ask()` method form — `actor_ref.ask(msg)` — is not a recognised
// receive fn name; actors are called directly by their `receive fn` name.
// This test pins that such a call fails at type-check (it parses, because
// method-call syntax is general, but no receive fn named `ask` exists on the
// actor type so the checker rejects it with an undefined-method error).
#[test]
fn ask_method_form_rejected_by_typechecker() {
    let output = typecheck(
        r"actor Counter {
    var count: i32 = 0;
    receive fn get() -> i32 {
        count
    }
}

fn main() {
    let c = spawn Counter();
    let _ = c.ask(1);
}
",
    );
    assert!(
        !output.errors.is_empty(),
        "`.ask()` on an actor ref should fail type-checking — actors are \
         called directly by their `receive fn` name, not via an `.ask()` method"
    );
}

// D458: an `init` parameter may not share a state field's name, so a spawn
// key always names exactly one of them.
#[test]
fn init_parameter_named_like_a_state_field_is_refused() {
    let output = typecheck(
        r"actor Widget {
    var n: string;
    init(n: i32) {
        let _ = n;
    }
    receive fn peek() -> string {
        n
    }
}

fn main() {
    let _w = spawn Widget(n: 7);
}
",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("variable `n` shadows a binding")),
        "an init parameter named like a field must be refused: {:?}",
        output.errors
    );
}

/// Every error of `output` as `(kind, message, start..end)` for exact asserts.
fn spawn_errors(output: &hew_types::TypeCheckOutput) -> Vec<(&'static str, String, usize, usize)> {
    output
        .errors
        .iter()
        .map(|e| {
            (
                e.kind.as_kind_str(),
                e.message.clone(),
                e.span.start,
                e.span.end,
            )
        })
        .collect()
}

const PAIR: &str = "actor Pair {
    let a: i64;
    let b: i64;
    receive fn show() -> string { f\"a={a} b={b}\" }
}
";

#[test]
fn spawn_key_unknown_to_the_actor_is_reported_at_the_key() {
    let source = format!("{PAIR}\nfn main() {{\n    let _p = spawn Pair(a: 1, b: 2, c: 3);\n}}\n");
    let output = typecheck(&source);
    let key = source.find("c: 3").expect("fixture key");
    assert_eq!(
        spawn_errors(&output),
        vec![(
            "E_SPAWN_ARG_UNKNOWN",
            "actor `Pair` has no field `c`".to_string(),
            key,
            key + 1
        )],
    );
}

#[test]
fn misspelt_spawn_shorthand_reports_the_unknown_and_the_missing_keys() {
    let source = format!(
        "{PAIR}\nfn main() {{\n    let x = 1;\n    let y = 2;\n    let _p = spawn Pair(x, y);\n}}\n"
    );
    let output = typecheck(&source);
    let messages: Vec<_> = spawn_errors(&output)
        .into_iter()
        .map(|(kind, message, _, _)| (kind, message))
        .collect();
    assert_eq!(
        messages,
        vec![
            (
                "E_SPAWN_ARG_UNKNOWN",
                "actor `Pair` has no field `x`".to_string()
            ),
            (
                "E_SPAWN_ARG_UNKNOWN",
                "actor `Pair` has no field `y`".to_string()
            ),
            (
                "MissingActorSpawnArgument",
                "missing field `a` in spawn of actor `Pair`".to_string()
            ),
            (
                "MissingActorSpawnArgument",
                "missing field `b` in spawn of actor `Pair`".to_string()
            ),
        ],
    );
}

#[test]
fn spawn_key_named_twice_is_reported_at_the_second_key() {
    let source = format!("{PAIR}\nfn main() {{\n    let _p = spawn Pair(a: 1, a: 2, b: 3);\n}}\n");
    let output = typecheck(&source);
    let second = source.find("a: 2").expect("fixture key");
    assert_eq!(
        spawn_errors(&output),
        vec![(
            "E_SPAWN_ARG_DUPLICATE",
            "spawn of `Pair` names `a` more than once".to_string(),
            second,
            second + 1
        )],
    );
}

#[test]
fn spawn_key_suggests_the_init_parameter_it_misspells() {
    let output = typecheck(
        r"actor Loader {
    var items: Vec<i64>;
    init(seed: i64) {
        items = [seed];
    }
    receive fn count() -> i64 {
        items.len()
    }
}

fn main() {
    let _l = spawn Loader(sed: 1);
}
",
    );
    let unknown = output
        .errors
        .iter()
        .find(|e| e.kind.as_kind_str() == "E_SPAWN_ARG_UNKNOWN")
        .unwrap_or_else(|| panic!("expected an unknown key: {:?}", output.errors));
    assert_eq!(
        unknown.message,
        "actor `Loader` has no field or `init` parameter `sed`"
    );
    assert_eq!(unknown.suggestions, vec!["seed".to_string()]);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message == "missing `init` parameter `seed` in spawn of actor `Loader`"),
        "the required init parameter is still missing: {:?}",
        output.errors
    );
}

#[test]
fn supervisor_spawn_keys_are_its_parameters() {
    let source = format!(
        "{PAIR}
supervisor Solo {{
    strategy: one_for_one;
    child p: Pair(a: 1, b: 2);
}}

supervisor Pool(a: i64, b: i64) {{
    strategy: one_for_one;
    child p: Pair(a: b, b: a);
}}

fn main() {{
    let _s = spawn Solo(x: 1);
    let _m = spawn Pool(a: 1);
    let _ok = spawn Pool(b: 2, a: 1);
}}
"
    );
    let output = typecheck(&source);
    let messages: Vec<_> = spawn_errors(&output)
        .into_iter()
        .map(|(kind, message, _, _)| (kind, message))
        .collect();
    assert_eq!(
        messages,
        vec![
            (
                "E_SPAWN_ARG_UNKNOWN",
                "supervisor `Solo` has no parameter `x`".to_string()
            ),
            (
                "MissingActorSpawnArgument",
                "missing parameter `b` in spawn of supervisor `Pool`".to_string()
            ),
        ],
    );
}

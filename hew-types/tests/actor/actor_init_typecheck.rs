use crate::common;

use common::typecheck_isolated as typecheck;

#[test]
fn test_actor_init_type_mismatch_detected() {
    let output = typecheck(
        r"
        actor Worker {
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
        r"
        actor Worker {
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
        r"
        actor Worker {
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
        r"
        actor Greeter {
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
        r"
        actor Counter {
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
        r"
        actor Counter {
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
fn test_actor_receive_self_uses_actor_guidance() {
    let output = typecheck(
        r"
        actor Counter {
            let count: i32;

            receive fn current() -> i32 {
                self.count
            }
        }

        fn main() {}
    ",
    );
    let self_error = output
        .errors
        .iter()
        .find(|e| e.message.contains("`self`"))
        .expect("expected an error mentioning `self`");
    assert!(
        self_error.message.contains("bare field names like `count`"),
        "actor `self` guidance should mention bare field access: {:?}",
        output.errors
    );
    assert!(
        self_error.message.contains("`this`"),
        "actor `self` guidance should mention `this`: {:?}",
        output.errors
    );
    assert!(
        !self_error.message.contains("named receiver parameter"),
        "actor `self` guidance should not use trait/impl receiver advice: {:?}",
        output.errors
    );
}

#[test]
fn test_actor_init_and_method_self_use_actor_guidance() {
    // The `terminate { }` block surface was retired; the equivalent
    // `#[on(stop)]` fn is exercised by the dedicated lifecycle-hook
    // fixtures (see `hew-types/tests/actor_lifecycle_hooks.rs`).
    let output = typecheck(
        r"
        actor Counter {
            let count: i32;

            init() {
                self.count
            }

            fn current() -> i32 {
                self.count
            }
        }

        fn main() {}
    ",
    );
    let self_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.message.contains("`self`"))
        .collect();
    assert_eq!(
        self_errors.len(),
        2,
        "expected actor-specific `self` errors in init and method: {:?}",
        output.errors
    );
    for self_error in self_errors {
        assert!(
            self_error.message.contains("bare field names like `count`"),
            "actor `self` guidance should mention bare field access: {:?}",
            output.errors
        );
        assert!(
            self_error.message.contains("`this`"),
            "actor `self` guidance should mention `this`: {:?}",
            output.errors
        );
        assert!(
            !self_error.message.contains("named receiver parameter"),
            "actor `self` guidance should not use trait/impl receiver advice: {:?}",
            output.errors
        );
    }
}

#[test]
fn test_actor_on_stop_hook_valid_field_access() {
    let output = typecheck(
        r"
        actor Worker {
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
        r"
        actor Counter {
            var count: i32 = 0;
            receive fn get() -> i32 { count }
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

// #2448: when a spawn arg name matches BOTH an explicit `init(...)` parameter
// and a state field whose types DISAGREE, the checker validates the arg
// against the init-param type but MIR routes the arg value into the
// same-named state-field slot. Before this fix the mismatch surfaced only as
// a raw `RecordInit ... source slot type does not match struct field type`
// codegen dump; now the checker names the collision (arg, both declarations,
// both types) with a note pointing at the init parameter.
#[test]
fn test_spawn_arg_param_field_type_collision_named_at_checker() {
    let output = typecheck(
        r"
        actor Counter {
            var count: bool;
            init(count: i32) {
            }
            receive fn ping() -> i32 { 0 }
        }
        fn main() {
            let _c = spawn Counter(count: 5);
        }
    ",
    );
    assert!(
        output.errors.iter().any(|e| {
            e.message.contains("names both `init` parameter")
                && e.message.contains("state field")
                && e.message.contains("types disagree")
        }),
        "param/field type collision should be named at the checker, not left \
         to a codegen RecordInit slot-type dump: {:?}",
        output.errors
    );
    // The raw codegen-shaped diagnostic must never be the surface for this.
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("RecordInit") || e.message.contains("source slot type")),
        "collision must not surface as a raw RecordInit dump: {:?}",
        output.errors
    );
}

// The collision guard must NOT fire when the same-named init parameter and
// state field agree on type — this is the common, legal shape (an
// `init(count: i32)` that stores into a `var count: i32` field), and the
// spawn arg checks cleanly against the init-param type.
#[test]
fn test_spawn_arg_param_field_same_type_is_not_a_collision() {
    let output = typecheck(
        r"
        actor Counter {
            var count: i32;
            init(count: i32) {
            }
            receive fn ping() -> i32 { 0 }
        }
        fn main() {
            let _c = spawn Counter(count: 5);
        }
    ",
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("types disagree")),
        "matching param/field types must not be flagged as a collision: {:?}",
        output.errors
    );
}

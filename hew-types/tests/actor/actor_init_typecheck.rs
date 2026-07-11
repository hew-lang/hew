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

// #2448 — a spawn arg name that matches BOTH an `init` parameter and a state
// field with a DIFFERENT type is unsatisfiable. Before the checker guard this
// slipped through arg checking (the arg was validated against the init param
// only) and failed closed in codegen with a raw `E_CODEGEN_FRONT_FAIL_CLOSED`
// RecordInit verifier dump. It must now be a clean checker diagnostic that
// names the parameter, the field, and the two disagreeing types.
#[test]
fn spawn_arg_name_collision_differing_types_reports_checker_diagnostic() {
    let output = typecheck(
        r"
        actor Widget {
            var n: string;
            init(n: i32) {
                let _ = n;
            }
            receive fn peek() -> string { n }
        }
        fn main() {
            let _w = spawn Widget(n: 7);
        }
    ",
    );
    let hit = output.errors.iter().find(|e| {
        e.message.contains("matches both the `init` parameter") && e.message.contains("state field")
    });
    assert!(
        hit.is_some(),
        "differing-type spawn-arg name collision should be a named checker \
         diagnostic, not a codegen fail-closed: {:?}",
        output.errors
    );
    let msg = &hit.unwrap().message;
    assert!(
        msg.contains("i32") && msg.contains("string"),
        "diagnostic should name both disagreeing types (i32 / string): {msg}"
    );
    // No raw codegen-front verifier dump should leak through.
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("RecordInit")
                || e.message.contains("E_CODEGEN_FRONT_FAIL_CLOSED")),
        "the checker diagnostic must pre-empt the raw codegen dump: {:?}",
        output.errors
    );
}

// #2448 companion — a same-name collision whose types AGREE is a legitimate
// COEXIST spawn (state field passed at spawn time alongside a same-named init
// param); it must NOT trip the differing-type guard.
#[test]
fn spawn_arg_name_collision_matching_types_is_accepted() {
    let output = typecheck(
        r"
        actor Widget2 {
            var n: i64;
            init(n: i64) {
                let _ = n;
            }
            receive fn peek() -> i64 { n }
        }
        fn main() {
            let _w = spawn Widget2(n: 7);
        }
    ",
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("matches both the `init` parameter")),
        "a same-type same-name spawn arg is a valid COEXIST, not a collision: {:?}",
        output.errors
    );
}

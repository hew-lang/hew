mod common;

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
            let count: i32;
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
fn test_actor_init_terminate_and_method_self_use_actor_guidance() {
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

            terminate {
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
        3,
        "expected actor-specific `self` errors in init, method, and terminate: {:?}",
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
fn test_actor_terminate_valid_field_access() {
    let output = typecheck(
        r"
        actor Worker {
            let id: i32;

            terminate {
                println(id);
            }
        }

        fn main() {}
    ",
    );
    assert!(
        output.errors.is_empty(),
        "terminate block should be able to read bare field names: {:?}",
        output.errors
    );
}

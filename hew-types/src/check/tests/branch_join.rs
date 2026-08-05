//! Move/release tracking across branch joins.
//!
//! Two authorities reject use-after-consume: this env checker (early, with the
//! span-pair diagnostic and the `clone` hint) and the checked-MIR dataflow pass
//! (`E_MIR_CHECK: UseAfterConsume`, flow-sensitive, the soundness backstop).
//! Every test here names which authority it expects: these all assert on the
//! env checker's diagnostics, so a shape whose only rejection is MIR's belongs
//! in the compiled `.hew` corpus, not here.
//!
//! The pair that matters is bidirectional. Consuming a binding once per path
//! must be ACCEPTED; consuming it twice on any single path, or using it after a
//! join where any path consumed it, must still be REJECTED.

#[allow(
    clippy::wildcard_imports,
    reason = "checker tests use the shared private test helpers"
)]
use super::*;

/// A `#[resource]` with both a discharging `close` and a plain consuming
/// method, so tests can separate the release path from the move path.
const SOCKET: &str = r"
#[resource]
type Socket { fd: i64 }

impl Socket {
    fn close(consuming self) {}
    fn detach(consuming self) -> i64 { self.fd }
}
";

fn check_with_socket(body: &str) -> TypeCheckOutput {
    check_source(&format!("{SOCKET}{body}"))
}

fn consume_errors(output: &TypeCheckOutput) -> Vec<String> {
    output
        .errors
        .iter()
        .filter(|error| {
            error.message.contains("use of moved value")
                || error.message.contains("cannot be closed more than once")
                || error.message.contains("cannot consume released resource")
        })
        .map(|error| error.message.clone())
        .collect()
}

fn assert_accepts(label: &str, body: &str) {
    let output = check_with_socket(body);
    let consume = consume_errors(&output);
    assert!(
        consume.is_empty(),
        "{label}: consuming once per path must be accepted, got: {consume:#?}"
    );
}

fn assert_rejects(label: &str, body: &str) {
    let output = check_with_socket(body);
    let consume = consume_errors(&output);
    assert!(
        !consume.is_empty(),
        "{label}: a second consume on one path must still be rejected, got no \
         consume diagnostic; all errors: {:#?}",
        output.errors
    );
}

// --- Accept direction: one consume per path -------------------------------

#[test]
fn close_in_every_match_arm_is_accepted() {
    assert_accepts(
        "match arms",
        r"
        fn probe(r: Result<i64, string>) {
            let held = Socket { fd: 1 };
            match r {
                Ok(_) => { held.close(); },
                Err(_) => { held.close(); },
            }
        }
        ",
    );
}

#[test]
fn move_in_every_match_arm_is_accepted() {
    assert_accepts(
        "match arms, plain consuming method",
        r"
        fn probe(r: Result<i64, string>) {
            let held = Socket { fd: 1 };
            match r {
                Ok(_) => { let _ = held.detach(); },
                Err(_) => { let _ = held.detach(); },
            }
        }
        ",
    );
}

#[test]
fn close_in_both_if_arms_is_accepted() {
    assert_accepts(
        "if/else",
        r"
        fn probe(x: bool) {
            let held = Socket { fd: 1 };
            if x { held.close(); } else { held.close(); }
        }
        ",
    );
}

#[test]
fn close_in_every_link_of_an_else_if_chain_is_accepted() {
    assert_accepts(
        "else-if chain",
        r"
        fn probe(n: i64) {
            let held = Socket { fd: 1 };
            if n == 0 {
                held.close();
            } else if n == 1 {
                held.close();
            } else {
                held.close();
            }
        }
        ",
    );
}

#[test]
fn close_in_both_if_let_arms_is_accepted() {
    assert_accepts(
        "if-let/else",
        r"
        fn probe(o: Option<i64>) {
            let held = Socket { fd: 1 };
            if let Some(_v) = o {
                held.close();
            } else {
                held.close();
            }
        }
        ",
    );
}

#[test]
fn consume_on_a_returning_path_does_not_leak_into_the_fall_through() {
    // The diverging-arm shape: the `return` path's consume must not reach the
    // code after the join, because that code only runs when the return did not.
    assert_accepts(
        "diverging then-arm",
        r"
        fn probe(x: bool) -> i64 {
            let held = Socket { fd: 1 };
            if x { return held.detach(); }
            held.detach()
        }
        ",
    );
}

#[test]
fn consume_in_a_diverging_match_arm_does_not_leak_into_the_other_arms() {
    assert_accepts(
        "diverging match arm ordered first",
        r"
        fn probe(r: Result<i64, string>) -> i64 {
            let held = Socket { fd: 1 };
            match r {
                Ok(_) => { return held.detach(); },
                Err(_) => { held.detach() },
            }
        }
        ",
    );
}

#[test]
fn let_else_diverging_branch_does_not_leak_its_consume() {
    assert_accepts(
        "let-else",
        r"
        fn probe(o: Option<i64>) -> i64 {
            let held = Socket { fd: 1 };
            let Some(_v) = o else { return held.detach(); };
            held.detach()
        }
        ",
    );
}

#[test]
fn a_consume_in_one_arm_still_allows_a_consume_in_a_later_sibling_arm() {
    // Three-way mix: moved in arm 1, released in arm 2, diverging arm 3. Each
    // arm starts from the branch-entry state, so none of them see each other.
    assert_accepts(
        "mixed arms",
        r"
        fn probe(n: i64) -> i64 {
            let held = Socket { fd: 1 };
            match n {
                0 => { held.detach() },
                1 => { held.close(); 0 },
                _ => { return held.detach(); },
            }
        }
        ",
    );
}

// --- Reject direction: a second consume on one path -----------------------

#[test]
fn use_after_a_join_where_every_arm_consumed_is_rejected() {
    // Union teeth. Accepting the arms must not make the binding live again.
    assert_rejects(
        "use after fully-consuming join",
        r"
        fn probe(r: Result<i64, string>) -> i64 {
            let held = Socket { fd: 1 };
            match r {
                Ok(_) => { held.close(); },
                Err(_) => { held.close(); },
            }
            held.detach()
        }
        ",
    );
}

#[test]
fn close_after_a_join_where_one_arm_closed_is_rejected() {
    // Double-close teeth: on the path that took the `if`, this closes twice.
    assert_rejects(
        "close after conditional close",
        r"
        fn probe(x: bool) {
            let held = Socket { fd: 1 };
            if x { held.close(); }
            held.close();
        }
        ",
    );
}

#[test]
fn conditional_move_then_unconditional_use_is_rejected() {
    assert_rejects(
        "conditional move then use",
        r"
        fn probe(x: bool) -> i64 {
            let held = Socket { fd: 1 };
            if x { let _ = held.detach(); }
            held.detach()
        }
        ",
    );
}

#[test]
fn two_consumes_in_the_same_arm_are_rejected() {
    assert_rejects(
        "sequential double consume inside one arm",
        r"
        fn probe(x: bool) {
            let held = Socket { fd: 1 };
            if x {
                held.close();
                held.close();
            } else {
                held.close();
            }
        }
        ",
    );
}

#[test]
fn consume_before_the_branch_then_again_in_an_arm_is_rejected() {
    assert_rejects(
        "consume at entry then in an arm",
        r"
        fn probe(x: bool) {
            let held = Socket { fd: 1 };
            held.close();
            if x { held.close(); }
        }
        ",
    );
}

#[test]
fn consume_after_a_join_where_only_a_diverging_arm_stayed_live_is_rejected() {
    // Divergence exclusion removes the returning arm from the join, but the
    // remaining arm did consume, so the post-join use is still a second consume.
    assert_rejects(
        "diverging arm live, other arm consumed",
        r"
        fn probe(x: bool) -> i64 {
            let held = Socket { fd: 1 };
            if x { return 0; } else { let _ = held.detach(); }
            held.detach()
        }
        ",
    );
}

// --- Loop escape: `Never` is not the same as "leaves the function" --------

#[test]
fn a_break_arm_consume_still_rejects_a_use_below_the_loop() {
    // `break` types `Never` but rejoins immediately after the loop, so the
    // consume on the break path reaches the code below it. Excluding the arm
    // because it "diverges" admits a genuine double consume.
    assert_rejects(
        "break escapes to below the loop",
        r"
        fn probe(n: i64) -> i64 {
            let held = Socket { fd: 1 };
            for i in 0..3 {
                if i == n {
                    let _ = held.detach();
                    break;
                }
            }
            held.detach()
        }
        ",
    );
}

#[test]
fn a_continue_arm_consume_still_rejects_a_use_below_the_loop() {
    assert_rejects(
        "continue rejoins at the loop head",
        r"
        fn probe(n: i64) -> i64 {
            let held = Socket { fd: 1 };
            for i in 0..3 {
                if i == n {
                    let _ = held.detach();
                    continue;
                }
            }
            held.detach()
        }
        ",
    );
}

#[test]
fn a_labelled_break_arm_consume_still_rejects_a_use_below_the_loop() {
    // The break sits inside an inner loop, so nesting depth alone would say it
    // cannot escape the arm. The label is what carries it out to `@outer`.
    assert_rejects(
        "labelled break escapes two loops",
        r"
        fn probe(n: i64) -> i64 {
            let held = Socket { fd: 1 };
            @outer: for i in 0 .. 3 {
                for j in 0 .. 3 {
                    if i + j == n {
                        let _ = held.detach();
                        break @outer;
                    }
                }
            }
            held.detach()
        }
        ",
    );
}

#[test]
fn a_return_arm_containing_an_inner_loop_break_still_skips_the_join() {
    // The break belongs to a loop written INSIDE the arm, so it cannot carry
    // the consume past the branch; the arm still leaves the function.
    assert_accepts(
        "inner-loop break does not make a returning arm reachable",
        r"
        fn probe(flag: bool, n: i64) -> i64 {
            let held = Socket { fd: 1 };
            if flag {
                for i in 0..3 {
                    if i == n {
                        break;
                    }
                }
                return held.detach();
            }
            held.detach()
        }
        ",
    );
}

// --- Operand scoping: sequential setup vs branched alternatives -----------

#[test]
fn a_guard_consume_still_rejects_a_consume_in_a_later_arm() {
    // A guard runs whenever its pattern matched and every earlier arm did not,
    // so guard 1 and body 2 both execute on one path. Guards thread through the
    // fall-through state rather than restarting from the branch entry.
    assert_rejects(
        "guard consume then sibling arm consume",
        r"
        fn probe(r: Result<i64, string>) -> i64 {
            let held = Socket { fd: 1 };
            match r {
                Ok(_) if held.detach() > 0 => { 10 },
                Err(_) => { held.detach() },
                Ok(_) => { 20 },
            }
        }
        ",
    );
}

#[test]
fn an_else_if_condition_consume_still_rejects_a_use_after_the_chain() {
    assert_rejects(
        "else-if condition consume",
        r"
        fn probe(n: i64) -> i64 {
            let held = Socket { fd: 1 };
            if n == 0 {
                let _ = 1;
            } else if held.detach() > 0 {
                let _ = 2;
            }
            held.detach()
        }
        ",
    );
}

// --- R2: the snapshot must not carry lint state ---------------------------

#[test]
fn a_read_inside_a_branch_arm_still_counts_as_a_use() {
    // Restoring per arm must rewind ownership only. If it rewound `read_count`
    // the unused-variable lint would fire on a binding that was read.
    let output = check_source(
        r#"
        fn probe(x: bool) {
            let value = 7;
            if x {
                println(f"{value}");
            } else {
                println("other");
            }
        }
        "#,
    );
    assert!(
        !output
            .warnings
            .iter()
            .any(|warning| warning.message.contains("never used")),
        "a read inside an arm must survive the per-arm ownership restore, got: {:#?}",
        output.warnings
    );
}

#[test]
fn a_write_inside_a_branch_arm_still_counts_as_a_mutation() {
    let output = check_source(
        r"
        fn probe(x: bool) -> i64 {
            var total = 0;
            if x {
                total = 1;
            }
            total
        }
        ",
    );
    assert!(
        !output
            .warnings
            .iter()
            .any(|warning| warning.message.contains("never reassigned")),
        "a write inside an arm must survive the per-arm ownership restore, got: {:#?}",
        output.warnings
    );
}

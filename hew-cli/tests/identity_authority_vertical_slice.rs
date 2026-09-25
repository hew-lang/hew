//! Identity-authority batch (plans/identity-authority-final.md section 6):
//! acceptance oracles for the wrong-result families the audit confirmed on
//! `origin/integration/v060-rc4` (8a786c595). This is L0 scaffolding: every
//! test here is a known failure until the carriers land (P1-B5) and fixes
//! the family it names; the ratchet row in
//! `scripts/nextest-expected-failures.tsv` carries the same reason. A lane
//! that fixes a family deletes both the ratchet row and this file's
//! now-passing assertion's `#[ignore]`/expected-failure status in the same
//! change — never widens the expectation to match wrong output.
//!
//! `accept/` fixtures currently run and print the audit's WRONG (run)
//! output; the `.expected` file holds the TARGET output from section 6, so
//! the assertion here fails until the family is fixed. `reject/` fixtures
//! currently either wrongly accept the program (unsound) or refuse it with
//! the wrong diagnostic; each test names the target diagnostic and asserts
//! against it.

mod support;

use std::process::Command;

use support::{hew_binary, repo_root, require_codegen, strip_ansi};

fn run_accept_fixture(fixture: &str) -> (bool, String) {
    require_codegen();
    let source = repo_root().join(format!("tests/vertical-slice/accept/{fixture}.hew"));
    let output = Command::new(hew_binary())
        .args(["run", source.to_str().expect("source utf-8")])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew run");
    (
        output.status.success(),
        strip_ansi(&String::from_utf8_lossy(&output.stdout)),
    )
}

fn check_reject_fixture(fixture: &str) -> (bool, String) {
    require_codegen();
    let source = repo_root().join(format!("tests/vertical-slice/reject/{fixture}.hew"));
    let output = Command::new(hew_binary())
        .args(["check", source.to_str().expect("source utf-8")])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    (output.status.success(), combined)
}

/// W1 (dot-call selection, R1): an inherent method must win over any trait
/// method of the same name; only a bound/dyn/`Trait.m(recv)` path reaches
/// the trait body. Known failure: dispatch currently resolves `t.show()` to
/// whichever `impl` block registered last (`label`, `label`, `label`).
#[test]
fn identity_w1a_inherent_wins_dot_call() {
    let (ok, stdout) = run_accept_fixture("identity_w1a");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/identity_w1a.expected"),
    )
    .expect("read identity_w1a.expected");
    assert!(ok, "identity_w1a must run to completion");
    assert_eq!(stdout, expected, "R1: inherent method must win t.show()");
}

/// W1 (D509 structural witness): an inherent method wins over a
/// structurally-satisfied trait method of the same name.
#[test]
fn identity_w1c_inherent_wins_over_structural_trait() {
    let (ok, stdout) = run_accept_fixture("identity_w1c");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/identity_w1c.expected"),
    )
    .expect("read identity_w1c.expected");
    assert!(ok, "identity_w1c must run to completion");
    assert_eq!(stdout, expected, "R4: inherent witness must win b.at(0)");
}

/// W3 (R3 callee order): a local closure must shadow a same-named module
/// `fn` in lexical scope.
#[test]
fn identity_w3b_closure_shadows_fn() {
    let (ok, stdout) = run_accept_fixture("identity_w3b");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/identity_w3b.expected"),
    )
    .expect("read identity_w3b.expected");
    assert!(ok, "identity_w3b must run to completion");
    assert_eq!(
        stdout, expected,
        "R3: the local closure `helper` must shadow `fn helper`"
    );
}

/// W4 (R6 type parameters): a type parameter never resolves to a nominal
/// of the same spelling, so `g<T>(x: T) { x.v }` is refused beside
/// `type T { v: i64 }` exactly as it is without one.
#[test]
fn identity_w4a_type_param_never_shadowed_by_nominal() {
    for fixture in ["identity_w4a", "identity_w4a_control"] {
        let (ok, combined) = check_reject_fixture(fixture);
        assert!(!ok, "{fixture} must be refused; got success:\n{combined}");
        assert!(
            combined.contains("cannot access field `v` on type parameter `T`"),
            "{fixture}: R6 diagnostic expected; got:\n{combined}"
        );
    }
}

/// W2 (R2 predicates by identity, D554-2): a user trait spelled `Send` is an
/// ordinary trait, not the `Send` marker, so `dyn Send` must not satisfy the
/// actor-mailbox `Send` obligation. Known failure: today's mailbox check
/// matches the predicate by spelling and unsoundly accepts the send.
#[test]
fn identity_w2a_user_trait_named_send_is_not_the_predicate() {
    for (fixture, trait_name) in [("identity_w2a", "Send"), ("identity_w2a_control", "Pinger")] {
        let (ok, combined) = check_reject_fixture(fixture);
        assert!(!ok, "{fixture} must be refused; got success:\n{combined}");
        assert!(
            combined.contains(&format!(
                "cannot send `dyn {trait_name}` to actor: type is not Send"
            )),
            "{fixture}: expected the Send refusal; got:\n{combined}"
        );
    }
}

/// W2 (R4 structural witness + R2): `Q`'s `dup` witnesses a user trait
/// spelled `Clone`, not the `Clone` predicate; `f<T: Clone>` must refuse `Q`
/// for missing the predicate's own witness.
#[test]
fn identity_w2b_user_trait_named_clone_is_not_the_predicate() {
    let (ok, combined) = check_reject_fixture("identity_w2b");
    assert!(
        !ok,
        "target: refused \"`Q` does not implement trait `Clone` ... missing `dup`\"; \
         got success:\n{combined}"
    );
}

/// W2 (R2, Ord/PartialOrd): a user `Ord` predicate-shaped bound must not
/// imply the `PartialOrd` predicate by spelling. Known failure: the current
/// compiler fails closed with `E_SIR_UNSUPPORTED`, a compiler limitation
/// that section 6 counts as a failure in its own right (never a pass).
#[test]
fn identity_w2b_ord_predicate_bound_not_implied_by_spelling() {
    let (ok, combined) = check_reject_fixture("identity_w2b_ord");
    assert!(
        !ok,
        "target: refused \"`T: Ord` does not imply `PartialOrd`\" with no \
         compiler-limitation escape hatch; got success:\n{combined}"
    );
    assert!(
        !combined.contains("E_SIR_UNSUPPORTED"),
        "E_SIR_UNSUPPORTED is a compiler-limitation failure, not the target \
         source diagnostic (plans/identity-authority-final.md section 6): \n{combined}"
    );
    assert!(
        combined.contains("does not implement trait `PartialOrd`"),
        "the user `Ord` must not imply the user `PartialOrd`; got:\n{combined}"
    );
    // Negative control: the predicates themselves keep `Ord` implying
    // `PartialOrd`.
    let (ok, stdout) = run_accept_fixture("identity_w2b_ord_control");
    assert!(ok, "predicate Ord must satisfy PartialOrd");
    assert_eq!(stdout, "5\n");
}

/// W11 (compiler-minted spelling as a string): a machine's companion event
/// type collision must be reported as an ordinary duplicate-declaration
/// diagnostic that names the owning machine. Known failure: the diagnostic
/// exists (the refusal itself is already correct, FC per section 1.5) but
/// its note points at the `type SlotEvent` declaration, not at
/// `machine Slot`, because the companion is a string in `known_types`
/// rather than a `DefId`-backed declaration.
#[test]
fn identity_w9a_machine_event_collision_names_the_machine() {
    let (ok, combined) = check_reject_fixture("identity_w9a");
    assert!(
        !ok,
        "SlotEvent collision must still be refused; got success:\n{combined}"
    );
    assert!(
        combined.contains("SlotEvent") && combined.contains("defined multiple times"),
        "expected the duplicate-declaration diagnostic; got:\n{combined}"
    );
    assert!(
        combined.contains("machine Slot") && combined.contains("declares its event type"),
        "target note: \"machine Slot declares its event type SlotEvent here\"; \
         got:\n{combined}"
    );
}

/// W1 (dot-call, ambiguity): two traits declaring the same method name with
/// no inherent method and no bound/dyn context must refuse the dot call as
/// ambiguous, not silently pick the last-registered impl. Known failure:
/// today's dispatch resolves `t.name()` to `B` (last impl wins) instead of
/// refusing.
#[test]
fn identity_w1b_dot_ambiguous_trait_method_refused() {
    let (ok, combined) = check_reject_fixture("identity_w1b_dot");
    assert!(
        !ok,
        "target: refused E_AMBIGUOUS_TRAIT_METHOD naming `A` and `B`; got success:\n{combined}"
    );
}

/// W1 (R1, disjoint method names): two traits with distinct method names
/// (`Sz::size -> i64`, `Nm::size -> string`, same spelling but different
/// traits) implemented on the same type must both resolve independently
/// through their own bound calls. Known failure: the checker conflates
/// same-named methods across unrelated traits into one signature and
/// refuses the program with a spurious type mismatch, never reaching R1's
/// intended output.
#[test]
fn identity_w1d_disjoint_trait_methods_resolve_independently() {
    let (ok, stdout) = run_accept_fixture("identity_w1d");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/identity_w1d.expected"),
    )
    .expect("read identity_w1d.expected");
    assert!(
        ok,
        "identity_w1d must run to completion (target: `1` then `b`)"
    );
    assert_eq!(
        stdout, expected,
        "R1: Sz::size and Nm::size must not collide"
    );
}

/// W2 (D554-2, prelude collision): a user declaration spelled `Display`
/// collides with the protected prelude binding and must be refused with
/// exactly that diagnostic, no cascading errors. This already holds on
/// 8a786c595 (regression oracle, not a known failure): the checker refuses
/// the collision cleanly and emits no other diagnostic.
#[test]
fn identity_w2f_prelude_collision_no_cascade() {
    let (ok, combined) = check_reject_fixture("identity_w2f");
    assert!(
        !ok,
        "target: refused for colliding with the protected prelude binding `Display`; \
         got success:\n{combined}"
    );
    assert!(
        combined.contains("collides with the protected prelude binding"),
        "expected the prelude-collision diagnostic; got:\n{combined}"
    );
    assert_eq!(
        combined.matches("error:").count(),
        1,
        "target: the collision is the only diagnostic, no cascade; got:\n{combined}"
    );
}

/// W2 (D554-2, `Drop` is not a real trait): `impl Drop for P` must be
/// refused and pointed at `#[resource]` with `close()` as the deterministic
/// alternative. This already holds on 8a786c595 (regression oracle, not a
/// known failure).
#[test]
fn identity_w2g_impl_drop_refused_with_resource_help() {
    let (ok, combined) = check_reject_fixture("identity_w2g");
    assert!(
        !ok,
        "target: refused \"unknown trait `Drop`\" with `#[resource]` help; \
         got success:\n{combined}"
    );
    assert!(
        combined.contains("not supported") && combined.contains("#[resource]"),
        "expected the Drop-unsupported diagnostic naming #[resource]; got:\n{combined}"
    );
}

/// W3 (R3, callee identity): a user `fn` spelled `len`/`to_string`/
/// `assert_eq` must shadow the builtin of the same name, the same rule
/// `identity_w3b` exercises for a closure over a module fn. Known failure:
/// the builtins win regardless of the user declarations in scope, including
/// the assertion builtin panicking instead of calling the user override.
#[test]
fn identity_w3a_user_fn_shadows_builtin() {
    let (ok, stdout) = run_accept_fixture("identity_w3a");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/identity_w3a.expected"),
    )
    .expect("read identity_w3a.expected");
    assert!(
        ok,
        "identity_w3a must run to completion under the user overrides"
    );
    assert_eq!(
        stdout, expected,
        "R3: user `len`/`to_string`/`assert_eq` must win over the builtins"
    );
}

/// W3 (R3, callee identity): a user `fn println` must shadow the builtin
/// `println`, so calling it produces no output. Known failure: the builtin
/// wins and prints `x` regardless of the user declaration.
#[test]
fn identity_w3e_user_println_shadows_builtin() {
    let (ok, stdout) = run_accept_fixture("identity_w3e");
    let expected = std::fs::read_to_string(
        repo_root().join("tests/vertical-slice/accept/identity_w3e.expected"),
    )
    .expect("read identity_w3e.expected");
    assert!(
        ok,
        "identity_w3e must run to completion under the user println"
    );
    assert_eq!(
        stdout, expected,
        "R3: user `println` must win, producing no output"
    );
}

/// W4 (R6, pattern identity): a struct pattern naming a variant/record
/// (`Other { .. }`) that does not exist on the scrutinee's type (`Point`)
/// must be refused, never silently matched by field shape. Known failure:
/// the checker accepts the mismatched pattern name and matches by field
/// shape alone.
#[test]
fn identity_w4b_pattern_name_must_match_scrutinee_type() {
    let (ok, combined) = check_reject_fixture("identity_w4b");
    assert!(
        !ok && combined.contains("pattern names `Other`, scrutinee is `Point`"),
        "target: refused \"pattern names `Other`, scrutinee is `Point`\"; \
         got:\n{combined}"
    );
    // Negative control: the scrutinee's own name still destructures.
    let (ok, stdout) = run_accept_fixture("identity_w4b_control");
    assert!(ok, "Point {{ x, .. }} must match a Point");
    assert_eq!(stdout, "1\n");
}

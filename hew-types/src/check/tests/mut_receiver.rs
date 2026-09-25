#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;
use crate::LangItem;

#[test]
fn var_self_methods_accept_mutable_field_places_and_reject_immutable_roots() {
    let declarations = r"
        trait Bump { fn bump(var self) -> i64; }
        type Counter { value: i64 }
        impl Bump for Counter {
            fn bump(var self) -> i64 { self.value += 1; self.value }
        }
        type Inner<T> { counter: T }
        type Outer<T> { inner: Inner<T> }
    ";
    for (binding, mutable) in [("var", true), ("let", false)] {
        for (ty, value, receiver) in [
            (
                "Outer<Counter>",
                "Outer { inner: Inner { counter: Counter { value: 0 } } }",
                "owner.inner.counter",
            ),
            ("(Counter, i64)", "(Counter { value: 0 }, 7)", "owner.0"),
        ] {
            let output = check_source(&format!(
                "{declarations} fn main() -> i64 {{ {binding} owner: {ty} = {value}; {receiver}.bump() }}"
            ));
            if mutable {
                assert!(output.errors.is_empty(), "{receiver}: {:?}", output.errors);
                assert!(!output
                    .warnings
                    .iter()
                    .any(|warning| warning.message.contains("never reassigned")));
            } else {
                assert!(
                    output
                        .errors
                        .iter()
                        .any(|error| error.kind == TypeErrorKind::MutabilityError
                            && error.message.contains("`owner` is not declared with `var`")),
                    "{:?}",
                    output.errors
                );
            }
        }
        let output = check_source(&format!(
            "{declarations} fn bump<T: Bump>(source: Outer<T>) -> i64 {{ {binding} owner = source; owner.inner.counter.bump() }}"
        ));
        if mutable {
            assert!(output.errors.is_empty(), "{:?}", output.errors);
        } else {
            assert!(
                output
                    .errors
                    .iter()
                    .any(|error| error.kind == TypeErrorKind::MutabilityError
                        && error
                            .message
                            .contains("statically dispatched on type parameter")
                        && error.message.contains("`owner` is not declared with `var`")),
                "{:?}",
                output.errors
            );
        }
    }
    let output = check_source(&format!(
        "{declarations} fn main() -> i64 {{ Counter {{ value: 0 }}.bump() }}"
    ));
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::MutabilityError
                && error
                    .message
                    .contains("this expression is not declared with `var`")),
        "{:?}",
        output.errors
    );
}

// ---------------------------------------------------------------------------
// Q297 Stage 1 — receiver-mutability flag plumbing.
//
// These tests replace the descoped accept fixtures
// `iter_next_mut_receiver.hew` (S1-V2) and `iter_var_receiver_drop_once.hew`
// (S1-V5). End-to-end coverage of those shapes is blocked on pre-existing
// gaps (`Self`-substitution at the MIR boundary for user trait-impl bodies
// and `MethodCallNoRewrite` on direct `v.into_iter()` / `it.next()` outside
// the for-loop desugar). The checker-level invariant — that the
// `requires_mutable_receiver` flag is populated everywhere the call-site
// gate reads it from — is exactly what Stage 1 owns, so we pin it here.

#[test]
fn q297_user_iterator_impl_records_mut_receiver_flag_in_both_tables() {
    // `lookup_named_method_sig` prefers `td.methods` before `fn_sigs`, so
    // the flag must be set in BOTH tables. Missing either one silently
    // disables the caller-side mutable-binding gate.
    let output = check_source(
        r"
        type Counter { val: i32 }

        impl Iterator for Counter {
            type Item = i32;
            fn next(var self) -> Option<i32> {
                .Some(self.val)
            }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:?}",
        output.errors,
    );
    let sig = output
        .sigs()
        .get("Counter::next")
        .expect("Counter::next must be registered in fn_sigs");
    assert!(
        sig.requires_mutable_receiver,
        "fn_sigs[Counter::next].requires_mutable_receiver must be true for `var self`",
    );
    let td = output
        .type_def_at_path("Counter")
        .expect("Counter type must be registered");
    let method_sig = td
        .methods
        .get("next")
        .expect("Counter::next must be present in td.methods");
    assert!(
        method_sig.requires_mutable_receiver,
        "td.methods[next].requires_mutable_receiver must be true for `var self`",
    );
}

#[test]
fn q297_immut_self_method_records_no_mut_receiver_flag() {
    // Negative control: a plain `self` receiver must NOT carry the flag.
    let output = check_source(
        r"
        type Counter { val: i32 }

        impl Counter {
            fn peek(self) -> i32 { self.val }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:?}",
        output.errors,
    );
    let sig = output
        .sigs()
        .get("Counter::peek")
        .expect("Counter::peek must be registered");
    assert!(
        !sig.requires_mutable_receiver,
        "plain `self` receiver must not set requires_mutable_receiver",
    );
}

#[test]
fn q297_trait_var_self_vs_impl_self_rejects_with_receiver_mutability_detail() {
    // Trait declares `var self`; impl uses plain `self`. Q004's
    // receiver-mutability axis (added in Stage 1) must reject this.
    let output = check_source(
        r"
        trait Bump {
            fn step(var self) -> i64;
        }

        type Box { n: i64 }

        impl Bump for Box {
            fn step(self) -> i64 { self.n }
        }
        ",
    );
    let mismatch = output.errors.iter().find(|e| {
        matches!(
            &e.kind,
            TypeErrorKind::TraitImplSignatureMismatch { detail, .. }
                if *detail == "receiver mutability"
        )
    });
    assert!(
        mismatch.is_some(),
        "expected TraitImplSignatureMismatch(receiver mutability), got: {:?}",
        output.errors,
    );
}

#[test]
fn q297_let_bound_receiver_rejects_var_self_method_call() {
    // Caller-side gate: a `let`-bound (immutable) receiver cannot dispatch
    // through a method that requires `var self`. Use a trait-impl shape —
    // Stage 1 keeps the long-standing rejection of `var self` on inherent
    // impls; the relaxation only applies to trait impls where the trait
    // contract gives the mutation observable meaning.
    let output = check_source(
        r"
        trait Bump {
            fn step(var self) -> i64;
        }

        type Counter { val: i64 }

        impl Bump for Counter {
            fn step(var self) -> i64 {
                self.val = self.val + 1;
                self.val
            }
        }

        fn main() {
            let c = Counter { val: 0 };
            c.step();
        }
        ",
    );
    let mutability = output.errors.iter().find(|e| {
        matches!(e.kind, TypeErrorKind::MutabilityError)
            && e.message.contains("requires a mutable binding receiver")
    });
    assert!(
        mutability.is_some(),
        "expected MutabilityError on let-bound receiver, got: {:?}",
        output.errors,
    );
}

#[test]
fn q297_var_bound_receiver_accepts_var_self_method_call() {
    // Positive control for the caller-side gate: a `var`-bound receiver
    // must dispatch cleanly through the same `var self` trait method.
    let output = check_source(
        r"
        trait Bump {
            fn step(var self) -> i64;
        }

        type Counter { val: i64 }

        impl Bump for Counter {
            fn step(var self) -> i64 {
                self.val = self.val + 1;
                self.val
            }
        }

        fn main() {
            var c = Counter { val: 0 };
            c.step();
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "var-bound receiver must accept var-self trait-method call, got: {:?}",
        output.errors,
    );
}

#[test]
fn concrete_specialised_builtin_var_self_preserves_vec_dispatch_authority() {
    // A receiver binding reconstructed from `current_self_type` must retain
    // the builtin discriminator on parameterised builtins. Without
    // `BuiltinType::Vec`, `self[0] = 99` type-checks as a generic Index
    // operation but publishes no resolved `Vec::set` call for HIR/MIR.
    let output = check_source(
        r"
        trait Bump {
            fn bump(var self);
        }

        impl Bump for Vec<i64> {
            fn bump(var self) {
                self[0] = 99;
            }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:?}",
        output.errors,
    );
    assert!(
        output
            .resolved_calls
            .values()
            .any(|call| call.method_target.family == MethodTargetFamily::Vec(VecMethod::Set)),
        "Vec index assignment inside the specialised var-self impl must publish \
         a checker-authoritative Vec::set call; got: {:?}",
        output.resolved_calls,
    );
}

#[test]
fn user_generic_builtin_shadow_var_self_preserves_source_identity() {
    let output = check_source_allowing_prelude_redeclaration(
        r"
        type Option<T> { value: T, }

        trait Bump {
            fn bump(var self);
        }

        impl Bump for Option<i64> {
            fn bump(var self) {
                self.value = 99;
            }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:?}",
        output.errors,
    );
    let option_types: Vec<&Ty> = output
        .expr_types
        .values()
        .filter(|ty| matches!(ty, Ty::Named { head: name_head, .. } if name_head.spelling() == "Option"))
        .collect();
    assert!(
        !option_types.is_empty()
            && option_types.iter().all(|ty| {
                !matches!(
                    *ty,
                    Ty::Named {
                        head: crate::TypeHead::Builtin(BuiltinType::Option),
                        ..
                    }
                )
            }),
        "a source-defined Option<T> must never be tagged as builtin Option in \
         the impl body; got: {:?}",
        output.expr_types,
    );
}

#[test]
fn w3042_static_trait_dispatch_let_bound_receiver_rejects_var_self_method() {
    // W3.042 S2-S4: receiver-mutability gate on the generic-bound
    // StaticTraitDispatch sub-arm. A `let`-bound generic-typed receiver
    // dispatched through a trait method that declares `var self` must
    // emit a MutabilityError that names the dispatch kind so the
    // diagnostic is distinguishable from the (Ty::Named, _) variant.
    let output = check_source(
        r"
        trait Bump {
            fn step(var self) -> i64;
        }

        type Counter { val: i64 }

        impl Bump for Counter {
            fn step(var self) -> i64 {
                self.val = self.val + 1;
                self.val
            }
        }

        fn pump<I: Bump>(it: I) -> i64 {
            it.step()
        }

        fn main() {
            var c = Counter { val: 0 };
            pump(c);
        }
        ",
    );
    let mutability = output.errors.iter().find(|e| {
        matches!(e.kind, TypeErrorKind::MutabilityError)
            && e.message
                .contains("statically dispatched on type parameter")
            && e.message.contains("requires a mutable binding receiver")
    });
    assert!(
        mutability.is_some(),
        "expected StaticTraitDispatch MutabilityError on let-bound generic receiver, got: {:?}",
        output.errors,
    );
}

#[test]
fn w3042_static_trait_dispatch_var_bound_receiver_accepts_var_self_method() {
    // Positive control: a `var`-bound generic-typed receiver dispatched
    // through the same `var self` trait method must type-check clean.
    let output = check_source(
        r"
        trait Bump {
            fn step(var self) -> i64;
        }

        type Counter { val: i64 }

        impl Bump for Counter {
            fn step(var self) -> i64 {
                self.val = self.val + 1;
                self.val
            }
        }

        fn pump<I: Bump>(var it: I) -> i64 {
            it.step()
        }

        fn main() {
            var c = Counter { val: 0 };
            pump(c);
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::MutabilityError)),
        "var-bound generic receiver must accept var-self trait-method call, got: {:?}",
        output.errors,
    );
}

#[test]
fn w3042_dyn_trait_let_bound_receiver_rejects_var_self_method() {
    // W3.042 S2-S4: receiver-mutability gate on the Ty::TraitObject /
    // DynMethodCall arm. A `let`-bound `Box<dyn Trait>` receiver
    // dispatched through a trait method that declares `var self` must
    // emit a MutabilityError that names `dyn <Trait>` so the diagnostic
    // is distinguishable from the (Ty::Named, _) and StaticTraitDispatch
    // variants.
    let output = check_source(
        r"
        trait Bump {
            fn step(var self) -> i64;
        }

        type Counter { val: i64 }

        impl Bump for Counter {
            fn step(var self) -> i64 {
                self.val = self.val + 1;
                self.val
            }
        }

        fn invoke(b: dyn Bump) -> i64 {
            b.step()
        }

        fn main() {
            invoke(Counter { val: 0 });
        }
        ",
    );
    let mutability = output.errors.iter().find(|e| {
        matches!(e.kind, TypeErrorKind::MutabilityError)
            && e.message.contains("dyn Bump")
            && e.message.contains("requires a mutable binding receiver")
    });
    assert!(
        mutability.is_some(),
        "expected DynMethodCall MutabilityError on let-bound dyn receiver, got: {:?}",
        output.errors,
    );
}

#[test]
fn q297_stdlib_iterator_next_and_vec_iter_carry_mut_receiver_flag() {
    // Stage 1 flipped `Iterator::next` in `std/builtins.hew` to `var self`
    // and updated the `VecIter` impl in lockstep. Pin the impl-side
    // registration: `register_builtins_hew_impls` feeds the `impl<T>
    // Iterator for VecIter<T>` block through the same Pass-2 path that
    // user impls use, so `td.methods[next]` on `VecIter` must carry the
    // receiver-mutability flag for `lookup_named_method_sig` to surface
    // it to the caller-side gate. (The bare trait declaration in
    // builtins.hew is intentionally not promoted into `fn_sigs` — see
    // `register_builtins_hew_impls` doc comment — so we pin the
    // load-bearing impl side rather than the trait side.)
    let output = check_source("");
    let td = output
        .type_def_at_path("std.builtins.VecIter")
        .expect("VecIter must be pre-registered from std/builtins.hew");
    let next_sig = td
        .methods
        .get("next")
        .expect("VecIter::next must be present in td.methods");
    assert!(
        next_sig.requires_mutable_receiver,
        "VecIter::next must declare `var self` to match the trait after Q297 Stage 1; \
         td.methods[next].requires_mutable_receiver was false",
    );
}

#[test]
fn builtin_iterator_lang_item_publishes_exact_next_identity() {
    let output = check_source("");
    let binding = output
        .lang_items
        .get(LangItem::IteratorNext.key())
        .expect("the prelude Iterator::next must publish its lang-item identity");
    assert_eq!(binding.trait_name, "Iterator");
    assert_eq!(binding.method_name.as_deref(), Some("next"));
    assert_eq!(output.defs.path(binding.trait_id), "std.builtins.Iterator");
    assert_eq!(
        binding.method_id.map(|id| output.defs.path(id)),
        Some("std.builtins.Iterator::next")
    );
}

/// A418: a failing `var self` method hands its receiver back whole, so a
/// receiver field moved out and not yet restored is refused at every
/// operation that can fail, naming the field and the operation.
#[test]
fn var_self_receiver_stays_whole_wherever_it_can_fail() {
    let declarations = r"
        #[resource]
        type Conn { fd: i64 }
        impl Conn { fn close(consume self) {} fn weight(self) -> i64 { self.fd } }
        type Holder { conn: Conn, count: i64, spare: Option<Conn>, items: Vec<i64>, pool: Vec<Conn> }
        fn work() -> i64 { 1 }
        trait Touch { fn touch(var self, divisor: i64) -> i64; }
    ";
    for (body, operation) in [
        (
            "let conn = self.conn; self.count = 8 / divisor; self.conn = conn; 0",
            "this arithmetic",
        ),
        (
            "let conn = self.conn; let n = work(); self.conn = conn; n",
            "this call",
        ),
        (
            "let conn = self.conn; let n = self.items[0]; self.conn = conn; n",
            "this index",
        ),
        (
            "let conn = self.conn; self.count += 1; self.conn = conn; 0",
            "this assignment",
        ),
        (
            "let conn = self.conn; self.spare = Some(Conn { fd: 2 }); self.conn = conn; 0",
            "this assignment",
        ),
        (
            "let conn = self.conn; { let extra = Conn { fd: 3 }; } self.conn = conn; 0",
            "releasing `extra`",
        ),
        (
            "let conn = self.conn; let n = conn.weight(); self.conn = conn; n",
            "`weight(...)`",
        ),
        // Clearing releases elements that can reach a `close`.
        (
            "let conn = self.conn; self.pool.clear(); self.conn = conn; 0",
            "`clear(...)`",
        ),
        // Option predicates are std source bodies, and a source callee can fail.
        (
            "let conn = self.conn; let spare = self.spare.is_some(); self.conn = conn; if spare { 1 } else { 0 }",
            "`is_some(...)`",
        ),
    ] {
        let output = check_source(&format!(
            "{declarations} impl Touch for Holder {{ fn touch(var self, divisor: i64) -> i64 {{ {body} }} }}"
        ));
        let refusals: Vec<_> = output
            .errors
            .iter()
            .filter(|error| {
                error
                    .message
                    .contains("moved out of the `var self` receiver")
            })
            .collect();
        assert_eq!(refusals.len(), 1, "{body}: {:#?}", output.errors);
        assert!(
            refusals[0].message.contains(&format!(
                "`self.conn` is moved out of the `var self` receiver while {operation} can fail"
            )) && refusals[0]
                .suggestions
                .iter()
                .any(|hint| hint.contains("`take()`")),
            "{body}: {:#?}",
            refusals[0]
        );
    }

    for body in [
        // Restored before anything that can fail.
        "let old = self.conn; self.conn = Conn { fd: divisor }; old.fd * 2",
        // `take()` leaves `None` behind: the receiver stays whole.
        "match self.spare.take() { .Some(conn) => conn.fd / divisor, .None => 0 }",
        // Fields stay in place.
        "self.count += 1; self.conn.fd = self.conn.fd + work(); self.count",
        // A runtime operation fails only where its contract says so.
        "let conn = self.conn; self.count = self.items.len(); self.conn = conn; 0",
        "let conn = self.conn; let label = f\"{divisor}\"; self.count = label.len(); self.conn = conn; 0",
        "let spare = self.spare.is_some(); let conn = self.conn; self.conn = conn; if spare { 1 } else { 0 }",
        "let conn = self.conn; self.items.clear(); self.conn = conn; 0",
    ] {
        let output = check_source(&format!(
            "{declarations} impl Touch for Holder {{ fn touch(var self, divisor: i64) -> i64 {{ {body} }} }}"
        ));
        assert!(output.errors.is_empty(), "{body}: {:#?}", output.errors);
    }
}

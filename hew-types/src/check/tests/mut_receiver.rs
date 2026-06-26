#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

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
                Some(self.val)
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
        .fn_sigs
        .get("Counter::next")
        .expect("Counter::next must be registered in fn_sigs");
    assert!(
        sig.requires_mutable_receiver,
        "fn_sigs[Counter::next].requires_mutable_receiver must be true for `var self`",
    );
    let td = output
        .type_defs
        .get("Counter")
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
        .fn_sigs
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
        .type_defs
        .get("VecIter")
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

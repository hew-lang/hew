//! The §1.1 class table over real programs.
//!
//! Each fixture in `repros/ladder/p1/` is type-checked and its bindings' types
//! are looked up in `TypeCheckOutput::type_facts`. These assert **facts**, not
//! diagnostics: every one of these programs is accepted, and what the ladder
//! pins is the `(class, clone, send)` verdict the checker publishes for it.

mod common;

use std::fs;
use std::path::PathBuf;

use common::{repo_root, typecheck};
use hew_types::value_class::ClassContext;
use hew_types::{
    BuiltinType, CloneKind, ResolvedTy, SendFact, TypeCheckOutput, TypeFacts, TypeInstanceKey,
    ValueClass,
};

fn fixture(name: &str) -> PathBuf {
    repo_root().join("repros/ladder/p1").join(name)
}

fn facts_of(name: &str) -> TypeCheckOutput {
    let path = fixture(name);
    let source = fs::read_to_string(&path).unwrap_or_else(|error| {
        panic!("fixture `{}` must be readable: {error}", path.display());
    });
    let output = typecheck(&source);
    assert!(
        output.errors.is_empty(),
        "fixture `{name}` must type-check: {:#?}",
        output.errors
    );
    assert!(
        !output.type_facts.is_empty(),
        "an accepted program must publish a class table"
    );
    output
}

fn row(output: &TypeCheckOutput, ty: &ResolvedTy) -> TypeFacts {
    *output
        .type_facts
        .get(&TypeInstanceKey(ty.clone()))
        .unwrap_or_else(|| {
            panic!(
                "`{}` has no row in the published class table",
                ty.user_facing()
            )
        })
}

/// Find the one published row whose key satisfies `predicate`.
fn row_matching(
    output: &TypeCheckOutput,
    what: &str,
    predicate: impl Fn(&ResolvedTy) -> bool,
) -> TypeFacts {
    let mut hits = output
        .type_facts
        .iter()
        .filter(|(key, _)| predicate(&key.0));
    let (_, facts) = hits
        .next()
        .unwrap_or_else(|| panic!("no published row for {what}"));
    *facts
}

/// §1.1: integers, floats, `Bool`, `Char`, `Unit`, `Never` and `Duration` are
/// `BitCopy`/`Bits`, so none of them carries an ownership obligation.
#[test]
fn scalar_bindings_are_bitcopy() {
    let output = facts_of("class_scalars.hew");
    for ty in [
        ResolvedTy::I64,
        ResolvedTy::Bool,
        ResolvedTy::F64,
        ResolvedTy::Char,
        ResolvedTy::Duration,
    ] {
        let facts = row(&output, &ty);
        assert_eq!(
            (ValueClass::BitCopy, CloneKind::Bits, SendFact::Known(true)),
            (facts.class, facts.clone, facts.send),
            "class table row for `{}`",
            ty.user_facing()
        );
    }
}

/// §1.1: `String` is `CowValue`/`Retain`, so §1.2 gives every string value an
/// `Owned` obligation - one consuming use per path.
#[test]
fn string_bindings_are_cow_values_with_a_retain_path() {
    let output = facts_of("class_string.hew");
    let facts = row(&output, &ResolvedTy::String);
    assert_eq!(
        (
            ValueClass::CowValue,
            CloneKind::Retain,
            SendFact::Known(true)
        ),
        (facts.class, facts.clone, facts.send)
    );
    // §1.2 maps this class to `Owned`: one consuming use per path. The
    // mapping itself is asserted in `hew-sir`, which owns `OwnKind`.
    assert_ne!(ValueClass::BitCopy, facts.class);
    assert_ne!(ValueClass::View, facts.class);
}

/// §1.1: `Bytes` shares the `String` row - a bytes mutator forks inside the
/// runtime, so the value itself is a retained heap carrier.
#[test]
fn bytes_bindings_are_cow_values_with_a_retain_path() {
    let output = facts_of("class_bytes.hew");
    let facts = row(&output, &ResolvedTy::Bytes);
    assert_eq!(
        (
            ValueClass::CowValue,
            CloneKind::Retain,
            SendFact::Known(true)
        ),
        (facts.class, facts.clone, facts.send)
    );
}

/// The row that settles the `is_copy`-versus-`of_ty` split: `(i64, i64)` is
/// `BitCopy` where the legacy `of_ty` said `CowValue`, and `(string, i64)` is
/// `CowValue`/`FieldWise` where `Ty::is_copy` said only "not Copy" and offered
/// no clone kind at all.
#[test]
fn tuples_follow_the_aggregate_rule_rather_than_either_old_predicate() {
    let output = facts_of("class_tuple.hew");

    let scalars = row(
        &output,
        &ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::I64]),
    );
    assert_eq!(
        (ValueClass::BitCopy, CloneKind::Bits),
        (scalars.class, scalars.clone)
    );
    assert_ne!(
        ValueClass::CowValue,
        scalars.class,
        "the legacy `of_ty` classed every tuple `CowValue`"
    );

    let mixed = row(
        &output,
        &ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]),
    );
    assert_eq!(
        (ValueClass::CowValue, CloneKind::FieldWise),
        (mixed.class, mixed.clone)
    );
    assert_ne!(
        CloneKind::Bits,
        mixed.clone,
        "a tuple holding a string has no bit-copy path"
    );
}

/// §1.1 `#[resource]` row. The program is accepted; what this pins is
/// `clone == None`, which is what makes rule 6b reject a `copy_value`.
#[test]
fn a_resource_declaration_has_no_clone_path() {
    let output = facts_of("class_resource_no_clone.hew");
    let facts = row_matching(
        &output,
        "the `Conn` resource",
        |ty| matches!(ty, ResolvedTy::Named { name, builtin: None, .. } if name == "Conn"),
    );
    assert_eq!(
        (ValueClass::AffineResource, CloneKind::None),
        (facts.class, facts.clone)
    );
    // Counterfactual: a field-wise clone path would make 6b admit a copy of a
    // value whose close obligation cannot be duplicated.
    assert_ne!(CloneKind::FieldWise, facts.clone);
}

/// §1.1 `Rc`/`Weak` row plus §6.3's send fact: an `Rc` has a retain path and is
/// single-owner, and its non-atomic count is why it is not `Send`.
#[test]
fn an_rc_is_an_affine_resource_that_retains_and_does_not_send() {
    let output = facts_of("class_rc_not_send.hew");
    let facts = row_matching(&output, "`Rc<i64>`", |ty| {
        matches!(
            ty,
            ResolvedTy::Named {
                builtin: Some(BuiltinType::Rc),
                args,
                ..
            } if args.as_slice() == [ResolvedTy::I64]
        )
    });
    assert_eq!(
        (
            ValueClass::AffineResource,
            CloneKind::Retain,
            SendFact::Known(false)
        ),
        (facts.class, facts.clone, facts.send)
    );
}

/// §1.1's marker correction: a pid never owns the actor, so `LocalPid` is
/// `BitCopy` and its drop frees nothing.
#[test]
fn a_local_pid_is_bitcopy() {
    let output = facts_of("class_pid_bitcopy.hew");
    let facts = row_matching(&output, "`LocalPid<Counter>`", |ty| {
        matches!(
            ty,
            ResolvedTy::Named {
                builtin: Some(BuiltinType::LocalPid),
                ..
            }
        )
    });
    assert_eq!(
        (ValueClass::BitCopy, CloneKind::Bits),
        (facts.class, facts.clone)
    );
    // Counterfactual: `BuiltinType::marker()` gave `Resource` before this
    // correction, which would class the pid `AffineResource` and give it a
    // scope-exit release it must not have.
    assert_ne!(ValueClass::AffineResource, facts.class);
}

/// §1.1 `TypeParam` row: an abstract parameter never reaches SIR, so it is an
/// error rather than a class. Its instantiation is an ordinary `BitCopy`.
#[test]
fn an_abstract_type_parameter_has_no_class() {
    let output = facts_of("class_typeparam_refused.hew");

    let refused = ValueClass::of_ty(
        &ResolvedTy::TypeParam {
            name: "T".to_string(),
        },
        &ClassContext::empty(),
    )
    .expect_err("an abstract parameter has no value class");
    assert!(
        matches!(refused, hew_types::ClassError::TypeParam { .. }),
        "the refusal must name the parameter row: {refused}"
    );

    let instantiated = row(&output, &ResolvedTy::I64);
    assert_eq!(ValueClass::BitCopy, instantiated.class);
}

/// The published table is closed under a row's own component types, so an
/// element's facts are available wherever its container's are.
#[test]
fn the_published_table_carries_a_row_for_every_component_type() {
    let output = facts_of("class_tuple.hew");
    for component in [ResolvedTy::String, ResolvedTy::I64] {
        assert!(
            output
                .type_facts
                .contains_key(&TypeInstanceKey(component.clone())),
            "`{}` is a component of a published tuple row",
            component.user_facing()
        );
    }
}

/// §1.1 has no default class, so a declaration the aggregate rule cannot
/// decide gets no row at all. An `indirect` enum is that case today: the
/// recursion is legal only because the payload is heap-boxed, and the
/// declaration facts carry the members rather than the box. Publishing the
/// join's bottom for it would say `BitCopy` — no owner under §1.2, and a
/// `copy_value` of the box pointer under §1.3.
#[test]
fn an_indirect_enum_publishes_no_row_rather_than_a_bit_copy_one() {
    let output = facts_of("class_indirect_enum_refused.hew");
    let recursive = output
        .type_facts
        .keys()
        .find(|key| matches!(&key.0, ResolvedTy::Named { name, .. } if name.ends_with("Tree")));
    assert!(
        recursive.is_none(),
        "a heap-boxed recursive enum must publish no class row, got {recursive:?}"
    );
}

/// The counterfactual in the same program: a non-recursive enum still has a
/// row, so the missing row above is about the cycle and not about user enums.
#[test]
fn a_non_recursive_enum_in_the_same_program_still_publishes_a_row() {
    let output = facts_of("class_indirect_enum_refused.hew");
    let facts = row_matching(
        &output,
        "the non-recursive enum `Colour`",
        |ty| matches!(ty, ResolvedTy::Named { name, .. } if name.ends_with("Colour")),
    );
    assert_eq!(
        (ValueClass::BitCopy, CloneKind::Bits),
        (facts.class, facts.clone)
    );
}

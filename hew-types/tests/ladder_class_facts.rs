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

/// §1.1's indirect-enum row, on a real program: the recursive occurrence is an
/// owning edge, so the declaration keeps its payload class and clones
/// field-wise. The payload here is a scalar and the class is still `CowValue`,
/// because the recursion is legal only behind a heap box.
#[test]
fn an_indirect_enum_publishes_its_payload_class_over_an_owning_edge() {
    let output = facts_of("class_indirect_enum_owning_edge.hew");
    let facts = row_matching(
        &output,
        "the recursive enum `Tree`",
        |ty| matches!(ty, ResolvedTy::Named { name, .. } if name.ends_with("Tree")),
    );
    assert_eq!(
        (ValueClass::CowValue, CloneKind::FieldWise),
        (facts.class, facts.clone)
    );
}

/// The negative control for the row above, stated as the class a
/// bottom-element cut published: §1.2 gives `BitCopy` no owner, §1.3 lets
/// `copy_value` duplicate it at `clone == Bits`, and §2.1 bit-copies it across
/// an actor heap, so a heap-boxed payload must never carry it.
#[test]
fn an_indirect_enum_is_never_published_bit_copyable() {
    let output = facts_of("class_indirect_enum_owning_edge.hew");
    let facts = row_matching(
        &output,
        "the recursive enum `Tree`",
        |ty| matches!(ty, ResolvedTy::Named { name, .. } if name.ends_with("Tree")),
    );
    assert_ne!(
        (ValueClass::BitCopy, CloneKind::Bits),
        (facts.class, facts.clone)
    );
}

/// The counterfactual in the same program: a non-recursive enum over the same
/// scalar payloads is bit-copyable, so the owning edge above is about the cycle
/// and not about user enums.
#[test]
fn a_non_recursive_enum_in_the_same_program_stays_bit_copyable() {
    let output = facts_of("class_indirect_enum_owning_edge.hew");
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

/// Type-check a program that lives outside `repros/ladder/p1/`, so a live
/// fixture can be asserted where it is rather than copied.
fn facts_of_path(relative: &str) -> TypeCheckOutput {
    let path = repo_root().join(relative);
    let source = fs::read_to_string(&path).unwrap_or_else(|error| {
        panic!("fixture `{}` must be readable: {error}", path.display());
    });
    let output = typecheck(&source);
    assert!(
        output.errors.is_empty(),
        "fixture `{relative}` must type-check: {:#?}",
        output.errors
    );
    output
}

/// Does `ty` name the declaration `name` at the arguments `args` describes?
fn named_at(ty: &ResolvedTy, name: &str, args: impl Fn(&[ResolvedTy]) -> bool) -> bool {
    matches!(
        ty,
        ResolvedTy::Named { name: actual, args: actual_args, .. }
            if actual.ends_with(name) && args(actual_args)
    )
}

/// §1.1: a declaration nested inside its own type argument is not recursion.
/// `Wrapper<T> { value: T }` never mentions `Wrapper` in its members, so no
/// argument a caller supplies can make it refuse - `Wrapper<Wrapper<i64>>` is
/// the aggregate over one `Wrapper<i64>` field, which is the aggregate over
/// one `i64`.
#[test]
fn a_declaration_nested_in_its_own_argument_is_not_recursion() {
    let output = facts_of("class_nested_instantiation.hew");
    let facts = row_matching(&output, "`Wrapper<Wrapper<i64>>`", |ty| {
        named_at(ty, "Wrapper", |args| {
            args.len() == 1 && named_at(&args[0], "Wrapper", |inner| inner == [ResolvedTy::I64])
        })
    });
    assert_eq!(
        (ValueClass::BitCopy, CloneKind::Bits),
        (facts.class, facts.clone)
    );
}

/// The same rule one member deeper: `Wrapper<Outer<i64>>` reaches `Wrapper<i64>`
/// through `Outer`'s member, and neither declaration mentions itself, so the
/// walk is finite and the row is published.
#[test]
fn a_declaration_reached_transitively_at_a_second_instantiation_is_not_recursion() {
    let output = facts_of("class_nested_instantiation.hew");
    let facts = row_matching(&output, "`Wrapper<Outer<i64>>`", |ty| {
        named_at(ty, "Wrapper", |args| {
            args.len() == 1 && named_at(&args[0], "Outer", |inner| inner == [ResolvedTy::I64])
        })
    });
    assert_eq!(
        (ValueClass::BitCopy, CloneKind::Bits),
        (facts.class, facts.clone)
    );
}

/// The live vertical-slice fixture that the name-keyed cut refused: the nested
/// `Maybe<Maybe<i64>>` the program builds has a published row.
#[test]
fn the_nested_generic_enum_fixture_publishes_its_nested_row() {
    let output = facts_of_path("tests/vertical-slice/accept/generic_enum_nested_option.hew");
    let facts = row_matching(&output, "`Maybe<Maybe<i64>>`", |ty| {
        named_at(ty, "Maybe", |args| {
            args.len() == 1 && named_at(&args[0], "Maybe", |inner| inner == [ResolvedTy::I64])
        })
    });
    assert_eq!(
        (ValueClass::BitCopy, CloneKind::Bits),
        (facts.class, facts.clone)
    );
}

/// The second live fixture: `Pair<Pair<i64, i64>, string>` nests a generic
/// record inside itself as an argument, and its row carries the `string`
/// leaf's obligation rather than being refused.
#[test]
fn the_nested_generic_record_fixture_publishes_its_nested_row() {
    let output = facts_of_path("tests/hew/generic_record_clone_test.hew");
    let facts = row_matching(&output, "`Pair<Pair<i64, i64>, string>`", |ty| {
        named_at(ty, "Pair", |args| {
            args.len() == 2
                && named_at(&args[0], "Pair", |inner| {
                    inner == [ResolvedTy::I64, ResolvedTy::I64]
                })
                && args[1] == ResolvedTy::String
        })
    });
    assert_eq!(
        (ValueClass::CowValue, CloneKind::FieldWise),
        (facts.class, facts.clone)
    );
}

/// Every published row whose key names a declaration ending in `name`.
fn rows_named(output: &TypeCheckOutput, name: &str) -> Vec<TypeFacts> {
    output
        .type_facts
        .iter()
        .filter(|(key, _)| named_at(&key.0, name, |_| true))
        .map(|(_, facts)| *facts)
        .collect()
}

/// §1.1: an `#[opaque]` declaration with no ownership marker is refused, and a
/// name collision with the builtin table is not a class. Two identical fieldless
/// `#[opaque]` declarations, one of whose names is in `builtin_types!`, get one
/// verdict.
#[test]
fn a_fieldless_opaque_declaration_is_refused_whether_or_not_its_name_is_a_builtin() {
    let output = facts_of("class_opaque_handle_shadowing_a_builtin.hew");
    assert_eq!(
        (Vec::new(), Vec::new()),
        (
            rows_named(&output, "Location"),
            rows_named(&output, "Handle")
        ),
        "an opaque handle with no marker publishes no row, whatever its name"
    );
}

/// The counterfactual in the same program: a user declaration that shadows a
/// builtin name and carries members keeps its own class rather than the builtin
/// row's. `NodeId` is `BitCopy` in the builtin table and `CowValue` here.
#[test]
fn a_user_declaration_shadowing_a_builtin_name_keeps_its_own_class() {
    let output = facts_of("class_opaque_handle_shadowing_a_builtin.hew");
    let rows = rows_named(&output, "NodeId");
    let facts = *rows.first().expect("`NodeId` has a published row");
    assert_eq!(
        (ValueClass::CowValue, CloneKind::FieldWise),
        (facts.class, facts.clone)
    );
}

/// §1.1: the two fieldless `#[opaque]` stdlib channel handles carry a close
/// method, so both are `AffineResource` with no clone. Their verdict comes from
/// the builtin row, which the declaration lookup must leave reachable for a
/// name it holds no user declaration of.
#[test]
fn the_channel_handles_are_affine_resources_with_no_clone() {
    let output = facts_of("class_channel_handles.hew");
    for half in ["Sender", "Receiver"] {
        let rows = rows_named(&output, half);
        let facts = *rows
            .first()
            .unwrap_or_else(|| panic!("`channel.{half}` has a published row"));
        assert_eq!(
            (ValueClass::AffineResource, CloneKind::None),
            (facts.class, facts.clone),
            "class table row for `channel.{half}`"
        );
    }
}

/// §1.1's indirect-enum row on a generic declaration: `Nest<T>` mentions itself
/// at its own instantiation, so the recursive occurrence is an owning edge and
/// `Nest<i64>` keeps its payload class behind the heap box - `CowValue` with a
/// `FieldWise` clone, never `BitCopy`, which §1.2 would give no owner.
///
/// The program is inline rather than a `repros/ladder/p1/` fixture because the
/// corpus sweep's `hew check` runs codegen-front validation, which refuses a
/// generic `indirect enum` today: the instantiation classifies
/// `DropClass::IndirectEnum` in `hew-mir/src/ownership.rs` and still reaches
/// codegen as `DropKind::Resource` with no drop function (#3298). The class
/// verdict this test asserts is the checker's and is unaffected by that defect;
/// the sweep has no exclusion for a checker-level accept fixture, and inventing
/// one would be a second expected-failures list.
#[test]
fn a_generic_indirect_enum_publishes_over_the_owning_edge() {
    let output = typecheck(
        r"
indirect enum Nest<T> {
    Leaf(T);
    More(Nest<T>);
}

fn main() -> i64 {
    let n: Nest<i64> = Nest.More(Nest.Leaf(1));
    let _ = n;
    0
}
",
    );
    assert!(
        output.errors.is_empty(),
        "the generic indirect enum must type-check: {:#?}",
        output.errors
    );
    let facts = row_matching(&output, "`Nest<i64>`", |ty| {
        named_at(ty, "Nest", |args| args == [ResolvedTy::I64])
    });
    assert_eq!(
        (ValueClass::CowValue, CloneKind::FieldWise),
        (facts.class, facts.clone)
    );
    assert_ne!(
        (ValueClass::BitCopy, CloneKind::Bits),
        (facts.class, facts.clone),
        "a heap-boxed payload must never be published bit-copyable"
    );
}

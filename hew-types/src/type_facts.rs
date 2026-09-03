//! Capability facts on the class table (`docs/internal/ir-ladder.md` §6.3).
//!
//! The checker decides once per substituted type and publishes
//! `TypeCheckOutput.type_facts`, keyed by [`TypeInstanceKey`] (§6.2). SIR rules
//! 5 and 6, `TargetLayout`, glue emission and collection descriptors all read
//! this table; nothing downstream re-asks.

use crate::resolved_ty::ResolvedTy;
use crate::value_class::{ClassContext, ClassError, ValueClass};

/// What a `copy_value` of this type costs, per §1.1's `clone` column.
///
/// Class alone does not decide copy legality: `Rc<T>` and `Generator` are both
/// `AffineResource`, yet `Rc` has a retain path and a generator has none.
/// Rule 6b reads this, not the class.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CloneKind {
    /// Bit copy; no runtime call.
    Bits,
    /// Refcount increment.
    Retain,
    /// Fresh allocation, contents copied bit-wise.
    DeepCopy,
    /// Fresh structure, members copied through their own glue.
    FieldWise,
    /// No copy path exists; a `copy_value` of this type is refused (rule 6b).
    None,
}

/// The `Send` verdict for one type instance.
///
/// **The wrong reading is made unrepresentable** (§6.3): this is not a `bool`,
/// and every `Closure`-keyed row carries [`Self::DeferredToClosureFacts`], so a
/// consumer asking `type_facts[closure_key].send` for a yes/no gets a value it
/// cannot use without going to `closure_facts`. Two closures with identical
/// capture types and opposite capture modes are one type instance and need
/// opposite facts, so the mode-agnostic answer is always wrong for a closure.
///
/// This type deliberately has no `Deref`, no `Into<bool>`, no `unwrap_or` and
/// no other accessor that would hand back a plain `bool`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SendFact {
    Known(bool),
    /// A `Closure` row: the fact lives in `closure_facts`, keyed per closure
    /// expression rather than per type.
    DeferredToClosureFacts,
}

/// The key of every per-type fact table.
///
/// **Structural, not nominal** (§6.2). The nominal spelling
/// `{ template: NominalId, type_args }` is withdrawn: `nominal_instance()`
/// returns `Some` only for `Named { builtin: None }`, so `Tuple`, `Array`,
/// `Slice`, `Function`, `Closure`, `Pointer`, `Borrow`, `TraitObject` and
/// `Task` would have no key at all while §1.1 classes every one of them.
///
/// The key is a *type*, never a name: [`crate::mangle::mangle_resolved_ty`]
/// renders a symbol from it and is never a lookup key.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeInstanceKey(pub ResolvedTy);

impl TypeInstanceKey {
    #[must_use]
    pub const fn ty(&self) -> &ResolvedTy {
        &self.0
    }
}

impl From<ResolvedTy> for TypeInstanceKey {
    fn from(ty: ResolvedTy) -> Self {
        Self(ty)
    }
}

/// The authority for one substituted type's ownership and capability facts.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeFacts {
    pub class: ValueClass,
    pub clone: CloneKind,
    pub send: SendFact,
    pub hash: bool,
    pub eq: bool,
}

impl TypeFacts {
    /// Build the facts for one substituted type.
    ///
    /// `send`, `hash` and `eq` are decided by their own checker authorities and
    /// handed in; `class` and `clone` come from the §1.1 table.
    ///
    /// # Errors
    ///
    /// Returns [`ClassError`] when §1.1 refuses the type. There is no default
    /// row: a type with no class gets no entry, and a consumer fails closed on
    /// the absence.
    pub fn of_type(
        ty: &ResolvedTy,
        decls: &ClassContext<'_>,
        send: SendFact,
        hash: bool,
        eq: bool,
    ) -> Result<Self, ClassError> {
        let (class, clone) = crate::value_class::classify_ty(ty, decls)?;
        let send = match ty {
            ResolvedTy::Closure { .. } => SendFact::DeferredToClosureFacts,
            // MARKED SHORTCUT - a trait object's send fact is `false` here.
            // WHY: §6.3 makes it "the bound list contains `Send`", and that is
            // sound *only* because §1.1's `CoerceToDynTrait` wall refuses a
            // coercion into a `+ Send` object whose concrete is not `Send`. The
            // wall does not exist yet, so the bound list would let a
            // `dyn ... + Send` over an `Rc`-holding concrete be shared between
            // actors and race a non-atomic count.
            // WHEN: the coercion wall lands with the closure send facts.
            // WHAT: this constructor takes the caller's decided fact, which is
            // then the bound list §6.3 names.
            ResolvedTy::TraitObject { .. } => SendFact::Known(false),
            _ => send,
        };
        Ok(Self {
            class,
            clone,
            send,
            hash,
            eq,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use super::{CloneKind, SendFact, TypeFacts, TypeInstanceKey};
    use crate::builtin_type::{builtin_types, BuiltinType};
    use crate::resolved_ty::{ResolvedTraitBound, ResolvedTy};
    use crate::value_class::{
        ClassContext, ClassError, DeclarationMarker, DeclaredType, ValueClass,
    };

    fn named(name: &str, builtin: Option<BuiltinType>, args: Vec<ResolvedTy>) -> ResolvedTy {
        ResolvedTy::Named {
            name: name.to_string(),
            args,
            builtin,
            is_opaque: false,
        }
    }

    /// Declarations the §1.1 Aggregate rule needs for the cases below.
    fn declarations() -> BTreeMap<String, DeclaredType> {
        let mut decls = BTreeMap::new();
        decls.insert(
            "Conn".to_string(),
            DeclaredType {
                marker: DeclarationMarker::Resource,
                type_params: vec![],
                members: vec![ResolvedTy::I64],
            },
        );
        decls.insert(
            "Ticket".to_string(),
            DeclaredType {
                marker: DeclarationMarker::Linear,
                type_params: vec![],
                members: vec![ResolvedTy::I64],
            },
        );
        decls.insert(
            "Point".to_string(),
            DeclaredType {
                marker: DeclarationMarker::None,
                type_params: vec![],
                members: vec![ResolvedTy::I64, ResolvedTy::I64],
            },
        );
        decls.insert(
            "Label".to_string(),
            DeclaredType {
                marker: DeclarationMarker::None,
                type_params: vec![],
                members: vec![ResolvedTy::String, ResolvedTy::I64],
            },
        );
        // `std/failure.hew::CrashInfo { code: i64, message: string }`.
        decls.insert(
            "CrashInfo".to_string(),
            DeclaredType {
                marker: DeclarationMarker::None,
                type_params: vec![],
                members: vec![ResolvedTy::I64, ResolvedTy::String],
            },
        );
        // `std/failure.hew::CrashNotification { actor_id: u64, kind: CrashKind }`.
        decls.insert(
            "CrashNotification".to_string(),
            DeclaredType {
                marker: DeclarationMarker::None,
                type_params: vec![],
                members: vec![
                    ResolvedTy::U64,
                    named("CrashKind", Some(BuiltinType::CrashKind), vec![]),
                ],
            },
        );
        decls
    }

    fn facts(ty: &ResolvedTy) -> (ValueClass, CloneKind) {
        let decls = declarations();
        let context = ClassContext::new(&decls);
        crate::value_class::classify_ty(ty, &context)
            .unwrap_or_else(|error| panic!("§1.1 refused `{ty:?}`: {error}"))
    }

    fn conn() -> ResolvedTy {
        named("Conn", None, vec![])
    }

    fn rc(inner: ResolvedTy) -> ResolvedTy {
        named("Rc", Some(BuiltinType::Rc), vec![inner])
    }

    fn vec_of(element: ResolvedTy) -> ResolvedTy {
        named("Vec", Some(BuiltinType::Vec), vec![element])
    }

    fn closure_over(captures: Vec<ResolvedTy>) -> ResolvedTy {
        ResolvedTy::Closure {
            params: vec![],
            ret: Box::new(ResolvedTy::Unit),
            captures,
        }
    }

    /// §1.1's own test sentence: every `ResolvedTy` arm, asserting
    /// `(class, clone)`.
    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "one case per ResolvedTy arm is the point of the table test"
    )]
    fn every_resolved_ty_arm_has_the_ladder_class_and_clone() {
        let cases: Vec<(ResolvedTy, ValueClass, CloneKind)> = vec![
            (ResolvedTy::I8, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::I16, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::I32, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::I64, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::U8, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::U16, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::U32, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::U64, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::Isize, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::Usize, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::F32, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::F64, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::Bool, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::Char, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::Duration, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::Unit, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::Never, ValueClass::BitCopy, CloneKind::Bits),
            (ResolvedTy::String, ValueClass::CowValue, CloneKind::Retain),
            (ResolvedTy::Bytes, ValueClass::CowValue, CloneKind::Retain),
            (
                ResolvedTy::CancellationToken,
                ValueClass::AffineResource,
                CloneKind::None,
            ),
            (
                ResolvedTy::Slice(Box::new(ResolvedTy::I64)),
                ValueClass::View,
                CloneKind::Bits,
            ),
            (
                ResolvedTy::Pointer {
                    is_mutable: false,
                    pointee: Box::new(ResolvedTy::I64),
                },
                ValueClass::View,
                CloneKind::Bits,
            ),
            (
                ResolvedTy::Borrow {
                    pointee: Box::new(ResolvedTy::I64),
                },
                ValueClass::View,
                CloneKind::Bits,
            ),
            (
                ResolvedTy::Function {
                    params: vec![],
                    ret: Box::new(ResolvedTy::Unit),
                },
                ValueClass::PersistentShare,
                CloneKind::Retain,
            ),
            (
                ResolvedTy::TraitObject {
                    traits: vec![ResolvedTraitBound {
                        trait_name: "Show".to_string(),
                        args: vec![],
                        assoc_bindings: vec![],
                    }],
                },
                ValueClass::PersistentShare,
                CloneKind::Retain,
            ),
            (
                closure_over(vec![ResolvedTy::I64]),
                ValueClass::PersistentShare,
                CloneKind::Retain,
            ),
            (
                ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::I64]),
                ValueClass::BitCopy,
                CloneKind::Bits,
            ),
            (
                ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]),
                ValueClass::CowValue,
                CloneKind::FieldWise,
            ),
            (
                ResolvedTy::Array(Box::new(ResolvedTy::I64), 4),
                ValueClass::BitCopy,
                CloneKind::Bits,
            ),
            (
                ResolvedTy::Array(Box::new(conn()), 3),
                ValueClass::AffineResource,
                CloneKind::None,
            ),
            (
                ResolvedTy::Task(Box::new(ResolvedTy::I64)),
                ValueClass::Linear,
                CloneKind::None,
            ),
            (conn(), ValueClass::AffineResource, CloneKind::None),
            (
                named("Ticket", None, vec![]),
                ValueClass::Linear,
                CloneKind::None,
            ),
            (
                named("Point", None, vec![]),
                ValueClass::BitCopy,
                CloneKind::Bits,
            ),
            (
                named("Label", None, vec![]),
                ValueClass::CowValue,
                CloneKind::FieldWise,
            ),
        ];
        for (ty, class, clone) in cases {
            assert_eq!((class, clone), facts(&ty), "class table row for `{ty:?}`");
        }

        // `TypeParam` is an error, not a class: the instance service
        // substitutes before SIR sees it.
        let decls = declarations();
        let context = ClassContext::new(&decls);
        assert_eq!(
            ValueClass::of_ty(
                &ResolvedTy::TypeParam {
                    name: "T".to_string()
                },
                &context
            ),
            Err(ClassError::TypeParam {
                name: "T".to_string()
            })
        );
    }

    /// §1.1's own test sentence: **every `BuiltinType` variant**, in a `match`
    /// with no wildcard arm, so a new variant is a compile error rather than a
    /// silent `Unknown`.
    #[test]
    #[expect(
        clippy::match_same_arms,
        reason = "distinct §1.1 rows that agree for these arguments must stay distinct arms"
    )]
    fn every_builtin_type_variant_has_the_ladder_class_and_clone() {
        let decls = declarations();
        let context = ClassContext::new(&decls);

        for info in builtin_types() {
            // Two type arguments cover every builtin's arity in this table;
            // extra arguments are ignored by the rows that do not read them.
            let ty = named(
                info.canonical_name,
                Some(info.kind),
                match info.arity {
                    0 => vec![],
                    1 => vec![ResolvedTy::I64],
                    _ => vec![ResolvedTy::I64, ResolvedTy::String],
                },
            );
            let expected: Option<(ValueClass, CloneKind)> = match info.kind {
                BuiltinType::SupervisorPool
                | BuiltinType::ChildRef
                | BuiltinType::NodeId
                | BuiltinType::Location
                | BuiltinType::RemotePid
                | BuiltinType::MonitorId
                | BuiltinType::DownTarget
                | BuiltinType::DownReason
                | BuiltinType::DownNotification
                | BuiltinType::Instant
                | BuiltinType::Unit
                | BuiltinType::Duration
                | BuiltinType::Range
                | BuiltinType::Trap
                | BuiltinType::TimeoutError
                | BuiltinType::CrashAction
                | BuiltinType::CrashKind
                | BuiltinType::SendError
                | BuiltinType::AskError
                | BuiltinType::LookupError
                | BuiltinType::RecvError
                | BuiltinType::LinkError
                | BuiltinType::MonitorError
                | BuiltinType::CloseError
                | BuiltinType::LocalPid
                | BuiltinType::HewActor => Some((ValueClass::BitCopy, CloneKind::Bits)),
                // `Option<i64>` / `Result<i64, string>` join their payloads.
                BuiltinType::Option => Some((ValueClass::BitCopy, CloneKind::Bits)),
                BuiltinType::Result => Some((ValueClass::CowValue, CloneKind::FieldWise)),
                // A collection's buffer is heap, so it is never `BitCopy`.
                BuiltinType::Vec | BuiltinType::VecIter | BuiltinType::HashSet => {
                    Some((ValueClass::CowValue, CloneKind::DeepCopy))
                }
                BuiltinType::HashMap | BuiltinType::HashMapIter => {
                    Some((ValueClass::CowValue, CloneKind::FieldWise))
                }
                BuiltinType::CrashInfo => Some((ValueClass::CowValue, CloneKind::FieldWise)),
                BuiltinType::CrashNotification => Some((ValueClass::BitCopy, CloneKind::Bits)),
                BuiltinType::Rc | BuiltinType::Weak | BuiltinType::LambdaPid => {
                    Some((ValueClass::AffineResource, CloneKind::Retain))
                }
                BuiltinType::Generator
                | BuiltinType::AsyncGenerator
                | BuiltinType::StreamPair
                | BuiltinType::BoxedActor
                | BuiltinType::Duplex
                | BuiltinType::Sink
                | BuiltinType::Stream
                | BuiltinType::Sender
                | BuiltinType::Receiver
                | BuiltinType::HewDuplex
                | BuiltinType::HewSendHalf
                | BuiltinType::HewRecvHalf
                | BuiltinType::SendHalf
                | BuiltinType::RecvHalf
                | BuiltinType::LambdaActorHandle
                | BuiltinType::MonitorRef
                | BuiltinType::CancellationToken => {
                    Some((ValueClass::AffineResource, CloneKind::None))
                }
                BuiltinType::Task => Some((ValueClass::Linear, CloneKind::None)),
                // Never the type of a value.
                BuiltinType::Iterator | BuiltinType::ActorState | BuiltinType::MachineState => None,
            };
            match expected {
                Some(expected) => assert_eq!(
                    Ok(expected),
                    crate::value_class::classify_ty(&ty, &context),
                    "class table row for builtin `{}`",
                    info.canonical_name
                ),
                None => assert_eq!(
                    Err(ClassError::NotAValueType { builtin: info.kind }),
                    crate::value_class::classify_ty(&ty, &context).map(|facts| facts.0),
                    "`{}` must be unreachable for values",
                    info.canonical_name
                ),
            }
        }
    }

    /// §1.1's marker correction, stated as the property it buys: `marker()`
    /// and the class table agree by construction, so no consumer can read one
    /// and get the other's answer.
    #[test]
    fn builtin_marker_and_the_class_table_agree() {
        use crate::builtin_type::BuiltinTypeMarker;

        let decls = declarations();
        let context = ClassContext::new(&decls);
        for info in builtin_types() {
            let ty = named(
                info.canonical_name,
                Some(info.kind),
                match info.arity {
                    0 => vec![],
                    1 => vec![ResolvedTy::I64],
                    _ => vec![ResolvedTy::I64, ResolvedTy::String],
                },
            );
            let classed = ValueClass::of_ty(&ty, &context);
            // RECORDED EXCEPTIONS - `LocalPid` and `HewActor`.
            //
            // §1.1 gives both the BitCopy row and the class table above records
            // that verdict. Neither `marker()` can follow in this change:
            // `marker()` is the legacy lowering's input and the legacy route is
            // the parity oracle. Flipping `LocalPid` routes a
            // `Vec<LocalPid<_>>` element off its pointer ABI
            // (`hew-cli::run_e2e run_generic_vec_element_methods_roundtrip_ptr_abi`
            // panics "Vec layout-aware operation is not implemented") and moves
            // an elaborated-MIR baseline
            // (`hew-cli::funcupdate_mir_baselines funcupdate_reassign_elab_mir_matches_committed_baselines`).
            // Flipping `HewActor` additionally fails
            // `hew-hir/src/builtin_type_classes.rs:331`, which asserts a BitCopy
            // builtin registers no close method, in every test that seeds the
            // builtin class table.
            if matches!(info.kind, BuiltinType::LocalPid | BuiltinType::HewActor) {
                assert_eq!(Ok(ValueClass::BitCopy), classed);
                assert_eq!(BuiltinTypeMarker::Resource, info.kind.marker());
                continue;
            }
            match info.kind.marker() {
                BuiltinTypeMarker::BitCopy => assert_eq!(
                    Ok(ValueClass::BitCopy),
                    classed,
                    "`{}` carries marker BitCopy",
                    info.canonical_name
                ),
                BuiltinTypeMarker::Resource => assert_eq!(
                    Ok(ValueClass::AffineResource),
                    classed,
                    "`{}` carries marker Resource",
                    info.canonical_name
                ),
                // `Linear` is carried only by the two compiler-internal payload
                // carriers, which are never the type of a value.
                BuiltinTypeMarker::Linear => assert_eq!(
                    Err(ClassError::NotAValueType { builtin: info.kind }),
                    classed,
                    "`{}` carries marker Linear",
                    info.canonical_name
                ),
                BuiltinTypeMarker::None => {}
            }
        }
    }

    /// The four cases the §10 exit gate names by hand.
    #[test]
    fn exit_gate_aggregate_cases_join_through_their_elements() {
        assert_eq!(
            (ValueClass::AffineResource, CloneKind::None),
            facts(&vec_of(conn())),
            "Vec<Conn>"
        );
        assert_eq!(
            (ValueClass::AffineResource, CloneKind::FieldWise),
            facts(&vec_of(rc(ResolvedTy::I64))),
            "Vec<Rc<i64>>"
        );
        assert_eq!(
            (ValueClass::PersistentShare, CloneKind::Retain),
            facts(&closure_over(vec![ResolvedTy::I64])),
            "closure capturing an i64"
        );
        assert_eq!(
            (ValueClass::AffineResource, CloneKind::Retain),
            facts(&closure_over(vec![conn()])),
            "closure capturing a Conn"
        );
    }

    /// The counterfactual for the two cases above that fail if the Aggregate
    /// rule is not applied through the element and capture classes.
    #[test]
    fn a_flat_collection_or_closure_row_would_be_wrong() {
        assert_ne!(
            (ValueClass::CowValue, CloneKind::DeepCopy),
            facts(&vec_of(conn())),
            "a flat `Vec` row would class Vec<Conn> CowValue/DeepCopy"
        );
        assert_ne!(
            (ValueClass::PersistentShare, CloneKind::Retain),
            facts(&closure_over(vec![conn()])),
            "a flat `Closure` row would class a Conn-capturing closure PersistentShare"
        );
    }

    /// The class rule refuses rather than guessing when it has no declaration.
    #[test]
    fn an_unknown_declaration_is_refused_not_defaulted() {
        let context = ClassContext::empty();
        assert_eq!(
            Err(ClassError::UnknownDeclaration {
                name: "Conn".to_string()
            }),
            ValueClass::of_ty(&conn(), &context)
        );
    }

    /// §6.2's own test sentence: every one of these has no `NominalId`, so a
    /// nominal key could not express them; the structural key hits for all
    /// five.
    #[test]
    fn type_facts_lookups_hit_for_five_non_nominal_types() {
        let decls = declarations();
        let context = ClassContext::new(&decls);
        let instances = vec![
            ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]),
            ResolvedTy::Array(Box::new(conn()), 3),
            ResolvedTy::TraitObject {
                traits: vec![ResolvedTraitBound {
                    trait_name: "Show".to_string(),
                    args: vec![],
                    assoc_bindings: vec![],
                }],
            },
            closure_over(vec![ResolvedTy::I64]),
            ResolvedTy::Task(Box::new(ResolvedTy::I64)),
        ];

        let mut table = std::collections::BTreeMap::new();
        for ty in &instances {
            // None of the five produces a nominal instance, which is the whole
            // argument for the structural key.
            assert!(ty.nominal_instance().is_none(), "`{ty:?}` has no NominalId");
            let facts = TypeFacts::of_type(ty, &context, SendFact::Known(true), false, false)
                .expect("§1.1 classes every one of these");
            table.insert(TypeInstanceKey(ty.clone()), facts);
        }

        for ty in &instances {
            assert!(
                table.contains_key(&TypeInstanceKey(ty.clone())),
                "structural lookup for `{ty:?}`"
            );
        }
    }

    /// §6.3: `send` is unrepresentable as a plain `bool` for a `Closure` key.
    #[test]
    fn a_closure_row_defers_its_send_fact() {
        let decls = declarations();
        let context = ClassContext::new(&decls);
        let closure = closure_over(vec![ResolvedTy::I64]);
        // Even when the caller hands in a decided fact, the closure row refuses
        // it: the mode-agnostic answer is wrong for a `BorrowMut` capture.
        let facts = TypeFacts::of_type(&closure, &context, SendFact::Known(true), false, false)
            .expect("a closure has a class");
        assert_eq!(SendFact::DeferredToClosureFacts, facts.send);

        // A consumer cannot read a `bool` out of it: the only way to a yes/no
        // is an explicit match that must name the deferral arm.
        let read_as_bool = match facts.send {
            SendFact::Known(value) => Some(value),
            SendFact::DeferredToClosureFacts => None,
        };
        assert_eq!(None, read_as_bool);
    }

    /// An ordinary row keeps the decided fact, so the deferral is not a
    /// blanket refusal.
    #[test]
    fn an_ordinary_row_keeps_a_known_send_fact() {
        let decls = declarations();
        let context = ClassContext::new(&decls);
        let facts = TypeFacts::of_type(
            &ResolvedTy::String,
            &context,
            SendFact::Known(true),
            false,
            true,
        )
        .expect("a string has a class");
        assert_eq!(SendFact::Known(true), facts.send);
    }

    /// A trait object's send fact is not the bound list yet: §6.3 makes that
    /// reading sound only through §1.1's `CoerceToDynTrait` wall, which has not
    /// landed, so the row publishes the fail-closed verdict rather than one a
    /// consumer could act on unsoundly.
    #[test]
    fn a_trait_object_row_does_not_publish_a_wall_less_send_fact() {
        let decls = declarations();
        let context = ClassContext::new(&decls);
        let dyn_show = ResolvedTy::TraitObject {
            traits: vec![ResolvedTraitBound {
                trait_name: "Show".to_string(),
                args: vec![],
                assoc_bindings: vec![],
            }],
        };
        let facts = TypeFacts::of_type(&dyn_show, &context, SendFact::Known(true), false, false)
            .expect("a trait object has a class");
        assert_eq!(SendFact::Known(false), facts.send);
    }

    /// Declarations that reach themselves, for the recursion cases below.
    ///
    /// `Tree` is the shape of the shipped `indirect enum` fixture and `ResTree`
    /// is the same shape over a resource payload. `Pair<T>` mentions one fixed
    /// instantiation of itself, so the walk reaches the declaration at an
    /// instantiation it did not enter.
    fn recursive_declarations() -> BTreeMap<String, DeclaredType> {
        let mut decls = declarations();
        decls.insert(
            "Tree".to_string(),
            DeclaredType {
                marker: DeclarationMarker::None,
                type_params: vec![],
                members: vec![
                    ResolvedTy::I64,
                    named("Tree", None, vec![]),
                    named("Tree", None, vec![]),
                ],
            },
        );
        decls.insert(
            "ResTree".to_string(),
            DeclaredType {
                marker: DeclarationMarker::None,
                type_params: vec![],
                members: vec![conn(), named("ResTree", None, vec![])],
            },
        );
        decls.insert(
            "Pair".to_string(),
            DeclaredType {
                marker: DeclarationMarker::None,
                type_params: vec!["T".to_string()],
                members: vec![
                    ResolvedTy::TypeParam {
                        name: "T".to_string(),
                    },
                    named("Pair", None, vec![conn()]),
                ],
            },
        );
        decls
    }

    /// §1.1's indirect-enum row: the recursive occurrence is an owning edge,
    /// so the declaration keeps its payload class and clones field-wise. The
    /// payload here is a scalar, and the answer is still `CowValue` because the
    /// recursion is only legal behind a heap box.
    #[test]
    fn a_self_recursive_declaration_keeps_its_payload_class_over_an_owning_edge() {
        let decls = recursive_declarations();
        let context = ClassContext::new(&decls);
        assert_eq!(
            Ok((ValueClass::CowValue, CloneKind::FieldWise)),
            crate::value_class::classify_ty(&named("Tree", None, vec![]), &context)
        );
    }

    /// The negative control for the row above, stated as the class the
    /// bottom-element cut used to publish: §1.2 gives `BitCopy` no owner, §1.3
    /// lets `copy_value` duplicate it at `clone == Bits`, and §2.1 bit-copies
    /// it across an actor heap, so a heap-boxed payload must never carry it.
    #[test]
    fn a_self_recursive_declaration_is_never_bit_copyable() {
        let decls = recursive_declarations();
        let context = ClassContext::new(&decls);
        let facts = crate::value_class::classify_ty(&named("Tree", None, vec![]), &context)
            .expect("an indirect enum has a class");
        assert_ne!((ValueClass::BitCopy, CloneKind::Bits), facts);
        assert_eq!(OwnershipObligation::Owned, obligation_of(facts.0));
    }

    /// The payload decides which owning class: a recursive declaration holding
    /// a resource is `AffineResource`, not the box's own `CowValue`, and its
    /// clone column collapses to `None` because the resource has no clone.
    #[test]
    fn a_recursive_declaration_over_a_resource_payload_is_affine() {
        let decls = recursive_declarations();
        let context = ClassContext::new(&decls);
        assert_eq!(
            Ok((ValueClass::AffineResource, CloneKind::None)),
            crate::value_class::classify_ty(&named("ResTree", None, vec![]), &context)
        );
    }

    /// The counterfactual for the rows above: a declaration whose members do
    /// not reach it takes no owning edge, so the heap floor is about the cycle
    /// and not about user declarations in general.
    #[test]
    fn a_non_recursive_declaration_over_the_same_members_still_classes() {
        let decls = recursive_declarations();
        let context = ClassContext::new(&decls);
        assert_eq!(
            Ok((ValueClass::AffineResource, CloneKind::None)),
            crate::value_class::classify_ty(&conn(), &context)
        );
        assert_eq!(
            Ok((ValueClass::BitCopy, CloneKind::Bits)),
            crate::value_class::classify_ty(&named("Point", None, vec![]), &context)
        );
    }

    /// The cut is keyed by the instantiation, so the instantiation the walk
    /// actually entered takes the owning edge and keeps its payload class.
    #[test]
    fn the_entered_instantiation_takes_the_owning_edge() {
        let decls = recursive_declarations();
        let context = ClassContext::new(&decls);
        assert_eq!(
            Ok((ValueClass::AffineResource, CloneKind::None)),
            crate::value_class::classify_ty(&named("Pair", None, vec![conn()]), &context)
        );
    }

    /// A cycle that reaches the declaration at an instantiation the walk did
    /// not enter has no finite fixpoint to join. Reading the entered
    /// instantiation's edge there would class `Pair<i64>` as the box's
    /// `CowValue` while the `Pair<Conn>` it holds owes a destructor, so this
    /// refuses instead.
    #[test]
    fn a_cycle_through_a_second_instantiation_refuses() {
        let decls = recursive_declarations();
        let context = ClassContext::new(&decls);
        assert_eq!(
            Err(ClassError::RecursiveInstantiation {
                name: "Pair".to_string()
            }),
            crate::value_class::classify_ty(&named("Pair", None, vec![ResolvedTy::I64]), &context)
        );
    }

    /// A classed recursive type publishes a row like any other aggregate: the
    /// missing-key contract is for types §1.1 refuses, and a legal `indirect`
    /// enum is not one of them.
    #[test]
    fn a_classed_recursive_type_publishes_its_row() {
        let decls = recursive_declarations();
        let context = ClassContext::new(&decls);
        let facts = TypeFacts::of_type(
            &named("Tree", None, vec![]),
            &context,
            SendFact::Known(true),
            false,
            false,
        )
        .expect("an indirect enum publishes a row");
        assert_eq!(ValueClass::CowValue, facts.class);
        assert_eq!(CloneKind::FieldWise, facts.clone);
    }

    /// What §1.2 owes for a class, as this module's tests read it.
    #[derive(Debug, PartialEq, Eq)]
    enum OwnershipObligation {
        None,
        Owned,
    }

    /// §1.2's kind table, restated locally so the assertion above is about the
    /// obligation and not about a spelling.
    fn obligation_of(class: ValueClass) -> OwnershipObligation {
        match class {
            ValueClass::BitCopy | ValueClass::View => OwnershipObligation::None,
            ValueClass::CowValue
            | ValueClass::PersistentShare
            | ValueClass::AffineResource
            | ValueClass::Linear => OwnershipObligation::Owned,
        }
    }
}

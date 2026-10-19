//! Shared symbol projections and canonical type substitution.

pub use hew_types::substitute_type_params;
use hew_types::ResolvedTy;

/// Scheme: `<origin>$$<arg1>$<arg2>...`. The `$` sigil is chosen because
/// the lexer's identifier rule (`hew-lexer/src/lib.rs:451`) only admits
/// `[a-zA-Z0-9_]`, so `$` cannot collide with any user-written Hew
/// identifier. LLVM IR permits `$` in global symbol names.
///
/// Type args are rendered via [`mangle_resolved_ty`]. Compound fragments use
/// `$`-letter tokens (for example, `Vec$li64$g`) so their structure remains
/// unambiguous while the top-level `$$` and `$` separators keep their existing
/// meanings.
#[must_use]
pub fn mangle(origin_name: &str, type_args: &[ResolvedTy]) -> String {
    // Thin wrapper over the parametric `mangle_instantiation` helper.
    // The `SymbolClass::Function` case emits no class prefix and no
    // const-args segment, so output is byte-identical to the prior
    // hand-written body for every input — verified by the snapshot
    // test in `tests/mono_foundation_byte_compat.rs`.
    crate::mono::mangle_instantiation(
        crate::mono::SymbolClass::Function,
        origin_name,
        type_args,
        &[],
    )
}

/// Project an already-selected generic function linker symbol and concrete
/// type arguments to the emitted monomorph symbol.
///
/// This is intentionally distinct from [`mangle_layout_key`]: function
/// linkage consumes the exact HIR body symbol, while record/enum layouts have
/// their own nominal-layout key policy. SIR callable production and verification use this projection, so a call site cannot invent
/// a second spelling for the same monomorphisation.
#[must_use]
pub fn function_monomorph_symbol(linker_symbol: &str, type_args: &[ResolvedTy]) -> String {
    let origin = if linker_symbol.contains("$$") {
        std::borrow::Cow::Owned(format!("$method${}", linker_symbol.replace('$', "$s")))
    } else {
        std::borrow::Cow::Borrowed(linker_symbol)
    };
    mangle(&origin, type_args)
}

/// Build the canonical registry key for a user record or enum layout.
///
/// Layout identity is a nominal identity: both the declaration owner and every
/// named type in its argument spine are significant.  Keep this authority next
/// to the shared mangler so HIR registration, MIR lowering, and codegen lookup
/// cannot independently decide whether to encode a dotted owner or transform a
/// nested payload type.  The compatibility normaliser preserves canonical
/// paths; it only removes `Closure` captures, which are not represented by the
/// type mangle either.
#[must_use]
pub fn mangle_layout_key(origin_name: &str, type_args: &[ResolvedTy]) -> String {
    let canonical_args: Vec<ResolvedTy> = type_args
        .iter()
        .cloned()
        .map(shorten_named_arg_qualifiers)
        .collect();
    mangle(&crate::mangle_dotted_name(origin_name), &canonical_args)
}

/// Build the class-discriminated registry key for a compiler-synthesised
/// cursor record. The builtin discriminator is the authority; a user nominal
/// named `VecIter` or `HashMapIter` cannot call this path.
#[must_use]
pub fn synthetic_cursor_layout_key(
    builtin: hew_types::BuiltinType,
    type_args: &[ResolvedTy],
) -> Option<String> {
    if !matches!(builtin, hew_types::BuiltinType::HashMapIter) {
        return None;
    }
    let canonical_args: Vec<ResolvedTy> = type_args
        .iter()
        .cloned()
        .map(shorten_named_arg_qualifiers)
        .collect();
    Some(crate::mono::mangle_instantiation(
        crate::mono::SymbolClass::SyntheticRecord,
        builtin.canonical_name(),
        &canonical_args,
        &[],
    ))
}

/// Build the class-discriminated registry key for any compiler-owned record
/// layout. Synthetic iterator cursors and fixed-shape builtin structs share
/// the compiler record namespace, while their distinct builtin discriminants
/// keep each nominal disjoint. A user `type MonitorRef { .. }` therefore
/// cannot collide with the runtime `MonitorRef` layout even though both retain
/// the same source-facing leaf.
#[must_use]
pub fn compiler_record_layout_key(
    builtin: hew_types::BuiltinType,
    type_args: &[ResolvedTy],
) -> Option<String> {
    if matches!(builtin, hew_types::BuiltinType::HashMapIter) {
        return synthetic_cursor_layout_key(builtin, type_args);
    }
    let registration = crate::builtin_type_classes::compiler_record_layout_registration(builtin)?;
    if !type_args.is_empty()
        || !matches!(
            registration.shape,
            crate::builtin_type_classes::BuiltinTypeShape::Struct(_)
        )
    {
        return None;
    }
    Some(crate::mono::mangle_instantiation(
        crate::mono::SymbolClass::SyntheticRecord,
        builtin.canonical_name(),
        &[],
        &[],
    ))
}

/// Resolve a named type to the one layout-registry key its semantic carrier
/// permits.  Generic user nominals use the ordinary layout mangle; compiler
/// cursor records use their disjoint synthetic namespace selected by the
/// builtin discriminator.  Keeping the discriminator in this API prevents a
/// user `type VecIter<T>` from selecting the cursor layout by spelling alone.
#[must_use]
pub fn layout_key_for_named(
    name: &str,
    type_args: &[ResolvedTy],
    builtin: Option<hew_types::BuiltinType>,
) -> Option<String> {
    if let Some(key) = builtin.and_then(|kind| compiler_record_layout_key(kind, type_args)) {
        return Some(key);
    }
    if type_args.is_empty() {
        Some(name.to_string())
    } else {
        Some(mangle_layout_key(name, type_args))
    }
}

/// Recursively preserve canonical named identities throughout a type spine.
///
/// Module qualification is declaration identity, not decoration. The old
/// normaliser stripped it before later lookup stages, which made same-leaf
/// nominals collide. The compatibility function name remains while callers
/// migrate, but it now preserves every canonical path exactly.
///
/// Recurses into every compound `ResolvedTy` shape that `mangle_resolved_ty`
/// descends into (Tuple/Array/Slice/Named-args, plus Function/Closure/
/// Pointer/Borrow/TraitObject/Task), so a NESTED qualified payload
/// (`Result<Vec<fs.Foo>, _>`, `Option<x.T>`) retains its declaration path at
/// every depth. Leaf variants and `TypeParam` (which carries only a parameter
/// name, never a qualifier-bearing nested type) pass through unchanged.
/// Closure captures are not part of the call-type identity (see
/// `mangle_resolved_ty`) and are dropped here exactly as they are dropped from
/// the mangle, keeping this normaliser congruent with the key it feeds.
#[must_use]
pub fn shorten_named_arg_qualifiers(ty: ResolvedTy) -> ResolvedTy {
    fn shorten_boxed(ty: ResolvedTy) -> Box<ResolvedTy> {
        Box::new(shorten_named_arg_qualifiers(ty))
    }
    match ty {
        ResolvedTy::Named {
            head,
            args,
            is_opaque,
            ..
        } => {
            let args = args.into_iter().map(shorten_named_arg_qualifiers).collect();
            ResolvedTy::Named {
                head,
                args,
                is_opaque,
            }
        }
        ResolvedTy::Tuple(items) => ResolvedTy::Tuple(
            items
                .into_iter()
                .map(shorten_named_arg_qualifiers)
                .collect(),
        ),
        ResolvedTy::Array(elem, n) => ResolvedTy::Array(shorten_boxed(*elem), n),
        ResolvedTy::Slice(elem) => ResolvedTy::Slice(shorten_boxed(*elem)),
        ResolvedTy::Function {
            capabilities,
            params,
            ret,
        } => ResolvedTy::Function {
            capabilities,
            params: params
                .into_iter()
                .map(shorten_named_arg_qualifiers)
                .collect(),
            ret: shorten_boxed(*ret),
        },
        ResolvedTy::Closure {
            capabilities,
            params,
            ret,
            ..
        } => ResolvedTy::Closure {
            capabilities,
            params: params
                .into_iter()
                .map(shorten_named_arg_qualifiers)
                .collect(),
            ret: shorten_boxed(*ret),
            // Captures are not part of the call-type identity (see
            // `mangle_resolved_ty`); drop them so the normalised form matches
            // what the mangle renders.
            captures: Vec::new(),
        },
        ResolvedTy::Pointer {
            is_mutable,
            pointee,
        } => ResolvedTy::Pointer {
            is_mutable,
            pointee: shorten_boxed(*pointee),
        },
        ResolvedTy::Borrow { pointee } => ResolvedTy::Borrow {
            pointee: shorten_boxed(*pointee),
        },
        ResolvedTy::TraitObject { traits } => ResolvedTy::TraitObject {
            traits: traits
                .into_iter()
                .map(|b| hew_types::ResolvedTraitBound {
                    trait_name: b.trait_name,
                    trait_id: b.trait_id,
                    args: b
                        .args
                        .into_iter()
                        .map(shorten_named_arg_qualifiers)
                        .collect(),
                    assoc_bindings: b
                        .assoc_bindings
                        .into_iter()
                        .map(|(n, t)| (n, shorten_named_arg_qualifiers(t)))
                        .collect(),
                })
                .collect(),
        },
        ResolvedTy::Task(inner) => ResolvedTy::Task(shorten_boxed(*inner)),
        other => other,
    }
}

/// The mangler moved to `hew_types::mangle` beside the class authority
/// (ir-ladder §5.1) so `hew-mir` does not depend on `hew-hir` for a name.
/// The nine call sites in this crate and downstream keep this spelling.
pub use hew_types::mangle::mangle_resolved_ty;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mangle_simple_types() {
        assert_eq!(mangle("id", &[ResolvedTy::I64]), "id$$i64");
        assert_eq!(mangle("id", &[ResolvedTy::String]), "id$$string");
    }

    #[test]
    fn mangle_two_args() {
        assert_eq!(
            mangle("pick", &[ResolvedTy::I64, ResolvedTy::Bool]),
            "pick$$i64$bool"
        );
    }

    #[test]
    fn mangle_nested_named() {
        let label = ResolvedTy::named_for_test("Label", vec![]);
        assert_eq!(mangle("describe", &[label]), "describe$$Label");
    }

    #[test]
    fn mangle_module_qualified_encodes_colons() {
        let ty = ResolvedTy::named_for_test("widgets::Label", vec![]);
        assert_eq!(mangle("describe", &[ty]), "describe$$widgets$mLabel");
    }

    #[test]
    fn substitute_replaces_bare_type_param() {
        let params = vec![hew_types::ParamHead::for_test("T")];
        let args = vec![ResolvedTy::I64];
        let ty = ResolvedTy::param(hew_types::ParamHead::for_test("T"));
        assert_eq!(substitute_type_params(&ty, &params, &args), ResolvedTy::I64);
    }

    #[test]
    fn substitute_descends_into_nested_named() {
        // Vec<T> with T=i64 -> Vec<i64>
        let params = vec![hew_types::ParamHead::for_test("T")];
        let args = vec![ResolvedTy::I64];
        let ty = ResolvedTy::named_builtin(
            hew_types::BuiltinType::Vec,
            vec![ResolvedTy::param(hew_types::ParamHead::for_test("T"))],
        );
        assert_eq!(
            substitute_type_params(&ty, &params, &args),
            ResolvedTy::named_builtin(hew_types::BuiltinType::Vec, vec![ResolvedTy::I64])
        );
    }

    #[test]
    fn substitute_leaves_unrelated_named_alone() {
        let params = vec![hew_types::ParamHead::for_test("T")];
        let args = vec![ResolvedTy::I64];
        let ty = ResolvedTy::named_for_test("Label", vec![]);
        assert_eq!(substitute_type_params(&ty, &params, &args), ty);
    }

    #[test]
    fn substitute_replaces_structural_type_param_variant() {
        // `ResolvedTy::TypeParam` is the encoding `from_ty_with_type_params`
        // produces for a bare parameter reference (e.g. an impl method's
        // `-> Option<T>` return type); it is a distinct enum variant from
        // the legacy `Named { args: [] }` form and must substitute the
        // same way.
        let params = vec![hew_types::ParamHead::for_test("T")];
        let args = vec![ResolvedTy::I64];
        let ty = ResolvedTy::TypeParam {
            name: hew_types::ParamHead::for_test("T"),
        };
        assert_eq!(substitute_type_params(&ty, &params, &args), ResolvedTy::I64);
    }

    #[test]
    fn substitute_descends_into_nested_type_param_variant() {
        // Option<T> where T is the structural TypeParam variant -> Option<i64>.
        let params = vec![hew_types::ParamHead::for_test("T")];
        let args = vec![ResolvedTy::I64];
        let builtin = hew_types::BuiltinType::Option;
        let ty = ResolvedTy::named_builtin(
            builtin,
            vec![ResolvedTy::TypeParam {
                name: hew_types::ParamHead::for_test("T"),
            }],
        );
        assert_eq!(
            substitute_type_params(&ty, &params, &args),
            ResolvedTy::named_builtin(builtin, vec![ResolvedTy::I64])
        );
    }

    /// Layout identity is declaration identity. Two modules may each declare a
    /// generic record with the same leaf, and their instantiations at the same
    /// type argument must stay separate layouts — a mangle that kept only the
    /// leaf would hand both declarations one symbol and one field shape.
    #[test]
    fn same_leaf_generic_records_from_different_modules_mangle_apart() {
        let args = vec![ResolvedTy::I32];
        let left = mangle_layout_key("hew.keyleft.Key", &args);
        let right = mangle_layout_key("hew.keyright.Key", &args);

        assert_ne!(left, right);
        // Neither key may degrade to the leaf-only spelling a same-named
        // declaration in a third module would also produce.
        let bare = mangle_layout_key("Key", &args);
        assert_ne!(left, bare);
        assert_ne!(right, bare);
    }

    #[test]
    fn synthetic_cursor_layout_keys_do_not_collide_with_user_same_leaf_records() {
        let map_args = vec![ResolvedTy::String, ResolvedTy::I64];
        let synthetic_map =
            synthetic_cursor_layout_key(hew_types::BuiltinType::HashMapIter, &map_args)
                .expect("HashMapIter is a synthetic cursor");
        assert_ne!(synthetic_map, mangle_layout_key("HashMapIter", &map_args));
    }

    #[test]
    fn compiler_struct_layout_keys_do_not_collide_with_user_same_leaf_records() {
        for builtin in [
            hew_types::BuiltinType::MonitorRef,
            hew_types::BuiltinType::CrashInfo,
            hew_types::BuiltinType::CrashNotification,
            hew_types::BuiltinType::NodeId,
        ] {
            let compiler = compiler_record_layout_key(builtin, &[])
                .expect("catalog Struct builtin has a compiler record layout");
            assert_ne!(compiler, builtin.canonical_name());
            assert_eq!(
                layout_key_for_named(builtin.canonical_name(), &[], Some(builtin)),
                Some(compiler)
            );
            assert_eq!(
                layout_key_for_named(builtin.canonical_name(), &[], None),
                Some(builtin.canonical_name().to_string())
            );
        }
    }

    #[test]
    fn shorten_named_arg_qualifiers_preserves_nested_qualified_payload() {
        // The compatibility helper recursively preserves identity through
        // every nested payload shape.
        let nested = ResolvedTy::named_for_test(
            "Result",
            vec![
                ResolvedTy::named_for_test(
                    "Vec",
                    vec![ResolvedTy::named_for_test("fs.Foo", vec![])],
                ),
                ResolvedTy::Tuple(vec![
                    ResolvedTy::Slice(Box::new(ResolvedTy::named_for_test("net.Conn", vec![]))),
                    ResolvedTy::Array(Box::new(ResolvedTy::named_for_test("io.Buf", vec![])), 4),
                ]),
            ],
        );
        let canonical = shorten_named_arg_qualifiers(nested.clone());
        assert_eq!(canonical, nested);
        // The canonical mangle carries every nested module qualifier.
        assert_eq!(
            mangle("Result", &[canonical]),
            "Result$$Result$lVec$lfs$mFoo$g$ctuple$xslice$xnet$mConn$g$carray$xio$mBuf$c4$g$g$g",
        );
    }
}

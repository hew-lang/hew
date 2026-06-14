//! HIR-level monomorphisation registry.
//!
//! Records every concrete instantiation of a generic top-level user function
//! observed at a call site, derived from the checker's authoritative
//! `call_type_args` side-table (`hew-types/src/check/calls.rs:76`). Each
//! distinct `(origin_fn_id, Vec<ResolvedTy>)` pair becomes one
//! `MonomorphizedFn` entry; downstream MIR (G-1.b) and LLVM (G-1.c) consume
//! these to emit one specialised function per instantiation.
//!
//! This slice (G-1.a) is the producer-bridge wakeup that takes the
//! `LowerCtx.call_type_args` field out of `#[expect(dead_code)]`. The
//! registry shape it emits is intentionally minimal — it stores the origin
//! identity, the concrete type args, and a mangled name — because actual
//! substitution into bodies is G-1.b's responsibility (per
//! `.tmp/plans/g1-generics-monomorphization.md`).
//!
//! Cycle / fixed-point note: the checker's
//! `record_concrete_call_type_args` (calls.rs:78) refuses to record any call
//! whose resolved type args still contain inference variables, which means
//! a polymorphic self-call inside a generic body (`fn id<T>(x:T) { id(x) }`)
//! never lands in `call_type_args` with `T` left abstract — only with
//! concrete substitutions. The registry therefore observes one entry per
//! concrete `T` at the outer callsite and converges by construction at
//! G-1.a; true polymorphic-recursion cycle detection lands in G-1.b once
//! substitution makes inner callsites concrete.
//!
//! LESSONS: `end-to-end-before-layer-thickening` (P1), `checker-authority` (P0).

use std::collections::HashMap;
use std::ops::Range;

use hew_types::ResolvedTy;

use crate::ids::ItemId;

/// Default maximum number of distinct monomorphisations admitted per
/// HIR-lowering invocation. A user program that exceeds this cap is
/// almost certainly the symptom of an inference path producing
/// ever-larger concrete types (e.g. trait bounds that admit
/// `T → Vec<T> → Vec<Vec<T>>` via successive callsites). The cap
/// surfaces a fail-closed diagnostic rather than letting downstream
/// stages OOM during codegen.
///
/// The cap is overridable via [`lower_program_with_mono_cap`] for tests
/// that exercise the diagnostic path with a smaller fixture.
pub const MONOMORPHISATION_REGISTRY_CAP: usize = 1024;

/// Stable identity for a single monomorphisation.
///
/// Two callsites that instantiate the same generic fn with the same
/// concrete type args produce equal `MonoKey`s and collapse to one
/// registry entry; two callsites with the same fn but different type
/// args produce distinct keys.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonoKey {
    /// `ItemId` of the originating generic function declaration, as
    /// allocated in the HIR first pass. Pairing on `ItemId` rather than
    /// on `name` defends against module-qualified shadowing (two
    /// imported `describe` symbols would be distinct `ItemId`s).
    pub origin: ItemId,
    /// Origin function name as written in source. Retained for
    /// diagnostics and the mangled-name scheme; not part of identity
    /// when paired with `origin` (the two move together).
    pub origin_name: String,
    /// Concrete type arguments in source-declared order. Always
    /// `ResolvedTy` (not `Ty`) because the boundary conversion
    /// `ResolvedTy::from_ty` rejects any leaked `Var`/`Error`/
    /// `IntLiteral`/`FloatLiteral` — the registry is therefore
    /// fail-closed at the side-table seam.
    pub type_args: Vec<ResolvedTy>,
}

/// One specialised function the downstream MIR/LLVM stages must emit.
///
/// G-1.a only populates identity + mangled name. G-1.b is responsible
/// for substituting the body and producing a real specialised
/// `HirFn` (or a MIR-level analogue).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MonomorphizedFn {
    /// Identity of this monomorphisation.
    pub key: MonoKey,
    /// Symbol name downstream codegen will emit. Built by
    /// [`mangle`] from `key.origin_name` and `key.type_args`.
    pub mangled_name: String,
}

/// Mangle an origin name and concrete type args into a single LLVM-safe
/// symbol.
///
/// Scheme: `<origin>$$<arg1>$<arg2>...`. The `$` sigil is chosen because
/// the lexer's identifier rule (`hew-lexer/src/lib.rs:451`) only admits
/// `[a-zA-Z0-9_]`, so `$` cannot collide with any user-written Hew
/// identifier. LLVM IR permits `$` in global symbol names.
///
/// Type args are rendered via [`mangle_resolved_ty`], which uses `_`
/// internally for nested args (e.g. `Vec_i64`) — the major/minor
/// separator distinction (`$$` vs `$`) keeps top-level args
/// unambiguously parseable.
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

/// Recursively replace every `Named { name, .. }` anywhere in `ty` with its
/// short (unqualified) name, stripping a leading `"module."` prefix, while
/// preserving the surrounding type structure.
///
/// This is the single canonical type-arg-spine normaliser shared by every
/// layout-key producer: the enum layout-registration side
/// (`EnumLayoutRegistry::insert`), the record layout-registration side
/// (`hew-mir::lower::user_record_layout_key`), and EVERY codegen
/// layout-lookup / drop-seed / codec-seed mangle site in
/// `hew-codegen-rs/src/llvm.rs`. Because all of them call THIS function, the
/// registration key and every lookup key for the same instantiated type are
/// byte-identical by construction and cannot drift.
///
/// C1 stamps an authoritative module-qualified name onto `ResolvedTy::Named`
/// for imported type references. That qualifier is meaningful for the OUTER
/// user-type identity (the struct-layout collision scan dedupes by distinct
/// qualified identity) but must NOT leak into a mangle key's type-arg spine:
/// one payload type reached via two import paths (`fs.IoError` vs `IoError`)
/// denotes one type and must collapse to one layout entry. Codegen always
/// normalises its spine to bare before mangling, so every key producer does
/// the same here.
///
/// Recurses into every compound `ResolvedTy` shape that `mangle_resolved_ty`
/// descends into (Tuple/Array/Slice/Named-args, plus Function/Closure/
/// Pointer/Borrow/TraitObject/Task), so a NESTED qualified payload
/// (`Result<Vec<fs.Foo>, _>`, `Option<x.T>`) is shortened at every depth —
/// not just the top-level args — and no shape can carry a qualified name into
/// the key. Leaf variants and `TypeParam` (which carries only a parameter
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
            name,
            args,
            builtin,
            is_opaque,
        } => ResolvedTy::Named {
            name: name
                .rsplit_once('.')
                .map_or(name.clone(), |(_, short)| short.to_string()),
            args: args.into_iter().map(shorten_named_arg_qualifiers).collect(),
            builtin,
            is_opaque,
        },
        ResolvedTy::Tuple(items) => ResolvedTy::Tuple(
            items
                .into_iter()
                .map(shorten_named_arg_qualifiers)
                .collect(),
        ),
        ResolvedTy::Array(elem, n) => ResolvedTy::Array(shorten_boxed(*elem), n),
        ResolvedTy::Slice(elem) => ResolvedTy::Slice(shorten_boxed(*elem)),
        ResolvedTy::Function { params, ret } => ResolvedTy::Function {
            params: params
                .into_iter()
                .map(shorten_named_arg_qualifiers)
                .collect(),
            ret: shorten_boxed(*ret),
        },
        ResolvedTy::Closure { params, ret, .. } => ResolvedTy::Closure {
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
                    trait_name: b
                        .trait_name
                        .rsplit_once('.')
                        .map_or(b.trait_name.clone(), |(_, short)| short.to_string()),
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

/// Render a single `ResolvedTy` as a mangled fragment.
///
/// Uses `_` as the nested separator so a top-level `$`-separated mangle
/// can recover individual args by splitting on `$` alone. Returns names
/// that are stable across runs (no hash, no monotonic counter). The
/// rendering is structural and injective over the `ResolvedTy` variants
/// (distinct types render to distinct fragments), so it is also the
/// canonical key source for codegen-synthesised per-type thunks.
#[must_use]
pub fn mangle_resolved_ty(ty: &ResolvedTy) -> String {
    match ty {
        ResolvedTy::I8 => "i8".to_string(),
        ResolvedTy::I16 => "i16".to_string(),
        ResolvedTy::I32 => "i32".to_string(),
        ResolvedTy::I64 => "i64".to_string(),
        ResolvedTy::U8 => "u8".to_string(),
        ResolvedTy::U16 => "u16".to_string(),
        ResolvedTy::U32 => "u32".to_string(),
        ResolvedTy::U64 => "u64".to_string(),
        ResolvedTy::Isize => "isize".to_string(),
        ResolvedTy::Usize => "usize".to_string(),
        ResolvedTy::F32 => "f32".to_string(),
        ResolvedTy::F64 => "f64".to_string(),
        ResolvedTy::Bool => "bool".to_string(),
        ResolvedTy::Char => "char".to_string(),
        ResolvedTy::String => "string".to_string(),
        ResolvedTy::Bytes => "bytes".to_string(),
        ResolvedTy::CancellationToken => "CancellationToken".to_string(),
        ResolvedTy::Duration => "duration".to_string(),
        ResolvedTy::Unit => "unit".to_string(),
        ResolvedTy::Never => "never".to_string(),
        ResolvedTy::Tuple(items) => {
            let mut out = String::from("tuple_");
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    out.push('_');
                }
                out.push_str(&mangle_resolved_ty(item));
            }
            out
        }
        ResolvedTy::Array(elem, n) => format!("array_{}_{}", mangle_resolved_ty(elem), n),
        ResolvedTy::Slice(elem) => format!("slice_{}", mangle_resolved_ty(elem)),
        ResolvedTy::Named { name, args, .. } => {
            // Replace any `::` from module-qualified names with `_` to keep
            // the symbol LLVM-clean.
            let mut out = name.replace("::", "_");
            for arg in args {
                out.push('_');
                out.push_str(&mangle_resolved_ty(arg));
            }
            out
        }
        ResolvedTy::Function { params, ret } => {
            let mut out = String::from("fn_");
            for p in params {
                out.push_str(&mangle_resolved_ty(p));
                out.push('_');
            }
            out.push_str("ret_");
            out.push_str(&mangle_resolved_ty(ret));
            out
        }
        ResolvedTy::Closure { params, ret, .. } => {
            // Captures are not part of the call-type identity.
            let mut out = String::from("closure_");
            for p in params {
                out.push_str(&mangle_resolved_ty(p));
                out.push('_');
            }
            out.push_str("ret_");
            out.push_str(&mangle_resolved_ty(ret));
            out
        }
        ResolvedTy::Pointer {
            is_mutable,
            pointee,
        } => {
            if *is_mutable {
                format!("ptrmut_{}", mangle_resolved_ty(pointee))
            } else {
                format!("ptr_{}", mangle_resolved_ty(pointee))
            }
        }
        ResolvedTy::Borrow { pointee } => {
            format!("borrow_{}", mangle_resolved_ty(pointee))
        }
        ResolvedTy::TraitObject { traits } => {
            let mut out = String::from("dyn_");
            for (i, b) in traits.iter().enumerate() {
                if i > 0 {
                    out.push('_');
                }
                out.push_str(&b.trait_name.replace("::", "_"));
                for a in &b.args {
                    out.push('_');
                    out.push_str(&mangle_resolved_ty(a));
                }
            }
            out
        }
        ResolvedTy::Task(inner) => format!("task_{}", mangle_resolved_ty(inner)),
        // A concrete monomorphisation never carries an abstract parameter in
        // its type-arg list, so this is not reached for a real instantiation.
        // It is mangled deterministically (and LLVM-clean) for totality.
        ResolvedTy::TypeParam { name } => format!("typeparam_{}", name.replace("::", "_")),
    }
}

/// Insertion-ordered registry used during HIR lowering.
///
/// Wraps a `HashMap` for O(1) deduplication and a `Vec` for stable
/// iteration order so that downstream IR emission is deterministic. The
/// `HirModule.monomorphisations` output is built by draining `order`.
#[derive(Debug, Default)]
pub(crate) struct MonoRegistry {
    seen: HashMap<MonoKey, usize>,
    order: Vec<MonomorphizedFn>,
    cap: usize,
}

impl MonoRegistry {
    pub(crate) fn with_cap(cap: usize) -> Self {
        Self {
            seen: HashMap::new(),
            order: Vec::new(),
            cap,
        }
    }

    /// Attempt to insert a new instantiation. Returns `Ok(true)` if a
    /// fresh entry landed, `Ok(false)` if the key was already present,
    /// and `Err(())` if the cap was exceeded (the registry refuses to
    /// grow past `cap`; subsequent inserts after the cap fires also
    /// return `Err(())` without growing the registry).
    pub(crate) fn insert(&mut self, key: MonoKey) -> Result<bool, ()> {
        if self.seen.contains_key(&key) {
            return Ok(false);
        }
        if self.order.len() >= self.cap {
            return Err(());
        }
        let mangled_name = mangle(&key.origin_name, &key.type_args);
        let idx = self.order.len();
        self.order.push(MonomorphizedFn {
            key: key.clone(),
            mangled_name,
        });
        self.seen.insert(key, idx);
        Ok(true)
    }

    pub(crate) fn cap(&self) -> usize {
        self.cap
    }

    pub(crate) fn into_vec(self) -> Vec<MonomorphizedFn> {
        self.order
    }
}

// ── Record-layout monomorphisation ──────────────────────────────────────────
//
// The record-layout registry is the structural sibling of `MonoRegistry`:
// it records every distinct `(record_origin_id, Vec<ResolvedTy>)` pair
// observed at user struct-init sites against a generic `pub type` (or
// `record` with type params). Each entry becomes one `RecordLayout` with
// concretely-substituted field types and a mangled symbol name that
// matches the function-mangling scheme (`mangle(origin_name, type_args)`).
//
// Producer side-table: `TypeCheckOutput.record_init_type_args`. Downstream
// MIR and LLVM consumers iterate this list to emit one layout per entry.
//
// LESSONS: `end-to-end-before-layer-thickening` (P1), `checker-authority` (P0).

/// Stable identity for one record-layout monomorphisation. Two struct-
/// init sites that instantiate the same generic record with the same
/// concrete type args produce equal `RecordMonoKey`s and collapse to
/// one registry entry.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordMonoKey {
    /// `ItemId` of the originating user record/type declaration, as
    /// allocated in the HIR first pass. Pairing on `ItemId` (rather than
    /// name) defends against module-qualified shadowing.
    pub origin: ItemId,
    /// Origin record name as written in source. Retained for diagnostics
    /// and the mangled-name scheme.
    pub origin_name: String,
    /// Concrete type arguments in source-declared order. Always
    /// `ResolvedTy` because the boundary conversion `ResolvedTy::from_ty`
    /// rejects any leaked `Var`/`Error`/`IntLiteral`/`FloatLiteral` — the
    /// registry is fail-closed at the side-table seam.
    pub type_args: Vec<ResolvedTy>,
}

/// One specialised record layout the downstream MIR/LLVM stages must
/// emit. Carries identity, a mangled symbol name, and substituted field
/// types; downstream consumers iterate `HirModule.record_layouts` to
/// emit one layout entry per record instantiation.
#[derive(Debug, Clone, PartialEq)]
pub struct RecordLayout {
    /// Identity of this monomorphisation.
    pub key: RecordMonoKey,
    /// Symbol name downstream codegen will emit for this layout.
    /// Built by [`mangle`] from `key.origin_name` and `key.type_args`,
    /// so a record-layout symbol shares the same `$$<args>` shape as a
    /// fn-monomorphisation symbol.
    pub mangled_name: String,
    /// Concrete field shape after substituting `key.type_args` for the
    /// origin record's `type_params`. Field order is source order.
    pub fields: Vec<(String, ResolvedTy)>,
    /// Span of one observed struct-init site (the first one inserted).
    /// Used for diagnostics that need to cite a concrete source location
    /// when complaining about the layout (e.g. recursive-generic
    /// detection). Not load-bearing for codegen.
    pub span: Range<usize>,
}

/// Insertion-ordered registry for record-layout monomorphisations.
/// Mirrors `MonoRegistry`'s shape.
#[derive(Debug, Default)]
pub(crate) struct RecordLayoutRegistry {
    seen: HashMap<RecordMonoKey, usize>,
    order: Vec<RecordLayout>,
    cap: usize,
}

impl RecordLayoutRegistry {
    pub(crate) fn with_cap(cap: usize) -> Self {
        Self {
            seen: HashMap::new(),
            order: Vec::new(),
            cap,
        }
    }

    /// Attempt to insert a new layout. Returns `Ok(true)` if a fresh
    /// entry landed, `Ok(false)` if the key was already present, and
    /// `Err(())` if the cap was exceeded.
    pub(crate) fn insert(
        &mut self,
        key: RecordMonoKey,
        fields: Vec<(String, ResolvedTy)>,
        span: Range<usize>,
    ) -> Result<bool, ()> {
        if self.seen.contains_key(&key) {
            return Ok(false);
        }
        if self.order.len() >= self.cap {
            return Err(());
        }
        let mangled_name = mangle(&key.origin_name, &key.type_args);
        let idx = self.order.len();
        self.order.push(RecordLayout {
            key: key.clone(),
            mangled_name,
            fields,
            span,
        });
        self.seen.insert(key, idx);
        Ok(true)
    }

    pub(crate) fn cap(&self) -> usize {
        self.cap
    }

    pub(crate) fn into_vec(self) -> Vec<RecordLayout> {
        self.order
    }
}

/// Substitute generic type-parameter symbols inside `ty` with the
/// concrete `args`. Parameters are matched by name against `params`; the
/// resolved argument at the corresponding index replaces every
/// `ResolvedTy::Named { name, args: [] }` whose name appears in
/// `params`.
///
/// Recursion descends into compound types so nested occurrences are also
/// substituted (e.g. `Box<T>`'s field type `Vec<T>` substitutes to
/// `Vec<i64>` when `T = i64`).
///
/// The traversal is a pure rewrite — it does not touch the registry. The
/// caller is responsible for detecting recursive polymorphic
/// instantiations after substitution.
#[must_use]
pub fn substitute_type_params(
    ty: &ResolvedTy,
    params: &[String],
    args: &[ResolvedTy],
) -> ResolvedTy {
    debug_assert_eq!(
        params.len(),
        args.len(),
        "param/arg arity mismatch — caller must validate before substituting"
    );
    match ty {
        ResolvedTy::Named {
            name,
            args: named_args,
            builtin,
            is_opaque,
        } => {
            // Bare type-parameter reference (e.g. `T`) — substitute.
            if named_args.is_empty() {
                if let Some(idx) = params.iter().position(|p| p == name) {
                    return args[idx].clone();
                }
            }
            // Otherwise descend into the args so `Vec<T>` becomes `Vec<i64>`.
            ResolvedTy::Named {
                name: name.clone(),
                args: named_args
                    .iter()
                    .map(|a| substitute_type_params(a, params, args))
                    .collect(),
                builtin: *builtin,
                is_opaque: *is_opaque,
            }
        }
        ResolvedTy::Tuple(items) => ResolvedTy::Tuple(
            items
                .iter()
                .map(|t| substitute_type_params(t, params, args))
                .collect(),
        ),
        ResolvedTy::Array(elem, n) => {
            ResolvedTy::Array(Box::new(substitute_type_params(elem, params, args)), *n)
        }
        ResolvedTy::Slice(elem) => {
            ResolvedTy::Slice(Box::new(substitute_type_params(elem, params, args)))
        }
        ResolvedTy::Function {
            params: fn_params,
            ret,
        } => ResolvedTy::Function {
            params: fn_params
                .iter()
                .map(|p| substitute_type_params(p, params, args))
                .collect(),
            ret: Box::new(substitute_type_params(ret, params, args)),
        },
        ResolvedTy::Closure {
            params: fn_params,
            ret,
            captures,
        } => ResolvedTy::Closure {
            params: fn_params
                .iter()
                .map(|p| substitute_type_params(p, params, args))
                .collect(),
            ret: Box::new(substitute_type_params(ret, params, args)),
            captures: captures
                .iter()
                .map(|c| substitute_type_params(c, params, args))
                .collect(),
        },
        ResolvedTy::Pointer {
            is_mutable,
            pointee,
        } => ResolvedTy::Pointer {
            is_mutable: *is_mutable,
            pointee: Box::new(substitute_type_params(pointee, params, args)),
        },
        ResolvedTy::Borrow { pointee } => ResolvedTy::Borrow {
            pointee: Box::new(substitute_type_params(pointee, params, args)),
        },
        ResolvedTy::Task(inner) => {
            ResolvedTy::Task(Box::new(substitute_type_params(inner, params, args)))
        }
        // Primitives and trait objects: nothing to substitute. Trait
        // objects are not exercised by user generic records in v0.5 —
        // leave their args verbatim; methods on generic records will
        // revisit this if needed.
        _ => ty.clone(),
    }
}

// ── Enum-layout monomorphisation ────────────────────────────────────────────
//
// The enum-layout registry is the structural sibling of `RecordLayoutRegistry`:
// it records every distinct `(enum_origin_id, Vec<ResolvedTy>)` pair observed
// at enum-ctor sites and match scrutinees. Each entry becomes one `EnumLayout`
// with concretely-substituted variant payload types and a mangled symbol name
// that matches the function-mangling scheme (`mangle(origin_name, type_args)`).
//
// Producer: the HIR mono pass discovery walker in `hew-hir/src/lower.rs`,
// which walks `HirExprKind::MachineVariantCtor` nodes and match scrutinees
// and consults the checker's `expr_types` side-table for the resolved enum
// type with its concrete type args. Downstream MIR and codegen consumers
// iterate `HirModule.enum_layouts` to emit one layout per entry.
//
// LESSONS: `end-to-end-before-layer-thickening` (P1), `checker-authority` (P0).

/// Stable identity for one enum-layout monomorphisation. Two ctor sites that
/// instantiate the same generic enum with the same concrete type args produce
/// equal `EnumMonoKey`s and collapse to one registry entry.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumMonoKey {
    /// `ItemId` of the originating generic enum declaration, as allocated
    /// in the HIR first pass. Pairing on `ItemId` rather than name defends
    /// against module-qualified shadowing.
    pub origin: ItemId,
    /// Origin enum name as written in source. Retained for diagnostics
    /// and the mangled-name scheme.
    pub origin_name: String,
    /// Concrete type arguments in source-declared order. Always `ResolvedTy`
    /// (the boundary conversion `ResolvedTy::from_ty` rejects leaked inference
    /// variables). The registry is fail-closed at the side-table seam.
    pub type_args: Vec<ResolvedTy>,
}

/// The substituted payload type-list for one enum variant in a mono'd enum
/// instantiation. `field_tys` has been substituted: every occurrence of
/// a type-param symbol has been replaced with the corresponding concrete arg.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariantLayout {
    /// Variant name as written in source. Retained for diagnostics; not
    /// load-bearing for codegen (codegen addresses variants by index).
    pub name: String,
    /// Positional payload types after substituting the mono key's type args
    /// into the variant's declared field types. Empty for unit variants.
    pub field_tys: Vec<ResolvedTy>,
}

/// One specialised enum layout the downstream MIR/codegen stages must emit.
/// Carries identity, a mangled symbol name, and the substituted variant list.
/// Downstream consumers iterate `HirModule.enum_layouts` to emit one layout
/// entry per enum instantiation under the mangled name.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumLayout {
    /// Identity of this monomorphisation.
    pub key: EnumMonoKey,
    /// Symbol name downstream codegen will emit for this layout. Built by
    /// [`mangle`] from `key.origin_name` and `key.type_args`, sharing the
    /// same `$$<args>` scheme as fn- and record-layout symbols.
    pub mangled_name: String,
    /// All variants in declaration order with substituted payload types.
    pub variants: Vec<EnumVariantLayout>,
}

/// Insertion-ordered registry for enum-layout monomorphisations.
/// Mirrors `RecordLayoutRegistry`'s shape exactly.
///
/// Deduplicates by `(origin ItemId, type_args)` key. Capped at the same
/// `mono_cap` used for record and fn monomorphisation registries so the
/// module-level cap is uniform. `insert` returns `Ok(true)` for a new
/// entry, `Ok(false)` for a duplicate, and `Err(())` when the cap is
/// exceeded (caller emits `EnumLayoutCapExceeded` diagnostic).
#[derive(Debug, Default)]
pub(crate) struct EnumLayoutRegistry {
    seen: HashMap<EnumMonoKey, usize>,
    order: Vec<EnumLayout>,
    cap: usize,
}

impl EnumLayoutRegistry {
    pub(crate) fn with_cap(cap: usize) -> Self {
        Self {
            seen: HashMap::new(),
            order: Vec::new(),
            cap,
        }
    }

    /// Attempt to insert a new layout. Returns `Ok(true)` if a fresh entry
    /// landed, `Ok(false)` if the key was already present, and `Err(())` if
    /// the cap was exceeded (uniform with `RecordLayoutRegistry`).
    ///
    /// The dedup key and mangled name are computed from the type-arg spine
    /// with module qualifiers stripped from every `Named` payload name. The
    /// codegen enum-layout lookup (`hew-codegen-rs/src/llvm.rs` `resolve_ty`)
    /// shortens its type-arg spine to bare names before mangling, so the
    /// registration key MUST do the same or the keys diverge: a generic enum
    /// instantiated through an import-use site (`Result<_, fs.IoError>`, with
    /// C1's authoritative qualified `Named.name`) would register under
    /// `Result$$_$fs.IoError` while the lookup probes `Result$$_$IoError`,
    /// the miss falling through to the D10 fail-closed gate. Normalising here
    /// also collapses the qualified and bare spellings of the same payload
    /// type to one layout entry (they denote one type reached via two import
    /// paths). The outer `origin_name` is kept verbatim — codegen keeps the
    /// outer enum name and only shortens the args.
    pub(crate) fn insert(
        &mut self,
        key: EnumMonoKey,
        variants: Vec<EnumVariantLayout>,
    ) -> Result<bool, ()> {
        let normalized_args: Vec<ResolvedTy> = key
            .type_args
            .iter()
            .cloned()
            .map(shorten_named_arg_qualifiers)
            .collect();
        let key = EnumMonoKey {
            type_args: normalized_args,
            ..key
        };
        if self.seen.contains_key(&key) {
            return Ok(false);
        }
        if self.order.len() >= self.cap {
            return Err(());
        }
        let mangled_name = mangle(&key.origin_name, &key.type_args);
        let idx = self.order.len();
        self.order.push(EnumLayout {
            key: key.clone(),
            mangled_name,
            variants,
        });
        self.seen.insert(key, idx);
        Ok(true)
    }

    pub(crate) fn cap(&self) -> usize {
        self.cap
    }

    pub(crate) fn into_vec(self) -> Vec<EnumLayout> {
        self.order
    }
}

/// Detect whether `ty` contains a reference to `origin_name` with
/// different concrete type args than `current_args`. Used to fail-closed
/// on recursive polymorphic instantiations like
/// `pub type Node<T> { next: Box<Node<int>> }` — observed `Node<T>` has
/// `args = [T_concrete]`, but the field mentions `Node<int>`, a
/// different arg set, which would force unbounded layout expansion.
///
/// Self-reference with the *same* args is fine (`pub type Box<T> { next:
/// Box<T> }` is a recursive shape but expansion converges at one
/// layout). Self-reference with *different* args is the unbounded case.
#[must_use]
pub(crate) fn contains_recursive_polymorphic_self(
    ty: &ResolvedTy,
    origin_name: &str,
    current_args: &[ResolvedTy],
) -> bool {
    match ty {
        ResolvedTy::Named { name, args, .. } => {
            if name == origin_name && args.as_slice() != current_args {
                return true;
            }
            args.iter()
                .any(|a| contains_recursive_polymorphic_self(a, origin_name, current_args))
        }
        ResolvedTy::Tuple(items) => items
            .iter()
            .any(|t| contains_recursive_polymorphic_self(t, origin_name, current_args)),
        ResolvedTy::Array(elem, _) | ResolvedTy::Slice(elem) => {
            contains_recursive_polymorphic_self(elem, origin_name, current_args)
        }
        ResolvedTy::Function { params, ret } => {
            params
                .iter()
                .any(|p| contains_recursive_polymorphic_self(p, origin_name, current_args))
                || contains_recursive_polymorphic_self(ret, origin_name, current_args)
        }
        ResolvedTy::Closure {
            params,
            ret,
            captures,
        } => {
            params
                .iter()
                .any(|p| contains_recursive_polymorphic_self(p, origin_name, current_args))
                || contains_recursive_polymorphic_self(ret, origin_name, current_args)
                || captures
                    .iter()
                    .any(|c| contains_recursive_polymorphic_self(c, origin_name, current_args))
        }
        ResolvedTy::Pointer { pointee, .. } | ResolvedTy::Borrow { pointee } => {
            contains_recursive_polymorphic_self(pointee, origin_name, current_args)
        }
        ResolvedTy::Task(inner) => {
            contains_recursive_polymorphic_self(inner, origin_name, current_args)
        }
        _ => false,
    }
}

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
        let label = ResolvedTy::named_user("Label", vec![]);
        assert_eq!(mangle("describe", &[label]), "describe$$Label");
    }

    #[test]
    fn mangle_module_qualified_strips_colons() {
        let ty = ResolvedTy::named_user("widgets::Label", vec![]);
        assert_eq!(mangle("describe", &[ty]), "describe$$widgets_Label");
    }

    #[test]
    fn registry_dedupes_identical_keys() {
        let mut reg = MonoRegistry::with_cap(8);
        let key = MonoKey {
            origin: ItemId(0),
            origin_name: "id".into(),
            type_args: vec![ResolvedTy::I64],
        };
        assert_eq!(reg.insert(key.clone()), Ok(true));
        assert_eq!(reg.insert(key), Ok(false));
        assert_eq!(reg.into_vec().len(), 1);
    }

    #[test]
    fn substitute_replaces_bare_type_param() {
        let params = vec!["T".to_string()];
        let args = vec![ResolvedTy::I64];
        let ty = ResolvedTy::named_user("T", vec![]);
        assert_eq!(substitute_type_params(&ty, &params, &args), ResolvedTy::I64);
    }

    #[test]
    fn substitute_descends_into_nested_named() {
        // Vec<T> with T=i64 -> Vec<i64>
        let params = vec!["T".to_string()];
        let args = vec![ResolvedTy::I64];
        let ty = ResolvedTy::named_builtin(
            "Vec",
            hew_types::BuiltinType::Vec,
            vec![ResolvedTy::named_user("T", vec![])],
        );
        assert_eq!(
            substitute_type_params(&ty, &params, &args),
            ResolvedTy::named_builtin("Vec", hew_types::BuiltinType::Vec, vec![ResolvedTy::I64])
        );
    }

    #[test]
    fn substitute_leaves_unrelated_named_alone() {
        let params = vec!["T".to_string()];
        let args = vec![ResolvedTy::I64];
        let ty = ResolvedTy::named_user("Label", vec![]);
        assert_eq!(substitute_type_params(&ty, &params, &args), ty);
    }

    #[test]
    fn record_layout_registry_dedupes_identical_keys() {
        let mut reg = RecordLayoutRegistry::with_cap(8);
        let key = RecordMonoKey {
            origin: ItemId(0),
            origin_name: "Box".into(),
            type_args: vec![ResolvedTy::I64],
        };
        let fields = vec![("value".to_string(), ResolvedTy::I64)];
        assert_eq!(reg.insert(key.clone(), fields.clone(), 0..0), Ok(true));
        assert_eq!(reg.insert(key, fields, 0..0), Ok(false));
        assert_eq!(reg.into_vec().len(), 1);
    }

    #[test]
    fn record_layout_mangles_match_fn_scheme() {
        // The fn-mangling and record-mangling schemes share `mangle()`,
        // so a record `Box<i64>` and a hypothetical fn `Box<i64>` mangle
        // the same. This is intentional — codegen disambiguates by the
        // declaration kind (a record symbol is never called as a fn),
        // and the shared scheme means cross-IR debugging tooling can
        // round-trip a single mangler.
        assert_eq!(mangle("Box", &[ResolvedTy::I64]), "Box$$i64");
        assert_eq!(
            mangle("Pair", &[ResolvedTy::I64, ResolvedTy::String]),
            "Pair$$i64$string"
        );
    }

    #[test]
    fn recursive_polymorphic_self_detects_different_args() {
        // Node<T> with field `next: Box<Node<int>>` — the field
        // mentions Node with a different arg set than T.
        let current_args = vec![ResolvedTy::named_user("T", vec![])];
        let field_ty = ResolvedTy::named_user(
            "Box",
            vec![ResolvedTy::named_user("Node", vec![ResolvedTy::I64])],
        );
        assert!(contains_recursive_polymorphic_self(
            &field_ty,
            "Node",
            &current_args
        ));
    }

    #[test]
    fn recursive_polymorphic_self_ignores_matching_args() {
        // Box<T> with field `next: Box<T>` — same args, not a
        // polymorphic-recursion hazard (the layout converges).
        let current_args = vec![ResolvedTy::named_user("T", vec![])];
        let field_ty = ResolvedTy::named_user("Box", vec![ResolvedTy::named_user("T", vec![])]);
        assert!(!contains_recursive_polymorphic_self(
            &field_ty,
            "Box",
            &current_args
        ));
    }

    #[test]
    fn registry_cap_exceeded_emits_err() {
        let mut reg = MonoRegistry::with_cap(2);
        let mut overflowed = false;
        for i in 0..3 {
            let key = MonoKey {
                origin: ItemId(0),
                origin_name: "id".into(),
                type_args: vec![ResolvedTy::named_user(format!("T{i}"), vec![])],
            };
            if reg.insert(key).is_err() {
                overflowed = true;
            }
        }
        assert!(overflowed, "expected an insert to overflow the cap");
        assert_eq!(reg.into_vec().len(), 2);
    }

    // ── EnumLayoutRegistry tests ─────────────────────────────────────────────

    fn option_key(type_arg: ResolvedTy) -> EnumMonoKey {
        EnumMonoKey {
            origin: ItemId(10),
            origin_name: "Option".into(),
            type_args: vec![type_arg],
        }
    }

    fn some_variant(field_ty: ResolvedTy) -> EnumVariantLayout {
        EnumVariantLayout {
            name: "Some".into(),
            field_tys: vec![field_ty],
        }
    }

    fn none_variant() -> EnumVariantLayout {
        EnumVariantLayout {
            name: "None".into(),
            field_tys: vec![],
        }
    }

    #[test]
    fn enum_layout_registry_dedupes_identical_keys() {
        let mut reg = EnumLayoutRegistry::with_cap(8);
        let key = option_key(ResolvedTy::I64);
        let variants = vec![some_variant(ResolvedTy::I64), none_variant()];
        assert_eq!(reg.insert(key.clone(), variants.clone()), Ok(true));
        assert_eq!(reg.insert(key, variants), Ok(false));
        assert_eq!(reg.into_vec().len(), 1);
    }

    #[test]
    fn enum_layout_registry_two_distinct_keys_land_separately() {
        let mut reg = EnumLayoutRegistry::with_cap(8);
        assert_eq!(
            reg.insert(
                option_key(ResolvedTy::I64),
                vec![some_variant(ResolvedTy::I64), none_variant()],
            ),
            Ok(true)
        );
        assert_eq!(
            reg.insert(
                option_key(ResolvedTy::String),
                vec![some_variant(ResolvedTy::String), none_variant()],
            ),
            Ok(true)
        );
        let entries = reg.into_vec();
        assert_eq!(entries.len(), 2);
    }

    fn result_key(err_ty: ResolvedTy) -> EnumMonoKey {
        EnumMonoKey {
            origin: ItemId(11),
            origin_name: "Result".into(),
            type_args: vec![ResolvedTy::String, err_ty],
        }
    }

    #[test]
    fn enum_layout_registry_collapses_qualified_and_bare_payload() {
        // C1 stamps an authoritative module qualifier onto imported type
        // references, so the SAME generic enum instantiation reached through
        // an import-use site (`Result<string, fs.IoError>`) and through the
        // declaring module (`Result<string, IoError>`) arrive with divergent
        // payload spellings of one type. The registry must normalise the
        // type-arg spine to bare names — exactly as the codegen layout lookup
        // does — so both collapse to one entry under one mangled key. Without
        // this, the qualified spelling registers `Result$$string$fs.IoError`
        // while codegen probes `Result$$string$IoError`, the miss falling
        // through to the D10 fail-closed gate.
        let mut reg = EnumLayoutRegistry::with_cap(8);
        let bare_err = ResolvedTy::named_user("IoError", vec![]);
        let qualified_err = ResolvedTy::named_user("fs.IoError", vec![]);
        assert_eq!(
            reg.insert(
                result_key(qualified_err),
                vec![some_variant(ResolvedTy::String), none_variant()],
            ),
            Ok(true)
        );
        // Second insert with the bare spelling is recognised as the same key.
        assert_eq!(
            reg.insert(
                result_key(bare_err),
                vec![some_variant(ResolvedTy::String), none_variant()],
            ),
            Ok(false)
        );
        let entries = reg.into_vec();
        assert_eq!(entries.len(), 1);
        // The surviving entry is keyed by the bare-normalised mangle, the same
        // key the codegen enum-layout lookup produces.
        assert_eq!(entries[0].mangled_name, "Result$$string$IoError");
        assert_eq!(
            entries[0].key.type_args[1],
            ResolvedTy::named_user("IoError", vec![])
        );
    }

    #[test]
    fn shorten_named_arg_qualifiers_strips_nested_qualified_payload() {
        // A NESTED qualified payload must be shortened at every depth, not just
        // at the top-level type args. `Result<Vec<fs.Foo>, _>` registered under
        // a bare key but probed under a qualified key was the second asymmetry
        // C1 introduced: a `Named`-only shortener that did not recurse the
        // Array/Slice/Tuple/Vec-arg spine would leave the inner `fs.Foo`
        // qualified and the keys would diverge.
        let nested = ResolvedTy::named_user(
            "Result",
            vec![
                ResolvedTy::named_user("Vec", vec![ResolvedTy::named_user("fs.Foo", vec![])]),
                ResolvedTy::Tuple(vec![
                    ResolvedTy::Slice(Box::new(ResolvedTy::named_user("net.Conn", vec![]))),
                    ResolvedTy::Array(Box::new(ResolvedTy::named_user("io.Buf", vec![])), 4),
                ]),
            ],
        );
        let shortened = shorten_named_arg_qualifiers(nested);
        let expected = ResolvedTy::named_user(
            "Result",
            vec![
                ResolvedTy::named_user("Vec", vec![ResolvedTy::named_user("Foo", vec![])]),
                ResolvedTy::Tuple(vec![
                    ResolvedTy::Slice(Box::new(ResolvedTy::named_user("Conn", vec![]))),
                    ResolvedTy::Array(Box::new(ResolvedTy::named_user("Buf", vec![])), 4),
                ]),
            ],
        );
        assert_eq!(shortened, expected);
        // The mangle of the shortened spine carries no module qualifier — the
        // byte form every codegen lookup probes.
        assert_eq!(
            mangle("Result", &[shortened]),
            "Result$$Result_Vec_Foo_tuple_slice_Conn_array_Buf_4",
        );
    }

    #[test]
    fn enum_layout_registry_collapses_nested_qualified_payload() {
        // The registry collapse must also hold when the qualified `Named` is
        // NESTED inside another generic arg (`Option<Vec<fs.Foo>>`). The same
        // instantiation reached via the declaring module (`Vec<Foo>`) and via
        // an import-use site (`Vec<fs.Foo>`) must land on one entry under the
        // bare-normalised mangled key, matching the codegen lookup spine.
        fn option_vec_key(elem: ResolvedTy) -> EnumMonoKey {
            EnumMonoKey {
                origin: ItemId(12),
                origin_name: "Option".into(),
                type_args: vec![ResolvedTy::named_user("Vec", vec![elem])],
            }
        }
        let mut reg = EnumLayoutRegistry::with_cap(8);
        let qualified = ResolvedTy::named_user("fs.Foo", vec![]);
        let bare = ResolvedTy::named_user("Foo", vec![]);
        assert_eq!(
            reg.insert(
                option_vec_key(qualified),
                vec![some_variant(ResolvedTy::I64), none_variant()],
            ),
            Ok(true)
        );
        assert_eq!(
            reg.insert(
                option_vec_key(bare),
                vec![some_variant(ResolvedTy::I64), none_variant()],
            ),
            Ok(false)
        );
        let entries = reg.into_vec();
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].mangled_name, "Option$$Vec_Foo");
    }

    #[test]
    fn enum_layout_registry_into_vec_preserves_insertion_order() {
        let mut reg = EnumLayoutRegistry::with_cap(8);
        // Insert string first, then i64 — into_vec must return them in
        // insertion order (string at index 0, i64 at index 1).
        reg.insert(
            option_key(ResolvedTy::String),
            vec![some_variant(ResolvedTy::String), none_variant()],
        )
        .unwrap();
        reg.insert(
            option_key(ResolvedTy::I64),
            vec![some_variant(ResolvedTy::I64), none_variant()],
        )
        .unwrap();
        let entries = reg.into_vec();
        assert_eq!(entries[0].key.type_args[0], ResolvedTy::String);
        assert_eq!(entries[1].key.type_args[0], ResolvedTy::I64);
    }

    #[test]
    fn enum_layout_registry_cap_exceeded_emits_err() {
        let mut reg = EnumLayoutRegistry::with_cap(2);
        let mut overflowed = false;
        for ty in [ResolvedTy::I64, ResolvedTy::I32, ResolvedTy::Bool] {
            let key = option_key(ty.clone());
            let variants = vec![some_variant(ty), none_variant()];
            if reg.insert(key, variants).is_err() {
                overflowed = true;
            }
        }
        assert!(overflowed, "expected an insert to overflow the cap");
        assert_eq!(reg.into_vec().len(), 2);
    }

    #[test]
    fn enum_layout_mangled_name_uses_shared_scheme() {
        // The EnumLayoutRegistry uses `mangle(origin_name, type_args)` so
        // `Option<i64>` becomes `Option$$i64` — same scheme as fn/record.
        let mut reg = EnumLayoutRegistry::with_cap(8);
        let key = option_key(ResolvedTy::I64);
        reg.insert(key, vec![some_variant(ResolvedTy::I64), none_variant()])
            .unwrap();
        let entries = reg.into_vec();
        assert_eq!(entries[0].mangled_name, "Option$$i64");
    }
}

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
//! LESSONS: `producer-bridge-before-codegen` (P1), `checker-authority` (P0).

use std::collections::HashMap;
use std::fmt::Write;

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
    debug_assert!(
        !origin_name.contains('$'),
        "Hew identifiers cannot contain `$` (lexer rule); origin name `{origin_name}` is malformed"
    );
    let mut out = String::with_capacity(origin_name.len() + 8 * type_args.len());
    out.push_str(origin_name);
    out.push_str("$$");
    for (i, ty) in type_args.iter().enumerate() {
        if i > 0 {
            out.push('$');
        }
        let _ = write!(out, "{}", mangle_resolved_ty(ty));
    }
    out
}

/// Render a single `ResolvedTy` as a mangled fragment.
///
/// Uses `_` as the nested separator so a top-level `$`-separated mangle
/// can recover individual args by splitting on `$` alone. Returns names
/// that are stable across runs (no hash, no monotonic counter).
#[must_use]
fn mangle_resolved_ty(ty: &ResolvedTy) -> String {
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
        ResolvedTy::Named { name, args } => {
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
        let label = ResolvedTy::Named {
            name: "Label".into(),
            args: vec![],
        };
        assert_eq!(mangle("describe", &[label]), "describe$$Label");
    }

    #[test]
    fn mangle_module_qualified_strips_colons() {
        let ty = ResolvedTy::Named {
            name: "widgets::Label".into(),
            args: vec![],
        };
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
    fn registry_cap_exceeded_emits_err() {
        let mut reg = MonoRegistry::with_cap(2);
        let mut overflowed = false;
        for i in 0..3 {
            let key = MonoKey {
                origin: ItemId(0),
                origin_name: "id".into(),
                type_args: vec![ResolvedTy::Named {
                    name: format!("T{i}"),
                    args: vec![],
                }],
            };
            if reg.insert(key).is_err() {
                overflowed = true;
            }
        }
        assert!(overflowed, "expected an insert to overflow the cap");
        assert_eq!(reg.into_vec().len(), 2);
    }
}

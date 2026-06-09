//! Canonical instantiation-mangling helper, [`SymbolClass`]
//! discriminator, and placeholder [`ConstValue`] type.
//!
//! ## Mangling scheme
//!
//! [`mangle_instantiation`] produces an LLVM-safe symbol from an origin
//! name plus concrete type and const arguments, prefixed by a
//! class-tag derived from [`SymbolClass`]:
//!
//! - [`SymbolClass::Function`] (empty class prefix): `<name>$$<args>`
//!   — byte-identical to the legacy [`crate::monomorph::mangle`]
//!   output for the same `(name, type_args)` input, and with empty
//!   `const_args` produces no additional suffix. The legacy helper is
//!   now a thin wrapper over this entry point.
//! - All other [`SymbolClass`] variants prefix the symbol with
//!   `<tag>$$` where `<tag>` is a short ASCII discriminator
//!   (`mc` for machine, `ac` for actor, etc.). The prefix ensures two
//!   instantiations sharing origin name + type args but differing in
//!   class produce distinct mangled symbols.
//! - When `const_args` is non-empty, a `$$c$` separator follows the
//!   type-arg segment, with each const arg rendered via
//!   [`mangle_const_value`].
//!
//! The `$` and `$$` sigils are chosen because the lexer's identifier
//! rule (`hew-lexer/src/lib.rs:451`) only admits `[a-zA-Z0-9_]`, so
//! `$` cannot collide with any user-written Hew identifier; LLVM IR
//! permits `$` in global symbol names.
//!
//! ## Reuse of the legacy rendering helper
//!
//! Per-`ResolvedTy` segment rendering is delegated to
//! [`crate::monomorph::mangle_resolved_ty`], which the legacy
//! [`crate::monomorph::mangle`] also uses — so individual type-arg
//! fragments are byte-identical between the legacy and new paths by
//! construction. Only the surrounding prefix/separator scheme is new.

use std::fmt::Write;

use hew_types::ResolvedTy;

use crate::monomorph::mangle_resolved_ty;

/// Replace every character that is not ASCII alphanumeric or `_` with `_`,
/// producing a string that is safe to embed in an LLVM global symbol name.
///
/// This is a local mirror of `hew_mir::sanitize_for_symbol`. Because
/// `hew-mir` depends on `hew-hir`, we cannot take a reverse dep to avoid a
/// cycle; the two implementations **must be kept byte-for-byte identical**.
/// The invariant is guarded by the integration test
/// `mono_foundation_byte_compat::non_function_origin_name_sanitized`.
///
/// The function is intentionally `pub(crate)` — external callers that need
/// symbol sanitization should go through `hew_mir::sanitize_for_symbol`.
#[must_use]
pub(crate) fn sanitize_for_symbol(s: &str) -> String {
    s.chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect()
}

/// Discriminator selecting which monomorphisation category a mangled
/// symbol belongs to.
///
/// The variant determines the class-prefix that
/// [`mangle_instantiation`] emits. Two keys with the same origin name
/// and identical type args but different `SymbolClass` always mangle
/// to distinct symbols.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum SymbolClass {
    /// Generic function monomorphisation. No class prefix is emitted —
    /// the output is byte-compatible with the legacy
    /// [`crate::monomorph::mangle`] helper.
    Function,
    /// Generic record / `pub type` monomorphisation.
    Record,
    /// Generic enum monomorphisation.
    Enum,
    /// Generic machine monomorphisation.
    Machine,
    /// Generic actor-spawn monomorphisation (consumed by the
    /// actor-spawn discovery subsystem).
    Actor,
    /// A `(trait, concrete-type)` pair used for static dispatch
    /// trampolines and vtable entries. Reserved for trait-related
    /// symbol unification work that consumes this helper.
    TraitConcrete,
}

impl SymbolClass {
    /// Short ASCII tag that prefixes the mangled symbol when the class
    /// is non-`Function`. Stable across releases — downstream tooling
    /// may depend on the tag string.
    #[must_use]
    pub fn tag(self) -> &'static str {
        match self {
            Self::Function => "",
            Self::Record => "rc",
            Self::Enum => "en",
            Self::Machine => "mc",
            Self::Actor => "ac",
            Self::TraitConcrete => "tc",
        }
    }
}

/// Placeholder no longer — the real [`ConstValue`] now lives in
/// [`super::machine`] and is re-exported here for back-compat with
/// existing import sites (`use hew_hir::mono::mangle::ConstValue`).
///
/// `mangle_instantiation` renders each variant via
/// [`mangle_const_value`]; new variants added to [`ConstValue`] must
/// extend `mangle_const_value`'s match without collision.
pub use super::machine::ConstValue;

/// Render a single [`ConstValue`] as a mangled symbol fragment.
///
/// Stable across releases for variants that exist today; new variants
/// must extend this match with a render that does not collide with
/// any existing variant's output. The match is exhaustive by intent —
/// a non-exhaustive match would silently mangle an unknown variant to
/// a default string and risk symbol collisions across const-arg shapes.
///
/// Encoding scheme:
/// - `ConstValue::Usize(n)` → `u<n>` (e.g. `u16`, `u0`, `u65535`)
#[must_use]
pub fn mangle_const_value(value: &ConstValue) -> String {
    match value {
        ConstValue::Usize(n) => format!("u{n}"),
    }
}

/// Canonical instantiation-mangling entry point.
///
/// Produces an LLVM-safe symbol from `(class, origin_name, type_args,
/// const_args)`. See the module-level documentation for the exact
/// scheme. Output is deterministic — no hashing, no monotonic
/// counters, no global state — so two callers with the same inputs
/// always produce the same symbol.
///
/// ## `origin_name` sanitization
///
/// For every [`SymbolClass`] **other than** [`SymbolClass::Function`],
/// `origin_name` is passed through [`sanitize_for_symbol`] before it
/// is emitted. This turns colons, dots, hyphens, and any other
/// character that is not ASCII alphanumeric or `_` into `_`, producing
/// link-safe symbols. For example `My::Module::Foo` becomes
/// `My__Module__Foo` in the final symbol.
///
/// [`SymbolClass::Function`] deliberately **bypasses** sanitization to
/// preserve byte-for-byte compatibility with the legacy
/// [`crate::monomorph::mangle`] output (which never sanitized origin
/// names). The 70-fixture byte-compat snapshot in
/// `hew-hir/tests/mono_foundation_byte_compat.rs` guards this invariant.
///
/// # Panics (debug builds only)
///
/// Asserts that `origin_name` contains no `$` character, since the
/// lexer's identifier rule forbids `$` in user identifiers; a `$` in
/// `origin_name` therefore indicates upstream corruption rather than a
/// legitimate input.
#[must_use]
pub fn mangle_instantiation(
    class: SymbolClass,
    origin_name: &str,
    type_args: &[ResolvedTy],
    const_args: &[ConstValue],
) -> String {
    debug_assert!(
        !origin_name.contains('$'),
        "Hew identifiers cannot contain `$` (lexer rule); origin name `{origin_name}` is malformed"
    );

    // For non-Function classes, sanitize the origin name so that
    // module-qualified names like `My::Module::Foo` do not produce
    // LLVM-unsafe `:` characters in the final symbol. Function class
    // is deliberately exempted to preserve legacy byte compatibility
    // (see doc comment above).
    let safe_origin: std::borrow::Cow<str> = if matches!(class, SymbolClass::Function) {
        std::borrow::Cow::Borrowed(origin_name)
    } else {
        std::borrow::Cow::Owned(sanitize_for_symbol(origin_name))
    };

    let class_tag = class.tag();
    let mut out = String::with_capacity(
        class_tag.len()
            + if class_tag.is_empty() { 0 } else { 2 }
            + safe_origin.len()
            + 8 * type_args.len()
            + if const_args.is_empty() {
                0
            } else {
                4 + 8 * const_args.len()
            },
    );

    if !class_tag.is_empty() {
        out.push_str(class_tag);
        out.push_str("$$");
    }

    out.push_str(&safe_origin);
    out.push_str("$$");
    for (i, ty) in type_args.iter().enumerate() {
        if i > 0 {
            out.push('$');
        }
        let _ = write!(out, "{}", mangle_resolved_ty(ty));
    }

    if !const_args.is_empty() {
        out.push_str("$$c$");
        for (i, cv) in const_args.iter().enumerate() {
            if i > 0 {
                out.push('$');
            }
            out.push_str(&mangle_const_value(cv));
        }
    }

    out
}

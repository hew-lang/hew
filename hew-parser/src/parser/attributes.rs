//! The closed attribute table (HEW-SPEC-2026 §12.6).
//!
//! The attribute set is closed: every `#[name]` the parser accepts, and every
//! position it is legal in, is listed in [`legal_positions`]. An attribute
//! whose name is not in the table, or that appears in a position the table
//! does not list for it, is `E_UNKNOWN_ATTRIBUTE` (User channel) — this is
//! the one diagnostic path every attribute-consuming call site in the parser
//! routes through via [`Parser::validate_attributes_for`].
//!
//! Substrate attributes (`lang_item`, `intrinsic`, `diagnostic_item`,
//! `overload`, `runtime_capability`, `abi`) are recognised here at the
//! positions they occupy in `std/`; restricting them to `std/` itself is a
//! separate, existing authority (`RESERVED_SUBSTRATE_ATTRIBUTES` in
//! `hew-types::stdlib_authority`) that runs after parsing. The two checks
//! compose: this table says a name+position pair is *known*, the authority
//! check says a substrate name is only *usable* inside `std/`.

#[allow(
    clippy::wildcard_imports,
    reason = "grammar-area submodules share the parent parser namespace via the split"
)]
use super::*;

/// A syntactic position an attribute can appear in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AttrPosition {
    /// `type Name { .. }`, `enum Name { .. }`, `indirect enum Name { .. }`.
    TypeDecl,
    /// A field inside a `type { .. }` body.
    Field,
    /// A free function (top-level `fn`, including `pub`/`package`).
    FreeFn,
    /// A `trait Name { .. }` declaration itself.
    TraitDecl,
    /// A method signature inside a `trait { .. }` body.
    TraitMethod,
    /// An `actor Name { .. }` declaration itself.
    ActorDecl,
    /// A plain `fn` inside an actor body (lifecycle hooks).
    ActorMemberFn,
    /// A `receive fn` inside an actor body.
    ActorReceiveFn,
    /// A method inside an `impl { .. }` block.
    ImplMethod,
    /// A `fn` inside an `extern "C" { .. }` block.
    ExternFn,
    /// A position no attribute is legal in (`const`, `import`, `supervisor`,
    /// `machine`, a type alias, a tuple-form record, an `impl`/`extern`
    /// block's own header, an inline method inside a `type { .. }` body, an
    /// actor `init`/`let`/`var` field). No table entry ever names this
    /// variant, so every attribute found here is unconditionally
    /// `E_UNKNOWN_ATTRIBUTE`.
    Unsupported,
}

/// Returns the positions `name` is legal in, or `None` if `name` is not a
/// recognised attribute at all.
fn legal_positions(name: &str) -> Option<&'static [AttrPosition]> {
    use AttrPosition::{
        ActorDecl, ActorMemberFn, ActorReceiveFn, ExternFn, Field, FreeFn, ImplMethod, TraitDecl,
        TraitMethod, TypeDecl,
    };

    // Substrate attributes share one broad position set: every declaration
    // shape `hew-types::stdlib_authority::AuthorityDeclarationKind` tracks
    // (Type, Trait, TraitMethod, Function, Method, ExternFunction). Whether a
    // given use is inside `std/` is validated separately after parsing.
    const SUBSTRATE_POSITIONS: &[AttrPosition] = &[
        TypeDecl,
        TraitDecl,
        TraitMethod,
        FreeFn,
        ImplMethod,
        ExternFn,
    ];

    Some(match name {
        "resource" | "linear" | "opaque" | "json" | "yaml" | "deprecated" => &[TypeDecl],
        "wire" => &[TypeDecl, Field],
        // `#[ignore]`/`#[should_panic]`/`#[serial]` structurally sit at
        // `FreeFn` like `#[test]` and `#[export]`; the co-occurrence half of
        // their `#[test]`-only rule is enforced by
        // `Parser::validate_attributes_for`, not by this table.
        "test" | "export" | "ignore" | "should_panic" | "serial" => &[FreeFn],
        "on" => &[ActorMemberFn],
        "every" => &[ActorReceiveFn],
        "max_heap" => &[ActorDecl],
        "extern_symbol" => &[ImplMethod, ExternFn],
        // Not gated to `std/` by `RESERVED_SUBSTRATE_ATTRIBUTES` today (it is
        // used by non-stdlib code, e.g.
        // `tests/vertical-slice/accept/returns_receiver_consuming_result.hew`),
        // so it is listed as an ordinary attribute rather than folded into
        // `SUBSTRATE_POSITIONS`.
        "returns_receiver" => &[TraitMethod, ImplMethod],
        "lang_item" | "intrinsic" | "diagnostic_item" | "overload" | "runtime_capability"
        | "abi" => SUBSTRATE_POSITIONS,
        _ => return None,
    })
}

impl Parser<'_> {
    /// Validate every attribute in `attrs` against the closed table for
    /// `position`, emitting `E_UNKNOWN_ATTRIBUTE` for any name the table does
    /// not know, or that is not legal in `position`.
    ///
    /// This is the one diagnostic path every attribute-consuming call site
    /// routes through — no call site keeps its own bespoke allowlist.
    pub(crate) fn validate_attributes_for(&mut self, attrs: &[Attribute], position: AttrPosition) {
        let has_test = attrs.iter().any(|a| a.name == "test");
        for attr in attrs {
            let in_table = legal_positions(&attr.name).is_some_and(|p| p.contains(&position));
            // `#[ignore]`, `#[should_panic]`, and `#[serial]` structurally sit
            // at `FreeFn` like `#[test]` itself, but §12.6 only legalises them
            // on a function that also carries `#[test]` — a misspelled
            // `#[test]` must not leave them silently accepted either.
            let requires_test = matches!(attr.name.as_str(), "ignore" | "should_panic" | "serial");
            let legal = in_table && (!requires_test || has_test);
            if !legal {
                self.error_at(
                    format!(
                        "unrecognised attribute `#[{}]` in this position [E_UNKNOWN_ATTRIBUTE]",
                        attr.name
                    ),
                    attr.span.clone(),
                );
            }
        }
    }
}

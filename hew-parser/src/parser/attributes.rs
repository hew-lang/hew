//! Built-in attribute names and their legal positions. Other names resolve during macro expansion.

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
    /// An interceptor function inside a supervisor body.
    SupervisorMemberFn,
    /// A `receive fn` inside an actor body.
    ActorReceiveFn,
    /// A method inside an `impl { .. }` block.
    ImplMethod,
    /// A `fn` inside an `extern "C" { .. }` block.
    ExternFn,
    /// A position no attribute is legal in (`const`, `import`, `supervisor`,
    /// `machine`, a type alias, a tuple-form record, an `impl`/`extern`
    /// block's own header, an actor `init`/`let`/`var` field).
    /// No table entry ever names this
    /// variant, so every attribute found here is unconditionally
    /// `E_UNKNOWN_ATTRIBUTE`.
    Unsupported,
}

/// Returns the positions `name` is legal in, or `None` if `name` is not a
/// recognised attribute at all.
fn legal_positions(name: &str) -> Option<&'static [AttrPosition]> {
    use AttrPosition::{
        ActorDecl, ActorMemberFn, ActorReceiveFn, ExternFn, Field, FreeFn, ImplMethod,
        SupervisorMemberFn, TraitDecl, TraitMethod, TypeDecl,
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
        "wire" => &[TypeDecl, Field, ActorReceiveFn],
        // `#[ignore]`/`#[should_panic]`/`#[serial]`/`#[real_time]` sit at
        // `FreeFn` like `#[test]` and `#[export]`; the co-occurrence half of
        // their `#[test]`-only rule is enforced by
        // `Parser::validate_attributes_for`, not by this table.
        "test" | "bench" | "export" | "ignore" | "should_panic" | "real_time" | "timeout" => {
            &[FreeFn]
        }
        // On a test fn `#[serial]` runs it alone; on a type or field it sets
        // text keys (`case`, `key`, `skip`).
        "serial" => &[FreeFn, TypeDecl, Field],
        "on" => &[ActorMemberFn],
        "intercept" => &[ActorMemberFn, SupervisorMemberFn],
        "every" => &[ActorReceiveFn],
        "max_heap" => &[ActorDecl],
        "confined" => &[FreeFn, ActorDecl],
        "extern_symbol" => &[ImplMethod, ExternFn],
        "offload" => &[ExternFn],
        "runtime" => &[ImplMethod],
        // Not gated to `std/` by `RESERVED_SUBSTRATE_ATTRIBUTES` today (it is
        // used by non-stdlib code, e.g.
        // `tests/vertical-slice/accept/returns_receiver_consuming_result.hew`),
        // so it is listed as an ordinary attribute rather than folded into
        // `SUBSTRATE_POSITIONS`.
        "returns_receiver" => &[TraitMethod, ImplMethod],
        "lang_item" | "intrinsic" | "runtime_capability" => SUBSTRATE_POSITIONS,
        _ => return None,
    })
}

#[must_use]
pub fn is_builtin_attribute(name: &str) -> bool {
    legal_positions(name).is_some()
}

impl Parser<'_> {
    /// Validate literal shapes before positional strings lose their token kind.
    pub(crate) fn validate_test_attribute_arguments(
        &mut self,
        attr: &Attribute,
        argument_count: usize,
        quoted_arguments: bool,
    ) {
        let (valid, hint) = match attr.name.as_str() {
            "test" | "bench" | "real_time" | "offload" | "confined" => {
                (argument_count == 0, "remove the arguments")
            }
            "ignore" | "should_panic" => (
                argument_count <= 1 && quoted_arguments,
                "use no argument or one quoted string, such as (\"reason\")",
            ),
            "timeout" => (
                matches!(attr.args.as_slice(), [AttributeArg::Duration(ns)] if *ns > 0),
                "use one positive duration literal, such as #[timeout(5s)]",
            ),
            _ => return,
        };
        if !valid {
            self.error_at_with_kind_and_hint(
                format!(
                    "invalid arguments for `#[{}]` [E_ATTRIBUTE_ARGUMENT]",
                    attr.name
                ),
                attr.span.clone(),
                hint,
                ParseDiagnosticKind::AttributeArgument,
            );
        }
    }

    /// Validate every attribute in `attrs` against the closed table for
    /// `position`, emitting `E_UNKNOWN_ATTRIBUTE` for any name the table does
    /// not know, or that is not legal in `position`.
    ///
    /// This is the one diagnostic path every attribute-consuming call site
    /// routes through — no call site keeps its own bespoke allowlist.
    pub(crate) fn validate_attributes_for(&mut self, attrs: &[Attribute], position: AttrPosition) {
        let has_test = attrs.iter().any(|a| a.name == "test" || a.name == "bench");
        for attr in attrs {
            if !is_builtin_attribute(&attr.name) {
                if self.expander.is_some()
                    && (!matches!(
                        position,
                        AttrPosition::FreeFn
                            | AttrPosition::ImplMethod
                            | AttrPosition::TraitMethod
                            | AttrPosition::ActorMemberFn
                    ) || attrs.iter().any(|attr| attr.name == "on"))
                {
                    if let [AttributeArg::Tokens(call)] = attr.args.as_slice() {
                        let resolved = self
                            .expander
                            .as_ref()
                            .unwrap()
                            .borrow()
                            .resolve_attribute(call, &self.macro_source);
                        if let Err(message) = resolved {
                            self.error_at(message, attr.span.clone());
                            continue;
                        }
                    }
                    let hint = if position == AttrPosition::ActorReceiveFn {
                        "; use #[intercept] for receive handlers"
                    } else {
                        ""
                    };
                    self.error_at(format!("E_ATTRIBUTE_MACRO_POSITION: attribute macros require a function body{hint}"), attr.span.clone());
                }
                continue;
            }
            if attr.name == "bench" && attrs.iter().any(|other| other.name == "test") {
                self.error_at_with_kind_and_hint(
                    "a function cannot be both a test and a benchmark [E_ATTRIBUTE_ARGUMENT]"
                        .into(),
                    attr.span.clone(),
                    "keep either #[test] or #[bench]",
                    ParseDiagnosticKind::AttributeArgument,
                );
            }
            let in_table = legal_positions(&attr.name).is_some_and(|p| p.contains(&position));
            // `#[ignore]`, `#[should_panic]`, `#[serial]` and `#[real_time]` sit
            // at `FreeFn` like `#[test]` itself, but §12.6 only legalises them
            // on a function that also carries `#[test]` — a misspelled
            // `#[test]` must not leave them silently accepted either.
            let requires_test = matches!(
                attr.name.as_str(),
                "ignore" | "should_panic" | "real_time" | "timeout"
            ) || (attr.name == "serial" && position == AttrPosition::FreeFn);
            let legal = in_table && (!requires_test || has_test);
            if legal && attr.name == "serial" {
                match position {
                    AttrPosition::FreeFn if !attr.args.is_empty() => {
                        self.error_at_with_kind_and_hint(
                            "invalid arguments for `#[serial]` [E_ATTRIBUTE_ARGUMENT]".to_string(),
                            attr.span.clone(),
                            "remove the arguments",
                            ParseDiagnosticKind::AttributeArgument,
                        );
                    }
                    AttrPosition::Field => self.validate_serial_field(attr),
                    _ => {}
                }
            }
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

#[cfg(test)]
mod tests {
    use crate::{parse, ParseDiagnosticKind};

    #[test]
    fn test_attributes_preserve_valid_literal_arguments() {
        for attribute in [
            "#[test]",
            "#[ignore]",
            "#[ignore(\"reason\")]",
            "#[should_panic]",
            "#[should_panic(\"IndexOutOfBounds\")]",
            "#[should_panic(r\"message\")]",
            "#[serial]",
            "#[real_time]",
            "#[timeout(1ns)]",
            "#[timeout(5s)]",
        ] {
            let source = format!("#[test]\n{attribute}\nfn example() {{}}\n");
            let parsed = parse(&source);
            assert!(parsed.errors.is_empty(), "{attribute}: {:?}", parsed.errors);
        }
    }

    #[test]
    fn test_attribute_argument_errors_name_the_attribute_and_repair() {
        for attribute in [
            "#[test(timeout = \"1ms\")]",
            "#[serial(1)]",
            "#[real_time(yes)]",
            "#[ignore(reason)]",
            "#[ignore(1)]",
            "#[ignore(\"a\", \"b\")]",
            "#[should_panic(expected = \"x\")]",
            "#[should_panic(IndexOutOfBounds)]",
            "#[should_panic(\"x\", \"y\")]",
            "#[should_panic(5s)]",
            "#[timeout]",
            "#[timeout(0s)]",
            "#[timeout(\"5s\")]",
            "#[timeout(5)]",
            "#[timeout(1s, 2s)]",
            "#[timeout(value = 5)]",
        ] {
            let source = format!("#[test]\n{attribute}\nfn example() {{}}\n");
            let parsed = parse(&source);
            let error = parsed
                .errors
                .iter()
                .find(|error| error.kind == ParseDiagnosticKind::AttributeArgument)
                .unwrap_or_else(|| panic!("{attribute}: {:?}", parsed.errors));
            assert_eq!(&source[error.span.clone()], attribute);
            assert_eq!(error.kind.as_kind_str(), "E_ATTRIBUTE_ARGUMENT");
            assert!(error.hint.as_ref().is_some_and(|hint| !hint.is_empty()));
        }
        let parsed = parse("#[timeout(1s)] fn ordinary() {}");
        assert!(parsed
            .errors
            .iter()
            .any(|error| error.message.contains("E_UNKNOWN_ATTRIBUTE")));
    }
}

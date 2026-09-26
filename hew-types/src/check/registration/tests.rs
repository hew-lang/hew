//! Unit tests for `registration`, split out of the source file.
#![allow(
    unused_imports,
    clippy::wildcard_imports,
    clippy::module_inception,
    reason = "re-exported for nested test modules' glob imports"
)]
use super::*;
use hew_parser::ast::Ident;

#[cfg(test)]
mod canonical_type_publication_tests {
    use super::*;

    fn dog_type(methods: HashMap<String, FnSig>) -> TypeDef {
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Dog".to_string(),
            type_params: Vec::new(),
            bounds: HashMap::new(),
            fields: HashMap::new(),
            field_order: Vec::new(),
            variants: HashMap::new(),
            methods,
            doc_comment: None,
            is_indirect: false,
        }
    }

    #[test]
    fn canonical_refresh_preserves_accumulated_methods() {
        let mut checker = Checker::default();
        let __id = checker.test_declaration("greeting.Dog");
        checker.type_defs.insert(
            __id,
            dog_type(HashMap::from([
                ("greet".to_string(), FnSig::default()),
                ("name".to_string(), FnSig::default()),
            ])),
        );
        let source = dog_type(HashMap::from([(
            "name".to_string(),
            FnSig {
                return_type: Ty::String,
                ..FnSig::default()
            },
        )]));

        checker.register_canonical_type_def("greeting", "Dog", &source);

        let methods = &checker
            .type_def_view()
            .at_path("greeting.Dog")
            .unwrap()
            .methods;
        assert!(methods.contains_key("greet"));
        assert_eq!(methods["name"].return_type, Ty::String);
    }
}

#[cfg(test)]
mod node_builtin_catalog_tests {
    use super::*;
    use crate::runtime_call::RuntimeCallFamily;

    #[test]
    fn emitted_node_builtins_are_catalogued_for_wasm_rejection() {
        let mut checker = Checker::default();
        checker.register_builtins();

        let mut emitted: Vec<&str> = checker
            .sigs()
            .entries()
            .map(|(key, _)| key)
            .filter(|name| name.starts_with("Node::"))
            .collect();
        emitted.sort_unstable();

        assert_eq!(
            emitted,
            [
                "Node::connect",
                "Node::id",
                "Node::identity_key",
                "Node::lookup",
                "Node::register",
                "Node::shutdown",
                "Node::start",
            ]
        );

        for name in emitted {
            let family = RuntimeCallFamily::from_c_symbol(name)
                .unwrap_or_else(|| panic!("registered Node builtin {name:?} is not catalogued"));
            assert!(
                family.is_node_builtin(),
                "registered Node builtin {name:?} is not classified for wasm rejection"
            );
        }
    }
}

#[cfg(test)]
mod failure_surface_lockstep_tests {
    use super::FAILURE_HEW;

    /// R4 lockstep (M-5/M-7): the embedded `FAILURE_HEW` must declare the same
    /// type surface as the on-disk `std/failure.hew`, or module-graph and
    /// inline-test checking diverge. Pins the four type names the on-disk file
    /// declares — `CrashInfo`, `CrashAction`, `CrashNotification`, `CrashKind` —
    /// so adding/removing one from either copy without the other fails here.
    #[test]
    fn embedded_failure_surface_declares_full_type_set() {
        let parsed = hew_parser::parse(FAILURE_HEW);
        assert!(
            parsed.errors.is_empty(),
            "embedded FAILURE_HEW must parse cleanly: {:?}",
            parsed.errors
        );
        let declared: std::collections::HashSet<String> = parsed
            .program
            .items
            .iter()
            .filter_map(|(item, _)| match item {
                hew_parser::ast::Item::TypeDecl(decl) => Some(decl.name.to_string()),
                _ => None,
            })
            .collect();
        for expected in ["CrashInfo", "CrashAction", "CrashNotification", "CrashKind"] {
            assert!(
                declared.contains(expected),
                "embedded FAILURE_HEW is missing `{expected}` (drifted from std/failure.hew); \
                 declared: {declared:?}"
            );
        }
    }

    /// M-5: `CrashInfo` carries `code: i64` AND `message: string` in the
    /// embedded copy (the field-presence half of `wire-contract-test-presence`).
    #[test]
    fn embedded_crash_info_has_code_and_message_fields() {
        let parsed = hew_parser::parse(FAILURE_HEW);
        let crash_info = parsed
            .program
            .items
            .iter()
            .find_map(|(item, _)| match item {
                hew_parser::ast::Item::TypeDecl(decl) if decl.name.name.as_str() == "CrashInfo" => {
                    Some(decl)
                }
                _ => None,
            })
            .expect("embedded FAILURE_HEW must declare CrashInfo");
        let field_names: Vec<&str> = crash_info
            .body
            .iter()
            .filter_map(|item| match item {
                hew_parser::ast::TypeBodyItem::Field { name, .. } => Some(name.name.as_str()),
                _ => None,
            })
            .collect();
        assert_eq!(
            field_names,
            vec!["code", "message"],
            "CrashInfo must carry exactly `code` then `message`"
        );
    }
}

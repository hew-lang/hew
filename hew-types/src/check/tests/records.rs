#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

#[cfg(test)]
mod cross_module_same_name {
    use super::super::*;

    fn make_record(name: &str, fields: &[(&str, &str)]) -> TypeDecl {
        TypeDecl {
            visibility: Visibility::Pub,
            kind: TypeDeclKind::Struct,
            name: name.to_string(),
            type_params: None,
            where_clause: None,
            body: fields
                .iter()
                .map(|(field_name, ty_name)| TypeBodyItem::Field {
                    name: field_name.to_string(),
                    ty: (
                        TypeExpr::Named {
                            name: ty_name.to_string(),
                            type_args: None,
                        },
                        0..0,
                    ),
                    attributes: vec![],
                    doc_comment: None,
                    span: 0..0,
                })
                .collect(),
            doc_comment: None,
            wire: None,
            is_indirect: false,
            resource_marker: hew_parser::ast::ResourceMarker::None,
            is_opaque: false,
            consuming_methods: vec![],
        }
    }

    fn make_constructor_body(record_name: &str, field_name: &str) -> FnDecl {
        FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Pub,
            name: "ok".to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![(
                    Stmt::Let {
                        pattern: (Pattern::Identifier("thing".to_string()), 0..0),
                        ty: None,
                        value: Some((
                            Expr::StructInit {
                                name: record_name.to_string(),
                                fields: vec![(field_name.to_string(), make_int_literal(1, 0..0))],
                                type_args: None,
                                base: None,
                            },
                            0..0,
                        )),
                        else_block: None,
                    },
                    0..0,
                )],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
            consumes_self: false,
        }
    }

    #[test]
    fn construction_uses_module_local_same_named_record() {
        let root_id = ModuleId::root();
        let alpha_id = ModuleId::new(vec!["alpha".to_string()]);
        let beta_id = ModuleId::new(vec!["beta".to_string()]);

        let alpha_module = Module {
            id: alpha_id.clone(),
            items: vec![
                (Item::TypeDecl(make_record("Thing", &[("a", "i64")])), 0..10),
                (Item::Function(make_constructor_body("Thing", "a")), 10..20),
            ],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let beta_module = Module {
            id: beta_id.clone(),
            items: vec![(
                Item::TypeDecl(make_record("Thing", &[("p", "i64"), ("q", "i64")])),
                0..10,
            )],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let root_module = Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(root_module).unwrap();
        mg.add_module(alpha_module).unwrap();
        mg.add_module(beta_module).unwrap();
        mg.topo_order = vec![alpha_id, beta_id, root_id];

        let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        });

        assert!(
            output.errors.is_empty(),
            "module-local same-named records must not cross-contaminate at construction; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn qualified_construction_uses_imported_same_named_record() {
        let root_id = ModuleId::root();
        let alpha_id = ModuleId::new(vec!["alpha".to_string()]);
        let beta_id = ModuleId::new(vec!["beta".to_string()]);

        let alpha_module = Module {
            id: alpha_id.clone(),
            items: vec![
                (Item::TypeDecl(make_record("Thing", &[("a", "i64")])), 0..10),
                (
                    Item::Function(make_constructor_body("beta.Thing", "p")),
                    10..20,
                ),
            ],
            imports: vec![hew_parser::module::ModuleImport {
                target: beta_id.clone(),
                spec: None,
                span: 0..0,
            }],
            source_paths: vec![],
            doc: None,
        };
        let beta_module = Module {
            id: beta_id.clone(),
            items: vec![(Item::TypeDecl(make_record("Thing", &[("p", "i64")])), 0..10)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let root_module = Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(root_module).unwrap();
        mg.add_module(alpha_module).unwrap();
        mg.add_module(beta_module).unwrap();
        mg.topo_order = vec![beta_id, alpha_id, root_id];

        let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        });

        assert!(
            output.errors.is_empty(),
            "qualified imported records must not resolve to same-named local records; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn construction_rejects_ambiguous_same_named_imports() {
        let alpha_import = make_user_import(
            &["alpha"],
            Some(ImportSpec::Names(vec![ImportName {
                name: "Thing".to_string(),
                alias: None,
            }])),
            vec![(Item::TypeDecl(make_record("Thing", &[("a", "i64")])), 0..10)],
        );
        let beta_import = make_user_import(
            &["beta"],
            Some(ImportSpec::Names(vec![ImportName {
                name: "Thing".to_string(),
                alias: None,
            }])),
            vec![(
                Item::TypeDecl(make_record("Thing", &[("p", "i64"), ("q", "i64")])),
                0..10,
            )],
        );
        let root_fn = make_constructor_body("Thing", "a");

        let output = check_items(vec![
            (Item::Import(alpha_import), 0..0),
            (Item::Import(beta_import), 0..0),
            (Item::Function(root_fn), 0..0),
        ]);

        assert!(
            output
                .errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::AmbiguousType)),
            "ambiguous bare imports must fail closed instead of falling back to the wrong record; got: {:?}",
            output.errors
        );
        assert!(
            !output
                .errors
                .iter()
                .any(|e| e.message.contains("no field `a` on type `Thing`")),
            "ambiguous bare imports must not leak a misleading field error from the wrong record; got: {:?}",
            output.errors
        );
    }
}

// ---------------------------------------------------------------------------
// record admission (A-3)
// ---------------------------------------------------------------------------

mod record_admission {
    use super::*;

    #[test]
    fn construction_ok_named_record() {
        // Named-field construction with all required fields and correct types
        // must produce no errors.
        let output = check_source(
            r"
            record Point { x: i64, y: i64 }
            fn main() {
                let p = Point { x: 1, y: 2 };
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "valid record construction must not produce errors; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn construction_missing_field_rejected() {
        // Omitting a required field must produce a missing-field error.
        let output = check_source(
            r"
            record Point { x: i64, y: i64 }
            fn main() {
                let p = Point { x: 1 };
            }
            ",
        );
        let has_missing = output
            .errors
            .iter()
            .any(|e| e.message.contains("missing field") && e.message.contains('y'));
        assert!(
            has_missing,
            "omitting a required field must emit a missing-field error; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn construction_extra_field_rejected() {
        // Providing a field that does not exist in the record must produce an
        // undefined-field error.
        let output = check_source(
            r"
            record Point { x: i64, y: i64 }
            fn main() {
                let p = Point { x: 1, y: 2, z: 3 };
            }
            ",
        );
        let has_extra = output
            .errors
            .iter()
            .any(|e| e.message.contains('z') && e.message.contains("Point"));
        assert!(
            has_extra,
            "extra field must emit an undefined-field error; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn construction_wrong_type_rejected() {
        // A field initialised with the wrong type must produce a type-mismatch
        // error.
        let output = check_source(
            r#"
            record Point { x: i64, y: i64 }
            fn main() {
                let p = Point { x: "hello", y: 2 };
            }
            "#,
        );
        let has_mismatch = output.errors.iter().any(|e| {
            e.message.contains("string")
                || e.message.contains("type mismatch")
                || e.message.contains("expected")
        });
        assert!(
            has_mismatch,
            "wrong-typed field must emit a type error; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn field_read_ok() {
        // Field access on a valid record must resolve the field type and
        // produce no errors.
        let output = check_source(
            r"
            record Point { x: i64, y: i64 }
            fn main() {
                let p = Point { x: 10, y: 20 };
                let n: i64 = p.x;
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "valid field read must produce no errors; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn field_write_rejected() {
        // Assigning to a record field through an immutable binding must be rejected.
        let output = check_source(
            r"
            record Point { x: i64, y: i64 }
            fn main() {
                let p: Point = Point { x: 1, y: 2 };
                p.x = 5;
            }
            ",
        );
        let has_rejection = output
            .errors
            .iter()
            .any(|e| e.message.contains("record") || e.message.contains("immutable"));
        assert!(
            has_rejection,
            "field assignment on a record must be rejected; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn field_write_through_mutable_binding_accepted() {
        let output = check_source(
            r"
            record Point { x: i64, y: i64 }
            fn main() {
                var p: Point = Point { x: 1, y: 2 };
                p.x = 5;
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "field assignment through a mutable record binding must type-check; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn tuple_record_construction_ok() {
        // Tuple-positional construction `UserId(42)` must resolve to the
        // declared record type without errors.
        let output = check_source(
            r"
            record UserId(i64);
            fn make() -> UserId {
                UserId(42)
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "valid tuple-record construction must produce no errors; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn tuple_record_dot_zero_access_rejected() {
        // `.0` index-style access on a tuple record must be rejected (A-D2).
        // Fields map is empty; `check_field_access` will report an undefined-field
        // error for the synthesised field name `"0"`.
        let output = check_source(
            r"
            record UserId(i64);
            fn get_inner(id: UserId) -> i64 {
                id.0
            }
            ",
        );
        let has_error = !output.errors.is_empty();
        assert!(
            has_error,
            "`.0` access on a tuple record must produce an error; got: {:#?}",
            output.errors
        );
    }
}

/// Tests for functional-update syntax `R { field: v, ..base }`.
/// Slice A-5 of the primitives surface plan.
#[cfg(test)]
mod record {
    mod functional_update {
        use super::super::*;

        #[test]
        fn base_fills_unspecified_fields_accepted() {
            // `Point { x: 5, ..base }` where base is Point — checker accepts it
            // because `base` fills the missing `y` field.
            let output = check_source(
                r"
                record Point { x: i64, y: i64 }
                fn f(base: Point) -> Point {
                    Point { x: 5, ..base }
                }
                ",
            );
            assert!(
                output.errors.is_empty(),
                "functional update with matching base type must have no errors; got: {:#?}",
                output.errors
            );
        }

        #[test]
        fn all_fields_explicit_with_base_accepted() {
            // All fields listed explicitly plus base — still valid (explicit overrides).
            let output = check_source(
                r"
                record Point { x: i64, y: i64 }
                fn f(base: Point) -> Point {
                    Point { x: 1, y: 2, ..base }
                }
                ",
            );
            assert!(
                output.errors.is_empty(),
                "functional update overriding all fields must have no errors; got: {:#?}",
                output.errors
            );
        }

        #[test]
        fn no_explicit_fields_base_only_accepted() {
            // `Point { ..base }` — base fills all fields.
            let output = check_source(
                r"
                record Point { x: i64, y: i64 }
                fn f(base: Point) -> Point {
                    Point { ..base }
                }
                ",
            );
            assert!(
                output.errors.is_empty(),
                "functional update with zero explicit fields must have no errors; got: {:#?}",
                output.errors
            );
        }

        #[test]
        fn base_wrong_type_rejected() {
            // `Point { x: 5, ..other }` where `other` is a different type — must error.
            let output = check_source(
                r"
                record Point { x: i64, y: i64 }
                record Color { r: i64, g: i64, b: i64 }
                fn f(other: Color) -> Point {
                    Point { x: 5, ..other }
                }
                ",
            );
            let has_error = output
                .errors
                .iter()
                .any(|e| e.message.contains("functional-update base"));
            assert!(
                has_error,
                "functional update with wrong base type must produce a type error; got: {:#?}",
                output.errors
            );
        }

        #[test]
        fn missing_field_without_base_still_rejected() {
            // No functional update — missing field still an error.
            let output = check_source(
                r"
                record Point { x: i64, y: i64 }
                fn f() -> Point {
                    Point { x: 1 }
                }
                ",
            );
            let has_missing = output
                .errors
                .iter()
                .any(|e| e.message.contains("missing field") && e.message.contains('y'));
            assert!(
                has_missing,
                "missing field without base must still produce a missing-field error; got: {:#?}",
                output.errors
            );
        }

        #[test]
        fn explicit_field_type_wrong_rejected() {
            // Explicit field type mismatch must still be caught even when base is present.
            let output = check_source(
                r#"
                record Point { x: i64, y: i64 }
                fn f(base: Point) -> Point {
                    Point { x: "not-an-i64", ..base }
                }
                "#,
            );
            assert!(
                !output.errors.is_empty(),
                "wrong type for explicit field in functional update must produce an error; got: {:#?}",
                output.errors
            );
        }

        #[test]
        fn base_wrong_type_in_typed_let_rejected() {
            // Typed `let p: Point = Point { x: 5, ..other }` where `other` is a
            // different type — the check_against path must also validate the base.
            let output = check_source(
                r"
                record Point { x: i64, y: i64 }
                record Color { r: i64, g: i64, b: i64 }
                fn f(other: Color) {
                    let p: Point = Point { x: 5, ..other };
                }
                ",
            );
            let has_error = output
                .errors
                .iter()
                .any(|e| e.message.contains("functional-update base"));
            assert!(
                has_error,
                "wrong base type must be caught even with type annotation on let; got: {:#?}",
                output.errors
            );
        }
    }
}

/// Tests for numeric opt-out arithmetic: `.wrapping_*`, `.checked_*`, `.saturating_*`
/// on every integer width. Slice B-3 of the primitives surface plan.
#[cfg(test)]
mod methods {
    use super::*;

    mod integer_checked_wrapping_saturating {
        use super::*;

        // --- wrapping_* accepts same-width argument and returns same type ---

        #[test]
        fn wrapping_add_i32_ok() {
            let output = check_source(r"fn f(a: i32, b: i32) -> i32 { a.wrapping_add(b) }");
            assert!(
                output.errors.is_empty(),
                "wrapping_add(i32, i32) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn wrapping_sub_i64_ok() {
            let output = check_source(r"fn f(a: i64, b: i64) -> i64 { a.wrapping_sub(b) }");
            assert!(
                output.errors.is_empty(),
                "wrapping_sub(i64, i64) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn wrapping_mul_u8_ok() {
            let output = check_source(r"fn f(a: u8, b: u8) -> u8 { a.wrapping_mul(b) }");
            assert!(
                output.errors.is_empty(),
                "wrapping_mul(u8, u8) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn wrapping_add_u64_ok() {
            let output = check_source(r"fn f(a: u64, b: u64) -> u64 { a.wrapping_add(b) }");
            assert!(
                output.errors.is_empty(),
                "wrapping_add(u64, u64) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn wrapping_add_isize_ok() {
            let output = check_source(r"fn f(a: isize, b: isize) -> isize { a.wrapping_add(b) }");
            assert!(
                output.errors.is_empty(),
                "wrapping_add(isize, isize) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn wrapping_sub_usize_ok() {
            let output = check_source(r"fn f(a: usize, b: usize) -> usize { a.wrapping_sub(b) }");
            assert!(
                output.errors.is_empty(),
                "wrapping_sub(usize, usize) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn wrapping_mul_i16_ok() {
            let output = check_source(r"fn f(a: i16, b: i16) -> i16 { a.wrapping_mul(b) }");
            assert!(
                output.errors.is_empty(),
                "wrapping_mul(i16, i16) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn wrapping_add_u16_ok() {
            let output = check_source(r"fn f(a: u16, b: u16) -> u16 { a.wrapping_add(b) }");
            assert!(
                output.errors.is_empty(),
                "wrapping_add(u16, u16) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn wrapping_add_i8_ok() {
            let output = check_source(r"fn f(a: i8, b: i8) -> i8 { a.wrapping_add(b) }");
            assert!(
                output.errors.is_empty(),
                "wrapping_add(i8, i8) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn wrapping_add_u32_ok() {
            let output = check_source(r"fn f(a: u32, b: u32) -> u32 { a.wrapping_add(b) }");
            assert!(
                output.errors.is_empty(),
                "wrapping_add(u32, u32) should typecheck: {:#?}",
                output.errors
            );
        }

        // --- checked_* returns Option<W> ---

        #[test]
        fn checked_add_i32_returns_option() {
            let output = check_source(
                r"
                fn f(a: i32, b: i32) -> Option<i32> {
                    a.checked_add(b)
                }
                ",
            );
            assert!(
                output.errors.is_empty(),
                "checked_add returns Option<i32>: {:#?}",
                output.errors
            );
        }

        #[test]
        fn checked_sub_u64_returns_option() {
            let output = check_source(
                r"
                fn f(a: u64, b: u64) -> Option<u64> {
                    a.checked_sub(b)
                }
                ",
            );
            assert!(
                output.errors.is_empty(),
                "checked_sub returns Option<u64>: {:#?}",
                output.errors
            );
        }

        #[test]
        fn checked_mul_i64_returns_option() {
            let output = check_source(
                r"
                fn f(a: i64, b: i64) -> Option<i64> {
                    a.checked_mul(b)
                }
                ",
            );
            assert!(
                output.errors.is_empty(),
                "checked_mul returns Option<i64>: {:#?}",
                output.errors
            );
        }

        #[test]
        fn checked_add_usize_returns_option() {
            let output = check_source(
                r"
                fn f(a: usize, b: usize) -> Option<usize> {
                    a.checked_add(b)
                }
                ",
            );
            assert!(
                output.errors.is_empty(),
                "checked_add(usize) returns Option<usize>: {:#?}",
                output.errors
            );
        }

        // --- saturating_* ---

        #[test]
        fn saturating_add_i32_ok() {
            let output = check_source(r"fn f(a: i32, b: i32) -> i32 { a.saturating_add(b) }");
            assert!(
                output.errors.is_empty(),
                "saturating_add(i32) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn saturating_sub_u8_ok() {
            let output = check_source(r"fn f(a: u8, b: u8) -> u8 { a.saturating_sub(b) }");
            assert!(
                output.errors.is_empty(),
                "saturating_sub(u8) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn saturating_mul_i64_ok() {
            let output = check_source(r"fn f(a: i64, b: i64) -> i64 { a.saturating_mul(b) }");
            assert!(
                output.errors.is_empty(),
                "saturating_mul(i64) should typecheck: {:#?}",
                output.errors
            );
        }

        // --- Negative: mixed-width argument rejected ---

        #[test]
        fn wrapping_add_mixed_width_rejected() {
            let output = check_source(r"fn f(a: i32, b: i64) -> i32 { a.wrapping_add(b) }");
            assert!(
                !output.errors.is_empty(),
                "wrapping_add(i32, i64) must be a type error"
            );
        }

        #[test]
        fn checked_add_mixed_width_rejected() {
            let output = check_source(
                r"
                fn f(a: u8, b: i32) -> Option<u8> {
                    a.checked_add(b)
                }
                ",
            );
            assert!(
                !output.errors.is_empty(),
                "checked_add(u8, i32) must be a type error"
            );
        }

        // --- Negative: float receiver rejected ---

        #[test]
        fn wrapping_add_float_receiver_rejected() {
            let output = check_source(r"fn f(a: f32, b: f32) { a.wrapping_add(b); }");
            assert!(
                !output.errors.is_empty(),
                "wrapping_add on f32 must be an error"
            );
        }

        #[test]
        fn saturating_add_f64_receiver_rejected() {
            let output = check_source(r"fn f(a: f64, b: f64) { a.saturating_add(b); }");
            assert!(
                !output.errors.is_empty(),
                "saturating_add on f64 must be an error"
            );
        }

        // --- Negative: unknown op in family rejected ---

        #[test]
        fn wrapping_div_rejected() {
            let output = check_source(r"fn f(a: i32, b: i32) { a.wrapping_div(b); }");
            assert!(
                !output.errors.is_empty(),
                "wrapping_div must be rejected (div is out of B-3 scope)"
            );
        }

        #[test]
        fn saturating_neg_rejected() {
            let output = check_source(r"fn f(a: i32) { a.saturating_neg(); }");
            assert!(
                !output.errors.is_empty(),
                "saturating_neg must be rejected (not in scope)"
            );
        }

        // --- Negative: zero args rejected ---

        #[test]
        fn wrapping_add_no_arg_rejected() {
            let output = check_source(r"fn f(a: i32) { a.wrapping_add(); }");
            assert!(
                !output.errors.is_empty(),
                "wrapping_add() with no argument must be an arity error"
            );
        }

        // --- Negative: checked_add result cannot be used as bare W ---

        #[test]
        fn checked_add_result_is_not_bare_i32() {
            let output = check_source(r"fn f(a: i32, b: i32) -> i32 { a.checked_add(b) }");
            assert!(
                !output.errors.is_empty(),
                "checked_add returns Option<i32>, not i32 — must be a type error"
            );
        }
    }
}

// ── Associated-types — slice 1 (bounds + defaults end-to-end) ──────────────
//
// Trait-side `type Bar [: Bound] [= default]` declarations flow through
// `trait_info_from_decl_with_diagnostics` into `TraitInfo.associated_types`.
// Impl-side `type Bar = X;` flows through `build_impl_alias_entries` and is
// bound-checked by `check_assoc_type_bounds` in `enter_impl_scope` when
// `enforce = true`.
//
// These tests pin the user-visible behaviour of slice 1:
//   1. defaults fill missing impl bindings;
//   2. trait-side bounds on assoc types reject violating impls;
//   3. impl-side type params satisfy assoc-type bounds via the impl's own
//      `where`/`<T: Bound>` clauses (not via `current_function` plumbing);
//   4. missing-binding diagnostics fire when no default exists;
//   5. `Self::Bar` continues to resolve in trait method signatures;
//   6. duplicate `type Bar; type Bar;` in a trait body is rejected.
mod assoc_types_slice1 {
    use super::*;

    #[test]
    fn assoc_type_default_fills_missing_impl_binding() {
        // Trait declares `type Step = i32`; impl omits the binding.
        // Resolution must use the default rather than reporting "missing".
        let output = check_source(
            r"
            trait Counter {
                type Step = i32;
                fn step(val: Self) -> Self::Step;
            }

            type Tick {}

            impl Counter for Tick {
                fn step(val: Tick) -> i32 { 1 }
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "expected no errors; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn assoc_type_bound_violation_rejects_impl() {
        // Trait declares `type Out: Display`; impl binds it to a type
        // that does not implement Display. Must produce BoundsNotSatisfied.
        let (errors, _warnings) = parse_and_check_with_stdlib(
            r"
            trait Show {
                type Out: Display;
                fn show(val: Self) -> Self::Out;
            }

            type Widget {}

            type Plain {}

            impl Show for Widget {
                type Out = Plain;
                fn show(val: Widget) -> Plain { Plain {} }
            }
            ",
        );
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)
                    && e.message.contains("Display")
                    && e.message.contains("Plain")),
            "expected BoundsNotSatisfied citing Display and Plain; got: {errors:?}"
        );
    }

    #[test]
    fn assoc_type_bound_satisfied_by_impl_type_param() {
        // `impl<T: Display> Show for Holder<T> { type Out = T; }` — the
        // assoc-type binding is itself a type parameter that carries the
        // required bound. Must accept without spurious BoundsNotSatisfied.
        let (errors, _warnings) = parse_and_check_with_stdlib(
            r"
            trait Show {
                type Out: Display;
                fn show(val: Self) -> Self::Out;
            }

            type Holder<T> {
                value: T;
            }

            impl<T: Display> Show for Holder<T> {
                type Out = T;
                fn show(val: Holder<T>) -> T { val.value }
            }
            ",
        );
        let bound_errors: Vec<_> = errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
            .collect();
        assert!(
            bound_errors.is_empty(),
            "impl<T: Display> binding `type Out = T` must satisfy `type Out: Display`; \
             got bound errors: {bound_errors:?}; all errors: {errors:?}"
        );
    }

    #[test]
    fn assoc_type_missing_binding_diagnostic() {
        // Non-defaulted assoc type with no impl-side binding must be
        // diagnosed at impl-registration time.
        let output = check_source(
            r"
            trait Container {
                type Item;
                fn first(val: Self) -> Self::Item;
            }

            type Box {}

            impl Container for Box {
                fn first(val: Box) -> i64 { 0 }
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.message.contains("Item") && e.message.contains("associated type")),
            "expected missing-associated-type diagnostic citing `Item`; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn self_assoc_in_trait_method_signature_resolves() {
        // Regression: the existing `Self::Bar` prefix-match path
        // (resolution.rs ~line 613) survives the TraitInfo schema change.
        // The method's declared return type is `Self::Item`; the impl
        // binds `type Item = i64`. Checker must accept the impl's `next`
        // returning `Option<i64>` against `Option<Self::Item>`.
        let output = check_source(
            r"
            trait Iterator {
                type Item;
                fn next(var val: Self) -> Option<Self::Item>;
            }

            type Counter {
                value: i64;
            }

            impl Iterator for Counter {
                type Item = i64;
                fn next(var c: Counter) -> Option<i64> { Some(c.value) }
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "expected clean check; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn duplicate_assoc_type_in_trait_body_rejected() {
        // `type Bar; type Bar;` in a single trait body is a duplicate
        // declaration. Must report DuplicateDefinition.
        let output = check_source(
            r"
            trait Foo {
                type Bar;
                type Bar;
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::DuplicateDefinition)
                    && e.message.contains("Bar")),
            "expected DuplicateDefinition citing `Bar`; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn assoc_type_bound_skips_when_rhs_is_error() {
        // Regression: when the impl-side `type Out = <unresolvable>` itself
        // produces a `Ty::Error` (e.g. an undefined type), the bounds
        // checker must skip its trait-bound check so we don't pile a
        // cascading `BoundsNotSatisfied` on top of the primary error.
        let (errors, _warnings) = parse_and_check_with_stdlib(
            r"
            trait Show {
                type Out: Display;
                fn show(val: Self) -> Self::Out;
            }

            type Widget {}

            impl Show for Widget {
                type Out = Task<i64>;
                fn show(val: Widget) -> i64 { 0 }
            }
            ",
        );
        let bound_errors: Vec<_> = errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
            .collect();
        assert!(
            bound_errors.is_empty(),
            "Ty::Error RHS (from `Task<i64>` not being nameable) must suppress the \
             bound-check cascade; got bound errors: {bound_errors:?}; all errors: {errors:?}"
        );
        // Sanity: the primary error from the bad RHS must still fire.
        assert!(
            !errors.is_empty(),
            "expected the primary diagnostic from the unresolvable RHS to remain"
        );
    }

    #[test]
    fn assoc_type_bound_violation_in_default_points_at_default() {
        // When a trait body supplies a default that violates its own
        // declared bound (`type Out: Display = Plain`), the impl that
        // omits the binding inherits the (bad) default — and the
        // diagnostic must point at the default expression in the trait
        // body, not at the trait header or the impl header.
        let source = r"
            trait Show {
                type Out: Display = Plain;
                fn show(val: Self) -> Self::Out;
            }

            type Plain {}

            type Widget {}

            impl Show for Widget {
                fn show(val: Widget) -> Plain { Plain {} }
            }
            ";
        let (errors, _warnings) = parse_and_check_with_stdlib(source);
        let bound_err = errors
            .iter()
            .find(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
            .expect("expected BoundsNotSatisfied for default `Plain` vs `Display`");
        let snippet = &source[bound_err.span.clone()];
        assert_eq!(
            snippet, "Plain",
            "diagnostic span must cover the default `Plain` site, not the trait \
             or impl header; got {snippet:?} (full span {:?})",
            bound_err.span
        );
    }

    #[test]
    fn assoc_type_composite_generic_binding_fails_closed() {
        // `impl<T: Display> Show for Container<T> { type Out = Option<T>; }`
        // — the RHS is a composite generic (`Option<T>`), not a bare impl
        // type-param. Slice 1 deliberately does *not* yet propagate bounds
        // through composite shapes, so `Option<T>` falls through to
        // `type_satisfies_trait_bound`, which (correctly) reports that
        // `Option<T>` does not implement `Display`. This test pins the
        // fail-closed behaviour: we'd rather reject a maybe-valid impl than
        // silently accept an unverified bound. Slice 2 (composite bound
        // propagation) is the proper home for the relaxation.
        let (errors, _warnings) = parse_and_check_with_stdlib(
            r"
            trait Show {
                type Out: Display;
                fn show(val: Self) -> Self::Out;
            }

            type Container<T> {
                value: T;
            }

            impl<T: Display> Show for Container<T> {
                type Out = Option<T>;
                fn show(val: Container<T>) -> Option<T> { Some(val.value) }
            }
            ",
        );
        let bound_err = errors
            .iter()
            .find(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
            .expect(
                "slice 1 must fail closed on composite `type Out = Option<T>`; \
                 slice 2 is the proper home for composite bound propagation",
            );
        assert!(
            bound_err.message.contains("Option") && bound_err.message.contains("Display"),
            "expected BoundsNotSatisfied citing `Option` and `Display`; got: {bound_err:?}"
        );
    }

    // TODO(assoc-types slice 2 / parser): `TraitDecl` does not yet parse a
    // `where` clause at the trait header, so `trait Foo where Self::Bar:
    // Display { ... }` cannot be tested end-to-end. Once the parser
    // surfaces `TraitDecl.where_clause`, drop the `#[ignore]` and verify
    // the bound is enforced on impls whose `Self::Bar` binding does not
    // satisfy `Display`.
    #[test]
    #[ignore = "trait-header where-clause syntax not yet parsed; see TODO above"]
    fn assoc_type_where_clause_bound_enforced() {
        let (errors, _warnings) = parse_and_check_with_stdlib(
            r"
            trait Show where Self::Out: Display {
                type Out;
                fn show(val: Self) -> Self::Out;
            }

            type Widget {}

            type Plain {}

            impl Show for Widget {
                type Out = Plain;
                fn show(val: Widget) -> Plain { Plain {} }
            }
            ",
        );
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)
                    && e.message.contains("Display")
                    && e.message.contains("Plain")),
            "expected BoundsNotSatisfied citing Display and Plain; got: {errors:?}"
        );
    }
}

// ── Associated-types — slice 2 (T::Bar projection in generic signatures) ──
//
// Slice 1 closed `Self::Bar` end-to-end. Slice 2 adds the load-bearing piece:
// `T::Bar` projections where `T` is a generic type parameter with a trait
// bound that declares `Bar`. The parser already stringifies `T::Bar` into
// `TypeExpr::Named { name: "T::Bar" }`; this slice teaches the resolver to
// (a) materialise a deferred `Ty::AssocType` carrier, (b) collapse it at
// call-site monomorphisation when `T` becomes concrete, and (c) reject
// surfaces where `T` has no bound declaring `Bar`.
mod assoc_types_slice2 {
    use super::*;

    #[test]
    fn tbar_projection_in_fn_signature_resolves() {
        // Trait declares `type Item`; generic fn returns `I::Item`. With no
        // call site, the signature alone must check clean — the return type
        // is a deferred `Ty::AssocType` carrier.
        let output = check_source(
            r"
            trait Iterator {
                type Item;
                fn next(it: Self) -> Option<Self::Item>;
            }

            fn make<I: Iterator>(it: I) -> I::Item {
                it.next().unwrap()
            }
            ",
        );
        // The body itself uses `it.next()` which requires dispatch to a
        // trait method on a generic-typed receiver. Whether that
        // dispatches today is orthogonal; the *signature* must accept
        // `I::Item` without an UndefinedType diagnostic citing `I::Item`.
        assert!(
            !output
                .errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::UndefinedType)
                    && e.message.contains("I::Item")
                    && e.message.contains("no bounds")),
            "signature `I::Item` must resolve via the `Iterator` bound; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn tbar_substitutes_at_call_site() {
        // Calling `make(counter)` where `Counter: Iterator<Item = i32>`
        // must materialise the return type as `i32` (the impl's binding).
        let output = check_source(
            r"
            trait Iterator {
                type Item;
                fn next(var it: Self) -> Option<Self::Item>;
            }

            type Counter {
                value: i64;
            }

            impl Iterator for Counter {
                type Item = i64;
                fn next(var c: Counter) -> Option<i64> { Some(c.value) }
            }

            fn make<I: Iterator>(it: I) -> Option<I::Item> {
                it.next()
            }

            fn caller() -> Option<i64> {
                make(Counter { value: 1 })
            }
            ",
        );
        // No type-mismatch error: caller's `Option<i64>` annotation must
        // unify with `make`'s monomorphised return `Option<I::Item>` →
        // `Option<i64>` once `I = Counter` collapses via the impl's
        // `type Item = i64` binding.
        let mismatches: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. }))
            .collect();
        assert!(
            mismatches.is_empty(),
            "expected `make(counter)` to monomorphise to `Option<i64>`; got mismatches: {mismatches:?}; all errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn tbar_missing_bound_diagnostic() {
        // `fn bad<T>(_: T) -> T::Item` with no bound on `T`. The resolver
        // must reject with a typed diagnostic naming the missing-bound
        // surface — not silently treat `T::Item` as an opaque named type.
        let output = check_source(
            r"
            trait Iterator {
                type Item;
                fn next(it: Self) -> Option<Self::Item>;
            }

            fn bad<T>(it: T) -> T::Item {
                it.next().unwrap()
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::UndefinedType)
                    && e.message.contains("T::Item")
                    && (e.message.contains("no bounds") || e.message.contains("no trait bound"))),
            "expected projection-missing-bound diagnostic; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn tbar_in_generic_position_collect() {
        // The canonical motivating example: `Vec<I::Item>` in a return type.
        // The carrier nests inside Vec, and at call-site monomorphisation
        // the projection collapses to the impl's binding.
        let output = check_source(
            r"
            trait Iterator {
                type Item;
                fn next(var it: Self) -> Option<Self::Item>;
            }

            type Counter {}

            impl Iterator for Counter {
                type Item = i64;
                fn next(var c: Counter) -> Option<i64> { None }
            }

            fn collect<I: Iterator>(it: I) -> Vec<I::Item> {
                Vec::new()
            }

            fn caller() -> Vec<i64> {
                collect(Counter {})
            }
            ",
        );
        let mismatches: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. }))
            .collect();
        assert!(
            mismatches.is_empty(),
            "expected `Vec<I::Item>` to monomorphise to `Vec<i64>`; got mismatches: {mismatches:?}; all errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn tbar_unknown_assoc_diagnostic() {
        // `T: Iterator` declares `type Item`. `T::Other` references an
        // assoc name the trait does not declare. Must produce a typed
        // diagnostic citing the bounds in scope.
        let output = check_source(
            r"
            trait Iterator {
                type Item;
                fn next(it: Self) -> Option<Self::Item>;
            }

            fn bad<T: Iterator>(it: T) -> T::Other {
                it.next().unwrap()
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::UndefinedType)
                    && e.message.contains("Other")
                    && e.message.contains("Iterator")),
            "expected unknown-assoc diagnostic citing Other and Iterator; got: {:?}",
            output.errors
        );
    }

    // ── D4: UnknownTraitBoundShape — reject positional type args on bounds ─────
    //
    // `T: Eq<U>` is not a valid bound form in Hew. Positional type arguments
    // on trait bounds are silently erased by `collect_type_param_bounds`,
    // which would reduce `Eq<U>` to bare `Eq` without any diagnostic.
    // The validator fires before erasure and produces `UnknownTraitBoundShape`.

    /// `fn f<T, U>(x: T) where T: Eq<U>` must produce `UnknownTraitBoundShape`
    /// citing `Eq`. This is the canonical D4 regression test.
    #[test]
    fn unknown_trait_bound_shape_rejected_for_positional_type_arg_in_where_clause() {
        let output = check_source(
            r"
            fn f<T, U>(x: T) -> T where T: Eq<U> {
                x
            }
            fn main() {}
            ",
        );
        assert!(
            output.errors.iter().any(|e| matches!(
                &e.kind,
                TypeErrorKind::UnknownTraitBoundShape { trait_name }
                    if trait_name == "Eq"
            )),
            "expected UnknownTraitBoundShape for `Eq<U>` in where-clause; got: {:?}",
            output.errors
        );
    }

    /// `fn f<T: Eq<U>, U>(x: T) -> T` must also produce `UnknownTraitBoundShape`
    /// when the positional arg is an inline type-param bound (not a where-clause).
    #[test]
    fn unknown_trait_bound_shape_rejected_for_positional_type_arg_inline() {
        let output = check_source(
            r"
            fn f<T: Eq<U>, U>(x: T) -> T {
                x
            }
            fn main() {}
            ",
        );
        assert!(
            output.errors.iter().any(|e| matches!(
                &e.kind,
                TypeErrorKind::UnknownTraitBoundShape { trait_name }
                    if trait_name == "Eq"
            )),
            "expected UnknownTraitBoundShape for inline `T: Eq<U>`; got: {:?}",
            output.errors
        );
    }

    /// `impl<T: Eq<U>> Foo<T> { }` must produce `UnknownTraitBoundShape` for
    /// the inline bound `Eq<U>` on the impl type parameter.
    #[test]
    fn unknown_trait_bound_shape_rejected_for_impl_inline_type_param_bound() {
        let output = check_source(
            r"
            type Foo<T> { value: T; }
            impl<T: Eq<U>, U> Foo<T> { }
            fn main() {}
            ",
        );
        assert!(
            output.errors.iter().any(|e| matches!(
                &e.kind,
                TypeErrorKind::UnknownTraitBoundShape { trait_name }
                    if trait_name == "Eq"
            )),
            "expected UnknownTraitBoundShape for inline `T: Eq<U>` on impl; got: {:?}",
            output.errors
        );
    }

    /// `impl<T, U> Foo<T> where T: Eq<U>` must produce `UnknownTraitBoundShape`
    /// for the where-clause bound `Eq<U>` on the impl type parameter.
    #[test]
    fn unknown_trait_bound_shape_rejected_for_impl_where_clause_bound() {
        let output = check_source(
            r"
            type Foo<T> { value: T; }
            impl<T, U> Foo<T> where T: Eq<U> { }
            fn main() {}
            ",
        );
        assert!(
            output.errors.iter().any(|e| matches!(
                &e.kind,
                TypeErrorKind::UnknownTraitBoundShape { trait_name }
                    if trait_name == "Eq"
            )),
            "expected UnknownTraitBoundShape for where-clause `T: Eq<U>` on impl; got: {:?}",
            output.errors
        );
    }

    /// `machine M<T: Eq<U>> { }` must produce `UnknownTraitBoundShape` for
    /// the inline bound `Eq<U>` on the machine type parameter.
    #[test]
    fn unknown_trait_bound_shape_rejected_for_machine_type_param_bound() {
        let output = check_source(
            r"
            machine M<T: Eq<U>, U> { }
            fn main() {}
            ",
        );
        assert!(
            output.errors.iter().any(|e| matches!(
                &e.kind,
                TypeErrorKind::UnknownTraitBoundShape { trait_name }
                    if trait_name == "Eq"
            )),
            "expected UnknownTraitBoundShape for machine `T: Eq<U>`; got: {:?}",
            output.errors
        );
    }

    // ── extern "rt" validation ─────────────────────────────────────────────────
    //
    // `extern "rt"` declares JIT-visible runtime functions. Every symbol must
    // appear in the `stable` section of scripts/jit-symbol-classification.toml.
    // Unclassified symbols produce `ExternRtSymbolUnclassified`; classified
    // symbols are accepted. `extern "C"` is unchanged by this validation.

    fn make_extern_rt_block(symbols: &[&str]) -> Item {
        Item::ExternBlock(ExternBlock {
            abi: "rt".to_string(),
            functions: symbols
                .iter()
                .map(|name| ExternFnDecl {
                    attributes: Vec::new(),
                    name: name.to_string(),
                    params: vec![],
                    return_type: None,
                    is_variadic: false,
                    span: 0..name.len(),
                })
                .collect(),
        })
    }

    /// A classified `extern "rt"` symbol must not produce an error.
    #[test]
    fn extern_rt_classified_symbol_accepted() {
        // hew_sleep_ns is in the stable list.
        let extern_item = make_extern_rt_block(&["hew_sleep_ns"]);
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&Program {
            items: vec![(extern_item, 0..30)],
            module_doc: None,
            module_graph: None,
        });
        let rt_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::ExternRtSymbolUnclassified { .. }))
            .collect();
        assert!(
            rt_errors.is_empty(),
            "classified symbol hew_sleep_ns must not produce ExternRtSymbolUnclassified; \
             got: {rt_errors:?}"
        );
    }

    /// An unclassified `extern "rt"` symbol must produce `ExternRtSymbolUnclassified`.
    #[test]
    fn extern_rt_unclassified_symbol_rejected() {
        let extern_item = make_extern_rt_block(&["fake_unclassified_symbol"]);
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&Program {
            items: vec![(extern_item, 0..40)],
            module_doc: None,
            module_graph: None,
        });
        assert!(
            output.errors.iter().any(|e| matches!(&e.kind,
                TypeErrorKind::ExternRtSymbolUnclassified { symbol_name, .. }
                if symbol_name == "fake_unclassified_symbol"
            )),
            "unclassified symbol must produce ExternRtSymbolUnclassified; got: {:?}",
            output.errors
        );
    }

    /// A `codegen-stable` symbol must be rejected in `extern "rt"` — it is
    /// compiler-emitted, not user-callable. The checker only accepts `stable`.
    #[test]
    fn extern_rt_codegen_stable_symbol_rejected() {
        // hew_actor_cooperate is in the codegen-stable tier, not stable.
        let extern_item = make_extern_rt_block(&["hew_actor_cooperate"]);
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&Program {
            items: vec![(extern_item, 0..40)],
            module_doc: None,
            module_graph: None,
        });
        assert!(
            output.errors.iter().any(|e| matches!(&e.kind,
                TypeErrorKind::ExternRtSymbolUnclassified { symbol_name, .. }
                if symbol_name == "hew_actor_cooperate"
            )),
            "codegen-stable symbol hew_actor_cooperate must be rejected in \
             extern \"rt\"; got: {:?}",
            output.errors
        );
    }

    /// The dyn-trait heap-box storage ABI (`hew_dyn_box_alloc` /
    /// `hew_dyn_box_free`) is compiler-emission only. Exposing it as
    /// user-callable would let user code call the free path with a wrong
    /// `(size, align)` pair or with a foreign pointer, producing double-free
    /// / wrong-layout UB. Both symbols must live in `codegen-stable` and the
    /// checker must reject any `extern "rt"` declaration that names them.
    #[test]
    fn extern_rt_dyn_box_symbols_rejected() {
        for sym in ["hew_dyn_box_alloc", "hew_dyn_box_free"] {
            let extern_item = make_extern_rt_block(&[sym]);
            let mut checker = Checker::new(ModuleRegistry::new(vec![]));
            let output = checker.check_program(&Program {
                items: vec![(extern_item, 0..40)],
                module_doc: None,
                module_graph: None,
            });
            assert!(
                output.errors.iter().any(|e| matches!(&e.kind,
                    TypeErrorKind::ExternRtSymbolUnclassified { symbol_name, .. }
                    if symbol_name == sym
                )),
                "codegen-stable symbol {sym} must be rejected in extern \"rt\"; \
                 got: {:?}",
                output.errors
            );
        }
    }

    /// The suspending-read lifecycle ABI (`hew_conn_await_read` /
    /// `hew_read_slot_new` / `_free` / `_cancel` / `_status` / `_take`) is
    /// compiler-emission only — codegen lowers `await conn.read()` into these
    /// calls and manages the slot's manual refcount + cancellation protocol.
    /// Exposing them as user-callable `extern "rt"` surface would let user code
    /// allocate/free/cancel slots out of protocol and corrupt the refcount
    /// (double-free / use-after-free). All six must live in `codegen-stable` and
    /// the checker must reject any `extern "rt"` declaration that names them.
    #[test]
    fn extern_rt_read_slot_lifecycle_symbols_rejected() {
        for sym in [
            "hew_conn_await_read",
            "hew_read_slot_new",
            "hew_read_slot_free",
            "hew_read_slot_cancel",
            "hew_read_slot_status",
            "hew_read_slot_take",
        ] {
            let extern_item = make_extern_rt_block(&[sym]);
            let mut checker = Checker::new(ModuleRegistry::new(vec![]));
            let output = checker.check_program(&Program {
                items: vec![(extern_item, 0..40)],
                module_doc: None,
                module_graph: None,
            });
            assert!(
                output.errors.iter().any(|e| matches!(&e.kind,
                    TypeErrorKind::ExternRtSymbolUnclassified { symbol_name, .. }
                    if symbol_name == sym
                )),
                "codegen-stable read-slot symbol {sym} must be rejected in \
                 extern \"rt\"; got: {:?}",
                output.errors
            );
        }
    }

    /// The Windows-ABI-shim supervisor helpers (`hew_supervisor_child_get_raw`
    /// and `hew_supervisor_nested_get_raw`) are compiler-emitted only — the
    /// codegen translates MIR `hew_supervisor_child_get` / `_nested_get` calls
    /// into these `_raw` variants to avoid the Windows x64 MSVC sret ABI
    /// mismatch.  Both must live in `codegen-stable` so that user `extern "rt"`
    /// declarations are rejected by the checker.
    #[test]
    fn extern_rt_supervisor_raw_shims_rejected() {
        for sym in [
            "hew_supervisor_child_get_raw",
            "hew_supervisor_nested_get_raw",
        ] {
            let extern_item = make_extern_rt_block(&[sym]);
            let mut checker = Checker::new(ModuleRegistry::new(vec![]));
            let output = checker.check_program(&Program {
                items: vec![(extern_item, 0..60)],
                module_doc: None,
                module_graph: None,
            });
            assert!(
                output.errors.iter().any(|e| matches!(&e.kind,
                    TypeErrorKind::ExternRtSymbolUnclassified { symbol_name, .. }
                    if symbol_name == sym
                )),
                "codegen-stable shim {sym} must be rejected in extern \"rt\"; \
                 got: {:?}",
                output.errors
            );
        }
    }

    /// The stream/sink error channel splits producer from consumer: the
    /// `set_last_error` setters are `internal` (AOT-only, called by hew-cabi
    /// forwarders in native packages) and MUST be rejected in `extern "rt"` — a
    /// user could otherwise mutate the thread-local error channel and allocate
    /// caller-sized strings. The read-side getters stay `stable` so user code
    /// may inspect the last error.
    #[test]
    fn extern_rt_stream_error_setters_rejected_getters_accepted() {
        for setter in [
            "hew_stream_set_last_error",
            "hew_stream_set_last_error_with_errno",
        ] {
            let extern_item = make_extern_rt_block(&[setter]);
            let mut checker = Checker::new(ModuleRegistry::new(vec![]));
            let output = checker.check_program(&Program {
                items: vec![(extern_item, 0..60)],
                module_doc: None,
                module_graph: None,
            });
            assert!(
                output.errors.iter().any(|e| matches!(&e.kind,
                    TypeErrorKind::ExternRtSymbolUnclassified { symbol_name, .. }
                    if symbol_name == setter
                )),
                "internal stream-error setter {setter} must be rejected in extern \"rt\"; \
                 got: {:?}",
                output.errors
            );
        }

        for getter in ["hew_stream_last_error", "hew_stream_last_errno"] {
            let extern_item = make_extern_rt_block(&[getter]);
            let mut checker = Checker::new(ModuleRegistry::new(vec![]));
            let output = checker.check_program(&Program {
                items: vec![(extern_item, 0..40)],
                module_doc: None,
                module_graph: None,
            });
            let rt_errors: Vec<_> = output
                .errors
                .iter()
                .filter(|e| {
                    matches!(&e.kind,
                        TypeErrorKind::ExternRtSymbolUnclassified { symbol_name, .. }
                        if symbol_name == getter
                    )
                })
                .collect();
            assert!(
                rt_errors.is_empty(),
                "stable stream-error getter {getter} must be accepted in extern \"rt\"; \
                 got: {rt_errors:?}"
            );
        }
    }

    /// `extern "C"` blocks with any symbol name must NOT be validated against
    /// the stable list — that is raw user FFI surface.
    #[test]
    fn extern_c_bypasses_rt_validation() {
        let extern_item = Item::ExternBlock(ExternBlock {
            abi: "C".to_string(),
            functions: vec![ExternFnDecl {
                attributes: Vec::new(),
                name: "totally_made_up_ffi_symbol".to_string(),
                params: vec![],
                return_type: None,
                is_variadic: false,
                span: 0..0,
            }],
        });
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&Program {
            items: vec![(extern_item, 0..50)],
            module_doc: None,
            module_graph: None,
        });
        let rt_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::ExternRtSymbolUnclassified { .. }))
            .collect();
        assert!(
            rt_errors.is_empty(),
            "extern \"C\" must not trigger ExternRtSymbolUnclassified; got: {rt_errors:?}"
        );
    }

    // ── gen{} in actor receive handler (A98 / Q98) ─────────────────────────

    /// A `gen { }` block inside an actor receive handler must produce
    /// `GenBlockInActorReceive`, not a generic `InvalidOperation`.
    #[test]
    fn genblock_inside_actor_receive_handler_is_rejected() {
        let output = check_source(
            r"
            actor Counter {
                count: i32;
                receive fn tick() {
                    let _g = gen { count = count + 1; };
                }
            }
            fn main() {}
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::GenBlockInActorReceive),
            "gen{{}} inside actor receive handler must emit GenBlockInActorReceive; got: {:?}",
            output.errors
        );
    }

    /// A `gen { }` block in a plain function (not an actor handler) must emit
    /// `EmptyGenerator` — never `GenBlockInActorReceive`.
    #[test]
    fn genblock_outside_actor_receive_handler_is_not_rejected_with_actor_error() {
        let output = check_source(
            r"
            fn main() {
                let _g = gen { };
            }
            ",
        );
        assert!(
            !output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::GenBlockInActorReceive),
            "gen{{}} outside actor handler must not emit GenBlockInActorReceive; got: {:?}",
            output.errors
        );
        // Empty gen{} fails with EmptyGenerator (no yield expressions to infer from).
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::EmptyGenerator),
            "empty gen{{}} must emit EmptyGenerator; got: {:?}",
            output.errors
        );
    }

    // ── gen{} typed checking ────────────────────────────────────────────────

    /// A `gen { yield 1; yield 2; }` in a plain function type-checks cleanly
    /// and produces no errors.  The yield type is inferred as `i32`.
    #[test]
    fn gen_block_outside_receive_type_checks_cleanly() {
        let output = check_source(
            r"
            fn main() {
                let _g = gen { yield 1; yield 2; };
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "gen block with yield expressions outside actor receive must type-check cleanly: {:?}",
            output.errors
        );
    }

    /// An empty `gen { }` block must emit `EmptyGenerator` because the yield
    /// type-variable cannot be resolved without any `yield` expressions.
    #[test]
    fn gen_block_empty_emits_empty_generator() {
        let output = check_source(
            r"
            fn main() {
                let _g = gen { };
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::EmptyGenerator),
            "empty gen{{}} must emit EmptyGenerator; got: {:?}",
            output.errors
        );
    }

    /// `gen { }` inside an actor receive handler must emit `GenBlockInActorReceive`
    /// and must NOT emit `EmptyGenerator` — the actor guard fires first.
    #[test]
    fn genblock_in_actor_receive_is_rejected_not_empty_generator() {
        let output = check_source(
            r"
            actor Counter {
                count: i32;
                receive fn tick() {
                    let _g = gen { };
                }
            }
            fn main() {}
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::GenBlockInActorReceive),
            "gen{{}} inside actor receive must emit GenBlockInActorReceive; got: {:?}",
            output.errors
        );
        assert!(
            !output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::EmptyGenerator),
            "gen{{}} inside actor receive must not emit EmptyGenerator (actor guard fires first); got: {:?}",
            output.errors
        );
    }

    // ── gen{} / await in machine transition bodies (Machine Lane B S6) ─────

    #[test]
    fn genblock_inside_machine_transition_is_rejected() {
        let output = check_source(
            r"
            machine Door {
                events {
                    Toggle;
                }

                state Closed;
                state Open;


                on Toggle: Closed => Open {
                    gen { yield Open; }
                }
                on Toggle: Open => Closed {
                    Closed
                }
            }
            fn main() {}
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::GenBlockInMachineTransition),
            "gen{{}} inside machine transition must emit GenBlockInMachineTransition; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn await_inside_machine_transition_is_rejected() {
        let output = check_source(
            r"
            machine Door {
                events {
                    Toggle;
                }

                state Closed;
                state Open;


                on Toggle: Closed => Open {
                    await pending;
                    Open
                }
                on Toggle: Open => Closed {
                    Closed
                }
            }
            fn main() {}
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::AwaitInMachineTransition),
            "await inside machine transition must emit AwaitInMachineTransition; got: {:?}",
            output.errors
        );
    }

    // ── gen{} Return-component inference ───────────────────────────────────

    /// `gen { 1 }` has a tail expression but no yield.  The Return component
    /// must be inferred as i64 (not Unit), and no `EmptyGenerator` is emitted.
    #[test]
    fn gen_block_tail_expr_infers_return_component() {
        let output = check_source(
            r"
            fn main() {
                let _g = gen { 1 };
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "gen block with tail expression but no yield must type-check cleanly: {:?}",
            output.errors
        );
    }

    /// `gen { return 1; }` has an explicit return but no yield.  The Return
    /// component is i64, and the body never yields (Yield=Never).  No error.
    #[test]
    fn gen_block_explicit_return_infers_return_component() {
        let output = check_source(
            r"
            fn main() {
                let _g = gen { return 1; };
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "gen block with explicit return but no yield must type-check cleanly: {:?}",
            output.errors
        );
    }

    /// `gen { yield 1; 2 }` has both yield and a tail expression.
    /// Both Yield and Return must be inferred (i64 each); no error.
    #[test]
    fn gen_block_yield_and_tail_both_infer() {
        let output = check_source(
            r"
            fn main() {
                let _g = gen { yield 1; 2 };
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "gen block with yield and tail expression must type-check cleanly: {:?}",
            output.errors
        );
    }

    /// `return 1` inside gen{} must NOT produce a return-type mismatch against
    /// the full Generator<Y, R> shape; the checker extracts the Return component
    /// for `Stmt::Return` when `in_generator` is set.
    #[test]
    fn gen_block_return_does_not_mismatch_full_generator_type() {
        let output = check_source(
            r"
            fn main() {
                let _g = gen { return 42; };
            }
            ",
        );
        assert!(
            !output
                .errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrorKind::Mismatch { .. })),
            "`return <expr>` inside gen{{}} must not produce a Mismatch against the Generator \
             wrapper; got: {:?}",
            output.errors
        );
    }

    // ── recursive closure self-reference (E_CLOSURE_RECURSIVE) ─────────────

    /// A closure that references its own let-binding by name must produce
    /// `ClosureRecursive`, not `UndefinedVariable`.
    #[test]
    fn recursive_closure_self_reference_is_rejected() {
        let output = check_source(
            r"
            fn main() {
                let f = |x: i32| -> i32 { f(x) };
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::ClosureRecursive { .. })),
            "recursive closure self-reference must emit ClosureRecursive; got: {:?}",
            output.errors
        );
    }

    /// A non-recursive closure that references a binding of the same name
    /// from an outer scope is fine — the binding exists at capture depth.
    #[test]
    fn closure_referencing_outer_binding_same_name_is_not_recursive_error() {
        let output = check_source(
            r"
            fn apply(f: fn(i32) -> i32, x: i32) -> i32 { f(x) }
            fn main() {
                let k: i32 = 10;
                let f = |x: i32| -> i32 { x + k };
                let _result = apply(f, 5);
            }
            ",
        );
        assert!(
            !output
                .errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::ClosureRecursive { .. })),
            "non-recursive closure should not emit ClosureRecursive; got: {:?}",
            output.errors
        );
    }

    /// The hint in the diagnostic must direct the user to add the symbol to
    /// the classification toml.
    #[test]
    fn extern_rt_unclassified_hint_mentions_toml() {
        let extern_item = make_extern_rt_block(&["my_custom_symbol"]);
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&Program {
            items: vec![(extern_item, 0..30)],
            module_doc: None,
            module_graph: None,
        });
        let err = output
            .errors
            .iter()
            .find(|e| matches!(e.kind, TypeErrorKind::ExternRtSymbolUnclassified { .. }))
            .expect("expected ExternRtSymbolUnclassified error");
        assert!(
            err.suggestions
                .iter()
                .any(|s| s.contains("jit-symbol-classification.toml")),
            "suggestion must mention jit-symbol-classification.toml; got: {:?}",
            err.suggestions
        );
    }
}

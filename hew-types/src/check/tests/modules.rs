#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

#[test]
fn module_graph_body_type_error_is_reported() {
    // fn bad() -> i64 { true }  — body returns bool, declared i64
    let bad_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        name: "bad".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            0..3,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new(make_bool_literal(true, 0..4))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    };

    let program = make_program_with_module_graph(vec![(Item::Function(bad_fn), 0..10)]);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        !output.errors.is_empty(),
        "expected a type error from non-root module body, but none were reported"
    );
    assert!(
        output.errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrorKind::Mismatch { expected, actual }
                if expected.contains("i64")
                    && actual.contains("bool")
        )),
        "expected a Mismatch(i64/i64, bool) error; got: {:?}",
        output.errors
    );
}

/// A function with an inferred return type (`-> _`) in a non-root `module_graph`
/// body must have its return type resolved by body checking.  Without the parity
/// fix the type var is never unified and the checker emits a spurious
/// `InferenceFailed` error.  With the fix the body resolves `_` to `i64` and no
/// error is emitted.
#[test]
fn module_graph_body_infer_return_resolves_without_error() {
    // fn inferred() -> _ { 42 }  — `_` must resolve to i64 from the body
    let inferred_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        name: "inferred".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((TypeExpr::Infer, 0..1)),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new(make_int_literal(42, 0..2))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    };

    let program = make_program_with_module_graph(vec![(Item::Function(inferred_fn), 0..10)]);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "inferred return type should resolve cleanly; errors: {:?}",
        output.errors
    );
}

/// A local binding in a non-root module body must take precedence over a
/// same-named module when typechecking method calls on identifier receivers.
#[test]
fn module_graph_body_local_binding_named_like_module_still_resolves_methods() {
    let ok_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        name: "ok".to_string(),
        type_params: None,
        params: vec![Param {
            name: "math".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "string".to_string(),
                    type_args: None,
                },
                0..6,
            ),
            is_mutable: false,
        }],
        return_type: Some((
            TypeExpr::Named {
                name: "bool".to_string(),
                type_args: None,
            },
            0..4,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new((
                Expr::MethodCall {
                    receiver: Box::new((Expr::Identifier("math".to_string()), 0..4)),
                    method: "contains".to_string(),
                    args: vec![CallArg::Positional((
                        Expr::Literal(Literal::String("x".to_string())),
                        5..8,
                    ))],
                },
                0..18,
            ))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    };

    let program = make_program_with_module_graph(vec![(Item::Function(ok_fn), 0..30)]);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "local bindings should win over same-named modules in non-root method calls; errors: {:?}",
        output.errors
    );
}

#[test]
fn module_qualified_call_rejects_private_body_only_signature() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.modules.insert("mymod".to_string());
    checker.fn_sigs.insert(
        "mymod.secret".to_string(),
        FnSig {
            return_type: Ty::I64,
            ..FnSig::default()
        },
    );

    let receiver = (Expr::Identifier("mymod".to_string()), 0..5);
    let ty = checker.check_method_call(&receiver, "secret", &[], &(0..12));

    assert_eq!(ty, Ty::Error);
    assert!(
        checker
            .errors
            .iter()
            .any(|err| matches!(err.kind, TypeErrorKind::UndefinedMethod)),
        "module-qualified calls must not resolve against non-exported private signatures: {:?}",
        checker.errors
    );
}

#[test]
fn module_qualified_call_accepts_exported_signature() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.modules.insert("mymod".to_string());
    checker
        .module_fn_exports
        .insert("mymod.visible".to_string());
    checker.fn_sigs.insert(
        "mymod.visible".to_string(),
        FnSig {
            return_type: Ty::I64,
            ..FnSig::default()
        },
    );

    let receiver = (Expr::Identifier("mymod".to_string()), 0..5);
    let ty = checker.check_method_call(&receiver, "visible", &[], &(0..13));

    assert_eq!(ty, Ty::I64);
    assert!(
        checker.errors.is_empty(),
        "exported module-qualified calls must keep working; errors: {:?}",
        checker.errors
    );
}

#[test]
fn module_graph_body_private_local_type_is_available() {
    let local_type = TypeDecl {
        visibility: Visibility::Private,
        name: "Local".to_string(),
        type_params: None,
        where_clause: None,
        kind: TypeDeclKind::Struct,
        body: vec![TypeBodyItem::Field {
            name: "x".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "i64".to_string(),
                    type_args: None,
                },
                0..1,
            ),
            attributes: Vec::new(),
            doc_comment: None,
            span: 0..0,
        }],
        is_indirect: false,
        doc_comment: None,
        wire: None,
        resource_marker: hew_parser::ast::ResourceMarker::None,
        is_opaque: false,
        consuming_methods: Vec::new(),
    };

    let ok_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        name: "ok".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            0..3,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![(
                Stmt::Let {
                    pattern: (Pattern::Identifier("a".to_string()), 0..1),
                    ty: None,
                    value: Some((
                        Expr::StructInit {
                            name: "Local".to_string(),
                            fields: vec![("x".to_string(), make_int_literal(1, 0..1))],
                            type_args: None,
                            base: None,
                        },
                        0..10,
                    )),
                    else_block: None,
                },
                0..10,
            )],
            trailing_expr: Some(Box::new((
                Expr::FieldAccess {
                    object: Box::new((Expr::Identifier("a".to_string()), 0..1)),
                    field: "x".to_string(),
                },
                11..14,
            ))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    };

    let program = make_program_with_module_graph(vec![
        (Item::TypeDecl(local_type), 0..10),
        (Item::Function(ok_fn), 10..30),
    ]);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "private non-root local types should resolve within the same module body; errors: {:?}",
        output.errors
    );
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "constructs an explicit multi-module fixture for the parity regression"
)]
fn module_graph_body_prefers_same_module_private_helper_over_global_bare_name() {
    let i64_ty = TypeExpr::Named {
        name: "i64".to_string(),
        type_args: None,
    };
    let string_ty = TypeExpr::Named {
        name: "string".to_string(),
        type_args: None,
    };

    let helper_i64 = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        name: "helper".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((i64_ty.clone(), 0..3)),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new(make_int_literal(42, 0..2))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    };

    let ok_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        name: "ok".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((i64_ty, 0..3)),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new((
                Expr::Call {
                    function: Box::new((Expr::Identifier("helper".to_string()), 0..6)),
                    type_args: None,
                    args: vec![],
                    is_tail_call: false,
                },
                0..8,
            ))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    };

    let helper_string = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        name: "helper".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((string_ty, 10..16)),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new((
                Expr::Literal(Literal::String("wrong".to_string())),
                10..17,
            ))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    };

    let root_id = ModuleId::root();
    let alpha_id = ModuleId::new(vec!["alpha".to_string()]);
    let beta_id = ModuleId::new(vec!["beta".to_string()]);
    let root_module = Module {
        id: root_id.clone(),
        items: vec![],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let alpha_module = Module {
        id: alpha_id.clone(),
        items: vec![
            (Item::Function(helper_i64), 0..20),
            (Item::Function(ok_fn), 20..40),
        ],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let beta_module = Module {
        id: beta_id.clone(),
        items: vec![(Item::Function(helper_string), 40..60)],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };

    let mut mg = ModuleGraph::new(root_id.clone());
    mg.add_module(root_module).unwrap();
    mg.add_module(alpha_module).unwrap();
    mg.add_module(beta_module).unwrap();
    mg.topo_order = vec![alpha_id, beta_id, root_id];

    let program = Program {
        module_graph: Some(mg),
        items: vec![],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "same-module private helper should win over another module's bare helper name; errors: {:?}",
        output.errors
    );
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "constructs an explicit multi-module fixture for the parity regression"
)]
fn module_graph_body_prefers_same_module_private_extern_over_global_bare_name() {
    let i64_ty = TypeExpr::Named {
        name: "i64".to_string(),
        type_args: None,
    };
    let string_ty = TypeExpr::Named {
        name: "string".to_string(),
        type_args: None,
    };

    let extern_i64 = ExternBlock {
        abi: "C".to_string(),
        functions: vec![ExternFnDecl {
            attributes: Vec::new(),
            name: "hew_test_raw".to_string(),
            params: vec![],
            return_type: Some((i64_ty.clone(), 0..3)),
            is_variadic: false,
            span: 0..0,
        }],
    };
    let ok_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        name: "ok".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((i64_ty, 0..3)),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new((
                Expr::UnsafeBlock(Box::new(Block {
                    stmts: vec![],
                    trailing_expr: Some(Box::new((
                        Expr::Call {
                            function: Box::new((
                                Expr::Identifier("hew_test_raw".to_string()),
                                0..12,
                            )),
                            type_args: None,
                            args: vec![],
                            is_tail_call: false,
                        },
                        0..14,
                    ))),
                })),
                0..14,
            ))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    };
    let extern_string = ExternBlock {
        abi: "C".to_string(),
        functions: vec![ExternFnDecl {
            attributes: Vec::new(),
            name: "hew_test_raw".to_string(),
            params: vec![],
            return_type: Some((string_ty, 20..26)),
            is_variadic: false,
            span: 0..0,
        }],
    };

    let root_id = ModuleId::root();
    let alpha_id = ModuleId::new(vec!["alpha".to_string()]);
    let beta_id = ModuleId::new(vec!["beta".to_string()]);
    let root_module = Module {
        id: root_id.clone(),
        items: vec![],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let alpha_module = Module {
        id: alpha_id.clone(),
        items: vec![
            (Item::ExternBlock(extern_i64), 0..20),
            (Item::Function(ok_fn), 20..40),
        ],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let beta_module = Module {
        id: beta_id.clone(),
        items: vec![(Item::ExternBlock(extern_string), 40..60)],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };

    let mut mg = ModuleGraph::new(root_id.clone());
    mg.add_module(root_module).unwrap();
    mg.add_module(alpha_module).unwrap();
    mg.add_module(beta_module).unwrap();
    mg.topo_order = vec![alpha_id, beta_id, root_id];

    let program = Program {
        module_graph: Some(mg),
        items: vec![],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "same-module private extern should win over another module's bare extern name; errors: {:?}",
        output.errors
    );
}

// ── module-body-diagnostic-completion: source_module tagging tests ────────────
//
// These tests prove that diagnostics originating in non-root module bodies
// carry the correct `source_module` value so the CLI can route them to the
// right source file when rendering.
//
// Regression coverage for module-body typecheck parity and diagnostic-envelope
// unification.

#[cfg(test)]
mod module_body_diagnostic_envelope {
    use super::*;

    // ── helpers ────────────────────────────────────────────────────────────────

    /// Build a minimal `Program` with a non-root module `mod_name` whose items
    /// are the supplied `items`.  The root module is empty.
    fn make_program_with_named_module(mod_name: &str, items: Vec<Spanned<Item>>) -> Program {
        let root_id = ModuleId::root();
        let mod_id = ModuleId::new(vec![mod_name.to_string()]);

        let non_root = Module {
            id: mod_id.clone(),
            items,
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(non_root).unwrap();
        mg.topo_order = vec![mod_id, root_id];

        Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        }
    }

    /// Build a minimal fn declaration that returns `bool` but is declared `-> i64`.
    fn make_mistyped_fn(name: &str) -> Spanned<Item> {
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Pub,
            name: name.to_string(),
            type_params: None,
            params: vec![],
            return_type: Some((
                TypeExpr::Named {
                    name: "i64".to_string(),
                    type_args: None,
                },
                5..8,
            )),
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: Some(Box::new((Expr::Literal(Literal::Bool(true)), 10..14))),
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
            consumes_self: false,
        };
        (Item::Function(fn_decl), 0..20)
    }

    // ── body-check errors carry the module name ────────────────────────────────

    /// A type mismatch in a non-root module body must be tagged with the
    /// module's dotted name in `TypeError::source_module`.
    #[test]
    fn body_mismatch_error_tagged_with_source_module() {
        let program = make_program_with_named_module("mymod", vec![make_mistyped_fn("bad")]);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        assert!(
            !output.errors.is_empty(),
            "expected a type error from non-root module body"
        );
        for err in &output.errors {
            assert_eq!(
                err.source_module.as_deref(),
                Some("mymod"),
                "error from non-root module body must be tagged with 'mymod'; got: {:?}",
                err.source_module
            );
        }
    }

    /// A type mismatch in the root module must NOT be tagged (`source_module`
    /// stays None).
    #[test]
    fn root_module_error_has_no_source_module_tag() {
        // fn bad() -> i64 { true }  in root items
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: "bad".to_string(),
            type_params: None,
            params: vec![],
            return_type: Some((
                TypeExpr::Named {
                    name: "i64".to_string(),
                    type_args: None,
                },
                5..8,
            )),
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: Some(Box::new((Expr::Literal(Literal::Bool(true)), 10..14))),
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
            consumes_self: false,
        };
        let program = Program {
            module_graph: None,
            items: vec![(Item::Function(fn_decl), 0..20)],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        assert!(
            !output.errors.is_empty(),
            "expected a type error from root module"
        );
        for err in &output.errors {
            assert!(
                err.source_module.is_none(),
                "root module error must not have source_module set; got: {:?}",
                err.source_module
            );
        }
    }

    /// Errors from different non-root modules must each carry their own module name.
    #[test]
    fn errors_from_multiple_modules_tagged_independently() {
        use hew_parser::ast::{Block, FnDecl, Item, Literal, TypeExpr, Visibility};

        let make_bad_fn = |fn_name: &str| -> Spanned<Item> {
            let fd = FnDecl {
                attributes: vec![],
                is_async: false,
                is_generator: false,
                visibility: Visibility::Pub,
                name: fn_name.to_string(),
                type_params: None,
                params: vec![],
                return_type: Some((
                    TypeExpr::Named {
                        name: "i64".to_string(),
                        type_args: None,
                    },
                    5..8,
                )),
                where_clause: None,
                body: Block {
                    stmts: vec![],
                    trailing_expr: Some(Box::new((Expr::Literal(Literal::Bool(true)), 10..14))),
                },
                doc_comment: None,
                decl_span: 0..0,
                fn_span: 0..0,
                intrinsic: None,
                consumes_self: false,
            };
            (Item::Function(fd), 0..20)
        };

        let root_id = ModuleId::root();
        let alpha_id = ModuleId::new(vec!["alpha".to_string()]);
        let beta_id = ModuleId::new(vec!["beta".to_string()]);

        let alpha = Module {
            id: alpha_id.clone(),
            items: vec![make_bad_fn("alpha_bad")],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let beta = Module {
            id: beta_id.clone(),
            items: vec![make_bad_fn("beta_bad")],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(alpha).unwrap();
        mg.add_module(beta).unwrap();
        mg.topo_order = vec![alpha_id, beta_id, root_id];

        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        assert!(
            output.errors.len() >= 2,
            "expected errors from both alpha and beta modules; got: {:?}",
            output.errors
        );

        let alpha_errs: Vec<_> = output
            .errors
            .iter()
            .filter(|e| e.source_module.as_deref() == Some("alpha"))
            .collect();
        let beta_errs: Vec<_> = output
            .errors
            .iter()
            .filter(|e| e.source_module.as_deref() == Some("beta"))
            .collect();

        assert!(
            !alpha_errs.is_empty(),
            "expected at least one error tagged 'alpha'; errors: {:?}",
            output.errors
        );
        assert!(
            !beta_errs.is_empty(),
            "expected at least one error tagged 'beta'; errors: {:?}",
            output.errors
        );
    }

    // ── deferred inference hole drain carries source module ────────────────────

    /// An unresolved cast target `as _` in a non-root module body must produce
    /// an `InferenceFailed` error tagged with the module name.
    #[test]
    fn deferred_cast_hole_tagged_with_source_module() {
        // fn foo(x: i64) { let y = x as _; }  — cast target _ never resolved
        use hew_parser::ast::{Block, FnDecl, Param, Stmt, TypeExpr, Visibility};

        let cast_expr = Expr::Cast {
            expr: Box::new((Expr::Identifier("x".to_string()), 20..21)),
            ty: (TypeExpr::Infer, 25..26),
        };
        let let_stmt = Stmt::Let {
            pattern: (Pattern::Identifier("y".to_string()), 14..15),
            ty: None,
            value: Some((cast_expr, 18..27)),
            else_block: None,
        };
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: "foo".to_string(),
            type_params: None,
            params: vec![Param {
                name: "x".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "i64".to_string(),
                        type_args: None,
                    },
                    8..11,
                ),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![(let_stmt, 13..28)],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
            consumes_self: false,
        };

        let program =
            make_program_with_named_module("castmod", vec![(Item::Function(fn_decl), 0..30)]);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let inference_failed: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::InferenceFailed))
            .collect();

        assert!(
            !inference_failed.is_empty(),
            "expected InferenceFailed for unresolved `as _` in non-root module; errors: {:?}",
            output.errors
        );
        for err in &inference_failed {
            assert_eq!(
                err.source_module.as_deref(),
                Some("castmod"),
                "deferred cast inference error must carry source module 'castmod'; got {:?}",
                err.source_module
            );
        }
    }

    /// Signature-level inference holes in non-root modules (fn param `_`) must
    /// also be tagged with the module name.
    #[test]
    fn signature_inference_hole_tagged_with_source_module() {
        // fn helper(_ : _) {}  — unresolved param type
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: "helper".to_string(),
            type_params: None,
            params: vec![Param {
                name: "v".to_string(),
                ty: (TypeExpr::Infer, 10..11),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
            consumes_self: false,
        };

        let program =
            make_program_with_named_module("sigmod", vec![(Item::Function(fn_decl), 0..20)]);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let inference_failed: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::InferenceFailed))
            .collect();

        assert!(
            !inference_failed.is_empty(),
            "expected InferenceFailed for `_` param in non-root module fn; errors: {:?}",
            output.errors
        );
        for err in &inference_failed {
            assert_eq!(
                err.source_module.as_deref(),
                Some("sigmod"),
                "signature inference error must carry source module 'sigmod'; got {:?}",
                err.source_module
            );
        }
    }

    /// Deferred channel rewrite finalization must preserve the non-root module
    /// tag when it emits a post-inference `InferenceFailed` diagnostic.
    #[test]
    fn deferred_channel_rewrite_error_tagged_with_source_module() {
        let parsed = hew_parser::parse(
            r"
                fn bad(rx: Receiver<_>) {
                    let _ = rx.recv();
                }
            ",
        );
        assert!(
            parsed.errors.is_empty(),
            "module parse errors: {:?}",
            parsed.errors
        );

        let program = make_program_with_named_module("chanmod", parsed.program.items.clone());

        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&program);

        let inference_failed: Vec<_> = output
            .errors
            .iter()
            .filter(|e| {
                matches!(e.kind, TypeErrorKind::InferenceFailed) && e.message.contains("inner type")
            })
            .collect();

        assert!(
            !inference_failed.is_empty(),
            "expected deferred channel inference failure in non-root module; errors: {:?}",
            output.errors
        );
        for err in inference_failed {
            assert_eq!(
                err.source_module.as_deref(),
                Some("chanmod"),
                "deferred channel rewrite error must carry source module 'chanmod'; got {:?}",
                err.source_module
            );
        }
    }

    #[test]
    fn assign_target_shapes_populated_for_while_loop_with_import() {
        // Reproduces the eval_large_stderr CI failure:
        // synthesized source for `fn spam_err` eval step with `import std::io`
        let source = "import std::io;\nfn spam_err() {\n    var i = 0;\n    while i < 20000 {\n        io.write_err(\"line\\n\");\n        i = i + 1;\n    }\n}\nfn main() {\n}\n";
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "parse errors: {:?}",
            parse_result.errors
        );
        let mut checker = crate::Checker::new(crate::module_registry::ModuleRegistry::new(
            crate::module_registry::build_module_search_paths(),
        ));
        let tco = checker.check_program(&parse_result.program);
        // `i` in `i = i + 1` must appear in assign_target_shapes
        let has_shape = tco.assign_target_shapes.iter().any(|(k, _)| k.start == 109);
        assert!(
            has_shape,
            "assign_target_shapes missing entry for i at ~109; got: {:?}",
            tco.assign_target_shapes.keys().collect::<Vec<_>>()
        );
    }
}

// ── Warning source-module attribution (PR-A slice) ──────────────────────────

#[cfg(test)]
mod warning_source_attribution {
    use super::*;

    fn make_unused_import_decl() -> ImportDecl {
        // import std::encoding::json  (unresolved — no resolved_items)
        // The module registry will fail to find it, but the import is still
        // registered into import_spans so the UnusedImport path is exercised.
        // Use a fake single-segment path so `register_import` takes the user-
        // module branch (path non-empty, no resolved_items → unresolved error
        // path, does NOT insert into import_spans).
        //
        // Instead we supply `resolved_items = Some(vec![])` to convince
        // register_import to follow the user-module branch and insert into
        // import_spans.
        ImportDecl {
            path: vec!["fakemod".to_string()],
            spec: None,
            module_alias: None,
            file_path: None,
            resolved_items: Some(vec![]),
            resolved_item_source_paths: vec![],
            resolved_source_paths: vec![],
        }
    }

    fn make_trivial_fn(name: &str) -> FnDecl {
        FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: name.to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
            consumes_self: false,
        }
    }

    fn make_non_root_program_with_fn_body(
        module_name: &str,
        name: &str,
        stmts: Vec<Spanned<Stmt>>,
    ) -> Program {
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: name.to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts,
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
            consumes_self: false,
        };
        let root_id = ModuleId::root();
        let module_id = ModuleId::new(vec![module_name.to_string()]);
        let root_module = Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let sub_module = Module {
            id: module_id.clone(),
            items: vec![(Item::Function(fn_decl), 0..40)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(root_module).unwrap();
        mg.add_module(sub_module).unwrap();
        mg.topo_order = vec![module_id, root_id];

        Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        }
    }

    /// Build a Program whose module graph has:
    ///   - a root module with `fn main()` (no imports, in program.items)
    ///   - a non-root module "submod" with an import of "fakemod" and `fn helper()`
    fn build_program_with_non_root_import_and_fn() -> Program {
        let root_id = ModuleId::root();
        let submod_id = ModuleId::new(vec!["submod".to_string()]);

        let main_fn = make_trivial_fn("main");
        let helper_fn = make_trivial_fn("helper");
        let import_decl = make_unused_import_decl();

        let root_module = Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let sub_module = Module {
            id: submod_id.clone(),
            items: vec![
                (Item::Import(import_decl), 0..20),
                (Item::Function(helper_fn), 25..50),
            ],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(root_module).unwrap();
        mg.add_module(sub_module).unwrap();
        mg.topo_order = vec![submod_id, root_id];

        Program {
            module_graph: Some(mg),
            // Root-module items live in program.items (not in Module.items for root).
            items: vec![(Item::Function(main_fn), 0..10)],
            module_doc: None,
        }
    }

    /// An `UnusedImport` warning emitted for an import registered while
    /// `current_module` was "submod" must carry `source_module = Some("submod")`.
    #[test]
    fn non_root_unused_import_carries_source_module() {
        let program = build_program_with_non_root_import_and_fn();
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let unused_import_warnings: Vec<_> = output
            .warnings
            .iter()
            .filter(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("fakemod"))
            .collect();

        assert!(
            !unused_import_warnings.is_empty(),
            "expected an UnusedImport warning for 'fakemod'; got warnings: {:?}",
            output.warnings
        );
        for w in &unused_import_warnings {
            assert_eq!(
                w.source_module.as_deref(),
                Some("submod"),
                "UnusedImport for 'fakemod' must carry source_module='submod'; got {:?}",
                w.source_module
            );
        }
    }

    /// A root-module `UnusedImport` (registered while `current_module = None`) must
    /// continue to carry `source_module = None` — no regression.
    #[test]
    fn root_unused_import_has_no_source_module() {
        // Build an ImportDecl with resolved_items so it reaches import_spans.
        let import_decl = make_unused_import_decl();
        let main_fn = make_trivial_fn("main");

        let program = Program {
            module_graph: None,
            items: vec![
                (Item::Import(import_decl), 0..20),
                (Item::Function(main_fn), 25..40),
            ],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let w = output
            .warnings
            .iter()
            .find(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("fakemod"));

        assert!(
            w.is_some(),
            "expected UnusedImport warning for root 'fakemod'; got: {:?}",
            output.warnings
        );
        assert_eq!(
            w.unwrap().source_module,
            None,
            "root-module UnusedImport must have source_module=None; got {:?}",
            w.unwrap().source_module
        );
    }

    /// Functions defined in a non-root module must have their source module stored
    /// in `fn_def_spans` even though the current dead-code filter skips dot-named
    /// functions (they are never promoted to `DeadCode` warnings today).
    /// This guards the attribution infrastructure for future use.
    #[test]
    fn non_root_fn_def_span_stores_source_module() {
        let program = build_program_with_non_root_import_and_fn();
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.check_program(&program);

        // Non-root function is keyed as "submod.helper" in fn_def_spans.
        let entry = checker.fn_def_spans.get("submod.helper");
        assert!(
            entry.is_some(),
            "fn_def_spans must contain 'submod.helper'; keys: {:?}",
            checker.fn_def_spans.keys().collect::<Vec<_>>()
        );
        let (_, stored_module) = entry.unwrap();
        assert_eq!(
            stored_module.as_deref(),
            Some("submod"),
            "fn_def_spans entry for 'submod.helper' must store source_module='submod'; got {stored_module:?}",
        );
    }

    /// Root-module functions must store `source_module = None` in `fn_def_spans`.
    #[test]
    fn root_fn_def_span_has_no_source_module() {
        let program = build_program_with_non_root_import_and_fn();
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.check_program(&program);

        // Root fn (main) has no module prefix.
        let entry = checker.fn_def_spans.get("main");
        assert!(
            entry.is_some(),
            "fn_def_spans must contain 'main'; keys: {:?}",
            checker.fn_def_spans.keys().collect::<Vec<_>>()
        );
        let (_, stored_module) = entry.unwrap();
        assert_eq!(
            stored_module.as_deref(),
            None,
            "fn_def_spans entry for 'main' must have source_module=None; got {stored_module:?}",
        );
    }

    #[test]
    fn non_root_unreachable_warning_carries_source_module() {
        let stmts = vec![
            (Stmt::Return(None), 10..16),
            (
                Stmt::Expression((Expr::Literal(Literal::Bool(true)), 21..25)),
                21..23,
            ),
        ];
        let program = make_non_root_program_with_fn_body("submod", "warns", stmts);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let warnings: Vec<_> = output
            .warnings
            .iter()
            .filter(|w| w.kind == TypeErrorKind::UnreachableCode)
            .collect();

        assert!(
            !warnings.is_empty(),
            "expected UnreachableCode warning in non-root module; got: {:?}",
            output.warnings
        );
        for warning in warnings {
            assert_eq!(
                warning.source_module.as_deref(),
                Some("submod"),
                "UnreachableCode warning must carry source_module='submod'; got {:?}",
                warning.source_module
            );
        }
    }

    // ── ImportKey: same short-name across different owning modules ─────────────

    fn make_named_import_decl(short_name: &str) -> ImportDecl {
        ImportDecl {
            path: vec![short_name.to_string()],
            spec: None,
            module_alias: None,
            file_path: None,
            resolved_items: Some(vec![]),
            resolved_item_source_paths: vec![],
            resolved_source_paths: vec![],
        }
    }

    /// Two different owning modules both import a module with the same short
    /// name.  Neither module uses the import, so both must get an
    /// `UnusedImport` warning — the use-site in one must not suppress the
    /// warning for the other.
    #[test]
    #[allow(
        clippy::similar_names,
        reason = "mod_a_id / mod_b_id are intentionally symmetric"
    )]
    fn same_short_name_imports_in_different_owners_each_warn_unused() {
        let root_id = ModuleId::root();
        let mod_a_id = ModuleId::new(vec!["mod_a".to_string()]);
        let mod_b_id = ModuleId::new(vec!["mod_b".to_string()]);

        let import_a = make_named_import_decl("fakemod");
        let import_b = make_named_import_decl("fakemod");

        let root_module = Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let module_a = Module {
            id: mod_a_id.clone(),
            items: vec![(Item::Import(import_a), 0..20)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let module_b = Module {
            id: mod_b_id.clone(),
            items: vec![(Item::Import(import_b), 100..120)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(root_module).unwrap();
        mg.add_module(module_a).unwrap();
        mg.add_module(module_b).unwrap();
        mg.topo_order = vec![mod_a_id, mod_b_id, root_id];

        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let unused: Vec<_> = output
            .warnings
            .iter()
            .filter(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("fakemod"))
            .collect();

        assert_eq!(
            unused.len(),
            2,
            "expected exactly 2 UnusedImport warnings (one per owning module), got {}: {:?}",
            unused.len(),
            unused
        );

        let owners: std::collections::HashSet<Option<&str>> =
            unused.iter().map(|w| w.source_module.as_deref()).collect();
        assert!(
            owners.contains(&Some("mod_a")),
            "expected an UnusedImport attributed to 'mod_a'; got: {owners:?}",
        );
        assert!(
            owners.contains(&Some("mod_b")),
            "expected an UnusedImport attributed to 'mod_b'; got: {owners:?}",
        );
    }

    /// When one owning module *uses* `fakemod` (via a module-qualified call
    /// registered through `import_spans`) and another owning module imports the
    /// same short name but never uses it, only the second module's import must
    /// be warned as unused.
    ///
    /// This is the core clobber regression: before the `ImportKey` fix, marking
    /// `fakemod` as used in `mod_a` would also suppress the warning for `mod_b`.
    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "inline AST construction for the import-clobber regression scenario"
    )]
    #[allow(
        clippy::similar_names,
        reason = "mod_a_id / mod_b_id are intentionally symmetric"
    )]
    fn used_import_in_one_owner_does_not_suppress_unused_in_another() {
        let root_id = ModuleId::root();
        let mod_a_id = ModuleId::new(vec!["mod_a".to_string()]);
        let mod_b_id = ModuleId::new(vec!["mod_b".to_string()]);

        // mod_a: import fakemod  +  fn caller() { fakemod.helper() }
        // The call to fakemod.helper() marks fakemod as used in mod_a's context.

        // Register a pub fn "helper" in fakemod so module_fn_exports and fn_sigs
        // contain "fakemod.helper" — that is what the method-dispatch path checks.
        let helper_fn = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Pub,
            name: "helper".to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
            consumes_self: false,
        };

        // caller() body: `fakemod.helper()` expressed as a MethodCall statement.
        let call_stmt = Stmt::Expression((
            Expr::MethodCall {
                receiver: Box::new((
                    Expr::Identifier("fakemod".to_string()),
                    Span::from(200..206),
                )),
                method: "helper".to_string(),
                args: vec![],
            },
            Span::from(200..215),
        ));
        let caller_fn = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: "caller".to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![(call_stmt, 200..215)],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 150..200,
            fn_span: 0..0,
            intrinsic: None,
            consumes_self: false,
        };

        // Import of fakemod in mod_a must carry resolved_items that include
        // the pub helper fn so the module is actually registered and
        // module_fn_exports gets "fakemod.helper".
        let import_a_with_items = ImportDecl {
            path: vec!["fakemod".to_string()],
            spec: None,
            module_alias: None,
            file_path: None,
            resolved_items: Some(vec![(Item::Function(helper_fn), 0..30)]),
            resolved_item_source_paths: vec![],
            resolved_source_paths: vec![],
        };

        // mod_b: import fakemod  (unused — no code references it)
        let import_b = make_named_import_decl("fakemod");

        let root_module = Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let module_a = Module {
            id: mod_a_id.clone(),
            items: vec![
                (Item::Import(import_a_with_items), 0..30),
                (Item::Function(caller_fn), 150..220),
            ],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let module_b = Module {
            id: mod_b_id.clone(),
            items: vec![(Item::Import(import_b), 300..320)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(root_module).unwrap();
        mg.add_module(module_a).unwrap();
        mg.add_module(module_b).unwrap();
        mg.topo_order = vec![mod_a_id, mod_b_id, root_id];

        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let unused: Vec<_> = output
            .warnings
            .iter()
            .filter(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("fakemod"))
            .collect();

        // mod_b's import is unused; mod_a's is used (caller() calls fakemod.helper())
        // so exactly one warning, attributed to mod_b.
        assert_eq!(
            unused.len(),
            1,
            "expected exactly 1 UnusedImport warning (for mod_b), got {}: {:?}",
            unused.len(),
            unused
        );
        assert_eq!(
            unused[0].source_module.as_deref(),
            Some("mod_b"),
            "the single UnusedImport must be attributed to 'mod_b'; got: {:?}",
            unused[0].source_module
        );
    }

    /// A trait impl recorded elsewhere must not make an unrelated owner-module
    /// import look used. The eager trait-use path records `ImportKey { owner,
    /// short_name }`; a global trait table fallback would over-suppress this.
    #[test]
    fn recorded_trait_impl_elsewhere_does_not_suppress_unused_import() {
        let root_id = ModuleId::root();
        let owner_module_b_id = ModuleId::new(vec!["owner_module_b".to_string()]);

        let fake_trait = TraitDecl {
            visibility: Visibility::Pub,
            name: "FakeTrait".to_string(),
            type_params: None,
            super_traits: None,
            items: vec![TraitItem::Method(TraitMethod {
                name: "fake".to_string(),
                type_params: None,
                params: vec![Param {
                    name: "val".to_string(),
                    ty: (
                        TypeExpr::Named {
                            name: "Self".to_string(),
                            type_args: None,
                        },
                        0..4,
                    ),
                    is_mutable: false,
                }],
                return_type: None,
                where_clause: None,
                body: None,
                span: 0..0,
                doc_comment: None,
                lang_item: None,
            })],
            doc_comment: None,
            lang_item: None,
        };
        let import_b = ImportDecl {
            path: vec!["fakemod".to_string()],
            spec: None,
            module_alias: None,
            file_path: None,
            resolved_items: Some(vec![(Item::Trait(fake_trait.clone()), 0..30)]),
            resolved_item_source_paths: vec![],
            resolved_source_paths: vec![],
        };

        let root_module = Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let module_b = Module {
            id: owner_module_b_id.clone(),
            items: vec![(Item::Import(import_b), 100..130)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(root_module).unwrap();
        mg.add_module(module_b).unwrap();
        mg.topo_order = vec![owner_module_b_id, root_id];

        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.trait_defs.insert(
            "fakemod.FakeTrait".to_string(),
            Checker::trait_info_from_decl(&fake_trait),
        );
        checker
            .trait_impls_set
            .insert(("SomeType".to_string(), "FakeTrait".to_string()));

        let output = checker.check_program(&program);
        let unused: Vec<_> = output
            .warnings
            .iter()
            .filter(|w| {
                w.kind == TypeErrorKind::UnusedImport
                    && w.message.contains("fakemod")
                    && w.source_module.as_deref() == Some("owner_module_b")
            })
            .collect();

        assert_eq!(
            unused.len(),
            1,
            "expected owner_module_b's unused fakemod import to warn despite global FakeTrait impl, got {}: {:?}",
            unused.len(),
            output.warnings
        );
    }

    // ── Ty::Error return-context seeding regressions ──────────────────────────
    //
    // When a function's return-type annotation cannot be resolved (e.g.
    // `UnknownType`), `resolve_type_expr` produces `Ty::Error`.  Before this
    // fix the error type was passed as the *expected* type into the body,
    // causing `expect_type`'s guard (`expected_resolved != Ty::Error`) to
    // silently swallow genuine body-level type errors.

    fn has_mismatch(errors: &[crate::error::TypeError]) -> bool {
        errors.iter().any(|e| {
            matches!(e.kind, TypeErrorKind::Mismatch { .. })
                || e.message.contains("mismatch")
                || e.message.contains("TypeMismatch")
        })
    }

    #[test]
    fn error_return_type_does_not_suppress_trailing_expr_mismatch() {
        // fn foo() -> UnknownType { let x: i32 = "bad"; x }
        // The `let x: i32 = "bad"` is a type mismatch inside the body.
        // When the fn return annotation is Ty::Error the body was previously
        // checked with check_against(_, Ty::Error), masking the let-binding error.
        // After the fix the body is synthesized (expected=None), so the let
        // mismatch is still reported.
        let source = r#"fn foo() -> UnknownType { let x: i32 = "bad"; x }"#;
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            has_mismatch(&output.errors),
            "trailing-expr body mismatch must be reported even when return type \
             is Ty::Error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn error_return_type_does_not_suppress_explicit_return_mismatch() {
        // fn foo() -> UnknownType { let x: i32 = "bad"; return x; }
        // The let-binding mismatch must be reported.
        let source = r#"fn foo() -> UnknownType { let x: i32 = "bad"; return x; }"#;
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            has_mismatch(&output.errors),
            "body mismatch inside explicit return must be reported even when \
             return type is Ty::Error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn error_return_type_does_not_suppress_receive_fn_body_mismatch() {
        // receive fn handler() -> UnknownType { let x: i32 = "bad"; x }
        // inside an actor; body mismatch must be reported.
        let source = r#"
actor MyActor {
    var value: i32 = 0;
    receive fn handler() -> UnknownType { let x: i32 = "bad"; x }
}
"#;
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            has_mismatch(&output.errors),
            "body mismatch in receive fn must be reported even when return type \
             is Ty::Error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn error_return_type_does_not_suppress_lambda_annotated_return_mismatch() {
        // Lambda with annotated (unresolvable) return type:
        //   let f = (x: i32) -> UnknownType => { let y: i32 = "bad"; y };
        // The let-binding mismatch inside the lambda body must still be reported.
        let source = r#"fn foo() { let f = |x: i32| -> UnknownType { let y: i32 = "bad"; y }; }"#;
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            has_mismatch(&output.errors),
            "body mismatch in lambda annotated return must be reported even when \
             return type is Ty::Error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn error_return_type_question_mark_on_non_result_still_reports() {
        // fn foo() -> UnknownType { let x: i64 = 1; x? }
        //
        // The inner-type check for `?` ("? requires Result or Option, found i64")
        // fires unconditionally via the else branch of the PostfixTry handler.
        // It must not be suppressed even when the enclosing return annotation
        // resolves to Ty::Error — that only bypasses the *context* diagnostic
        // ("? cannot be used in a function returning X"), not the inner-type check.
        let source = r"fn foo() -> UnknownType { let x: i64 = 1; x? }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        let has_try_err = output
            .errors
            .iter()
            .any(|e| e.message.contains("requires Result or Option"));
        assert!(
            has_try_err,
            "? on non-Result/non-Option must still report \
             '? requires Result or Option' even when return annotation is \
             Ty::Error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn question_mark_result_check_passes_with_matching_error_type() {
        let source = r"
fn pass(r: Result<i64, i64>) -> Result<i64, i64> {
    let x: i64 = r?;
    Ok(x)
}
";
        let (errors, _) = parse_and_check(source);
        assert!(
            errors.is_empty(),
            "Result<T, E>? in Result<_, same E> function must check-pass; got {errors:?}"
        );
    }

    #[test]
    fn question_mark_option_check_passes_in_option_function() {
        let source = r"
fn pass(o: Option<i64>) -> Option<i64> {
    let x: i64 = o?;
    Some(x)
}
";
        let (errors, _) = parse_and_check(source);
        assert!(
            errors.is_empty(),
            "Option<T>? in Option<_> function must check-pass; got {errors:?}"
        );
    }

    #[test]
    fn question_mark_in_i64_returning_function_errors() {
        let source = r"
fn bad(r: Result<i64, i64>) -> i64 {
    r?
}
";
        let (errors, _) = parse_and_check(source);
        assert!(
            errors.iter().any(|e| e
                .message
                .contains("cannot be used in a function returning `i64`")),
            "`?` in an i64-returning function must be rejected; got {errors:?}"
        );
    }

    #[test]
    fn question_mark_result_error_type_mismatch_errors() {
        let source = r"
fn bad(r: Result<i64, string>) -> Result<i64, i64> {
    let x: i64 = r?;
    Ok(x)
}
";
        let (errors, _) = parse_and_check(source);
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("`?` error type mismatch")),
            "Result<T, E1>? in Result<_, E2> function must reject mismatched E; got {errors:?}"
        );
    }

    #[test]
    fn error_return_type_question_mark_on_result_no_false_context_error() {
        // fn foo() -> UnknownType { let r: Result<i64, string> = Ok(1); r? }
        //
        // When the return annotation is unresolvable (Ty::Error) and `?` is
        // used on a valid Result, the *context* diagnostic ("? cannot be used
        // in a function returning <error>") must NOT fire — we cannot know
        // whether the intended return type would have supported `?`.  Only the
        // annotation-resolution error should appear.
        let source = r"fn foo() -> UnknownType { let r: Result<i64, string> = Ok(1); r? }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        let has_ctx_err = output
            .errors
            .iter()
            .any(|e| e.message.contains("cannot be used in a function returning"));
        assert!(
            !has_ctx_err,
            "? on valid Result in bad-annotation function must NOT emit a \
             spurious context error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn builtin_named_return_type_still_reports_question_mark_context_error() {
        // fn foo() -> Vec<i32> { let r: Result<i64, string> = Ok(1); let x: i64 = r?; Vec::new() }
        //
        // PR #923 bypasses the `?` context diagnostic for genuinely unknown named
        // return annotations. Builtin named types like Vec must still report the
        // context error even though they are not registered in type_defs/type_aliases.
        let source = r"fn foo() -> Vec<i32> { let r: Result<i64, string> = Ok(1); let x: i64 = r?; Vec::new() }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        let has_ctx_err = output.errors.iter().any(|e| {
            e.message
                .contains("cannot be used in a function returning `Vec<i32>`")
        });
        assert!(
            has_ctx_err,
            "? on valid Result in a function returning builtin Vec must still \
             emit the context error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn error_return_type_question_mark_in_lambda_no_false_context_error() {
        // fn foo() { let r: Result<i64, string> = Ok(1); let f = |x: i64| -> UnknownType { r? }; }
        //
        // Same invariant as the plain-function case but inside a lambda whose
        // return annotation is Ty::Error.  The `?` context check sees the
        // lambda's own `current_return_type` (Ty::Error), so the Ty::Error
        // bypass must apply there too.
        let source = r"fn foo() { let r: Result<i64, string> = Ok(1); let f = |x: i64| -> UnknownType { r? }; }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        let has_ctx_err = output
            .errors
            .iter()
            .any(|e| e.message.contains("cannot be used in a function returning"));
        assert!(
            !has_ctx_err,
            "? on valid Result inside a lambda with bad return annotation must \
             NOT emit a spurious context error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn valid_return_annotation_good_path_regression_guard() {
        // fn foo() -> i32 { 42 }
        //
        // A function with a valid, resolvable return annotation must typecheck
        // cleanly.  This guards against regressions introduced by the Ty::Error
        // seeding fix inadvertently breaking the happy path (literal coercion,
        // trailing-expression checking, and expect_type alignment).
        let source = r"fn _foo() -> i32 { 42 }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "valid typed-return function must not produce type errors; \
             got: {:?}",
            output.errors
        );
        assert!(
            output.warnings.is_empty(),
            "valid typed-return function must not produce warnings; \
             got: {:?}",
            output.warnings
        );
    }

    // ── ExternBlock scoped-lookup regression ───────────────────────────────────
    //
    // When an extern block lives in a non-root module, the key stored in
    // `fn_sig_inference_holes` is scoped as `"mymod.extfn"` (set by
    // `register_extern_block` → `scoped_module_item_name`).
    //
    // Before the fix, `report_unresolved_inference_in_items` used a bare-name
    // lookup (`fn_sig_inference_holes.get("extfn")`), so the inference hole was
    // never detected and no `InferenceFailed` error was emitted for non-root
    // extern functions with `_`-typed parameters.
    //
    // After the fix the arm uses `lookup_scoped_item(…, module_name, "extfn")`
    // which resolves the scoped key and the error is emitted + tagged correctly.

    fn make_extern_block_with_infer_param(fn_name: &str) -> Item {
        Item::ExternBlock(ExternBlock {
            abi: "C".to_string(),
            functions: vec![ExternFnDecl {
                attributes: Vec::new(),
                name: fn_name.to_string(),
                params: vec![Param {
                    name: "p".to_string(),
                    ty: (TypeExpr::Infer, 20..21),
                    is_mutable: false,
                }],
                return_type: None,
                is_variadic: false,
                span: 0..0,
            }],
        })
    }

    /// A non-root extern function with a `_`-typed parameter must fail closed
    /// with `InferenceFailed` tagged `source_module = Some("mymod")`.
    ///
    /// Regression guard for the `Item::ExternBlock` bare-name lookup bug in
    /// `report_unresolved_inference_in_items`.
    #[test]
    fn non_root_extern_fn_infer_param_fails_closed_with_source_module() {
        let extern_item = make_extern_block_with_infer_param("extfn");
        let root_id = ModuleId::root();
        let mymod_id = ModuleId::new(vec!["mymod".to_string()]);

        let mymod = Module {
            id: mymod_id.clone(),
            items: vec![(extern_item, 0..30)],
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
        mg.add_module(mymod).unwrap();
        mg.topo_order = vec![mymod_id, root_id];

        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let inference_errs: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::InferenceFailed))
            .collect();

        assert!(
            !inference_errs.is_empty(),
            "non-root extern fn with `_` param must produce InferenceFailed; \
             got errors: {:?}",
            output.errors
        );
        for err in &inference_errs {
            assert_eq!(
                err.source_module.as_deref(),
                Some("mymod"),
                "InferenceFailed for non-root extern fn must carry \
                 source_module='mymod'; got {:?}",
                err.source_module
            );
        }
    }

    /// A root-module extern function with a `_`-typed parameter must also fail
    /// closed with `InferenceFailed`, with `source_module = None`.
    ///
    /// Confirms the fix does not break the root-module code path.
    #[test]
    fn root_extern_fn_infer_param_fails_closed_without_source_module() {
        let extern_item = make_extern_block_with_infer_param("root_extfn");
        let program = Program {
            module_graph: None,
            items: vec![(extern_item, 0..30)],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let inference_errs: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::InferenceFailed))
            .collect();

        assert!(
            !inference_errs.is_empty(),
            "root extern fn with `_` param must produce InferenceFailed; \
             got errors: {:?}",
            output.errors
        );
        for err in &inference_errs {
            assert_eq!(
                err.source_module, None,
                "InferenceFailed for root extern fn must have source_module=None; got {:?}",
                err.source_module
            );
        }
    }

    /// A non-root scope warning (`UnusedVariable`) carries `source_module`.
    ///
    /// Certifies that `emit_scope_warnings` copies `self.current_module` into
    /// the warning and the snapshot module tagging pass in `check_program` does
    /// not overwrite it when already set.
    #[test]
    fn non_root_unused_variable_warning_carries_source_module() {
        // fn warns() { let x = 42; }  — `x` is never read
        let stmts = vec![(
            Stmt::Let {
                pattern: (Pattern::Identifier("x".to_string()), 10..11),
                ty: None,
                value: Some((
                    Expr::Literal(Literal::Integer {
                        value: 42,
                        radix: IntRadix::Decimal,
                    }),
                    14..16,
                )),
                else_block: None,
            },
            10..16,
        )];
        let program = make_non_root_program_with_fn_body("warnmod", "warns", stmts);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let unused: Vec<_> = output
            .warnings
            .iter()
            .filter(|w| w.kind == TypeErrorKind::UnusedVariable)
            .collect();

        assert!(
            !unused.is_empty(),
            "expected UnusedVariable warning for `x` in non-root module; got warnings: {:?}",
            output.warnings
        );
        for w in &unused {
            assert_eq!(
                w.source_module.as_deref(),
                Some("warnmod"),
                "UnusedVariable warning must carry source_module='warnmod'; got {:?}",
                w.source_module
            );
        }
    }
}

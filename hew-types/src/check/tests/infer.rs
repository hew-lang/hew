#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

#[cfg(test)]
mod non_root_module_inference_scope {
    use super::*;

    fn make_non_root_module(
        mod_id: &ModuleId,
        fn_name: &str,
        param_ty: TypeExpr,
        return_ty: Option<TypeExpr>,
    ) -> Module {
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: fn_name.to_string(),
            type_params: None,
            params: vec![Param {
                name: "x".to_string(),
                ty: (param_ty, 10..11),
                is_mutable: false,
                is_consume: false,
            }],
            return_type: return_ty.map(|ty| (ty, 15..16)),
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
        Module {
            id: mod_id.clone(),
            items: vec![(Item::Function(fn_decl), 0..30)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        }
    }

    fn make_program_with_non_root(module: Module) -> Program {
        let root_id = ModuleId::root();
        let mod_id = module.id.clone();
        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(module).unwrap();
        // Dependencies (non-root) come before the root in topo order.
        mg.topo_order = vec![mod_id, root_id];
        Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        }
    }

    // Helper: collect InferenceFailed errors from the output.
    fn inference_failed_errors(output: &TypeCheckOutput) -> Vec<&TypeError> {
        output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::InferenceFailed))
            .collect()
    }

    /// A non-root module function whose parameter type is `_` must produce an
    /// `InferenceFailed` error via `report_unresolved_inference_holes`.
    #[test]
    fn fn_param_infer_hole_fails_closed() {
        let mod_id = ModuleId::new(vec!["utils".to_string()]);
        let module = make_non_root_module(&mod_id, "helper", TypeExpr::Infer, None);
        let program = make_program_with_non_root(module);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            !errs.is_empty(),
            "expected InferenceFailed for unresolved `_` param in non-root module fn; got errors: {:?}",
            output.errors
        );
    }

    /// A non-root module function whose return type is `_` now resolves from
    /// body-checking just like a root-module function. An empty body therefore
    /// resolves `_` to `unit` instead of leaving an unresolved inference hole.
    #[test]
    fn fn_return_infer_hole_resolves_from_body() {
        let mod_id = ModuleId::new(vec!["helpers".to_string()]);
        let concrete_param = TypeExpr::Named {
            name: "i64".to_string(),
            type_args: None,
        };
        let module =
            make_non_root_module(&mod_id, "compute", concrete_param, Some(TypeExpr::Infer));
        let program = make_program_with_non_root(module);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.is_empty(),
            "non-root `_` return type should resolve from body-checking; got InferenceFailed errors: {errs:?}"
        );
        assert!(
            output.errors.is_empty(),
            "non-root `_` return type should resolve cleanly; got errors: {:?}",
            output.errors
        );
    }

    /// A non-root module function with fully concrete types must not produce
    /// any `InferenceFailed` errors (baseline / regression guard).
    #[test]
    fn fn_concrete_types_passes() {
        let mod_id = ModuleId::new(vec!["math".to_string()]);
        let concrete_param = TypeExpr::Named {
            name: "i64".to_string(),
            type_args: None,
        };
        let concrete_return = TypeExpr::Named {
            name: "i64".to_string(),
            type_args: None,
        };
        let module = make_non_root_module(&mod_id, "add", concrete_param, Some(concrete_return));
        let program = make_program_with_non_root(module);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.is_empty(),
            "concrete non-root module fn must not produce InferenceFailed; got: {errs:?}"
        );
    }

    /// A non-root module function with `_` param must not prevent the root
    /// module items from being checked — root errors remain independent.
    #[test]
    fn infer_hole_in_non_root_does_not_suppress_root_errors() {
        use hew_parser::ast::{FnDecl, Item};

        let mod_id = ModuleId::new(vec!["side".to_string()]);
        let module = make_non_root_module(&mod_id, "side_fn", TypeExpr::Infer, None);

        // Root module also has a function with `_` param — should also error.
        let root_fn = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: "root_fn".to_string(),
            type_params: None,
            params: vec![Param {
                name: "v".to_string(),
                ty: (TypeExpr::Infer, 50..51),
                is_mutable: false,
                is_consume: false,
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

        let root_id = ModuleId::root();
        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(module).unwrap();
        mg.topo_order = vec![mod_id, root_id];
        let program = Program {
            module_graph: Some(mg),
            items: vec![(Item::Function(root_fn), 40..80)],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.len() >= 2,
            "expected InferenceFailed for both the non-root module fn and the root fn; got: {:?}",
            output.errors
        );
    }

    /// Two non-root module functions both with `_` params must each produce
    /// an `InferenceFailed` error — holes are not collapsed into a single error.
    #[test]
    fn multiple_infer_holes_in_non_root_all_fail_closed() {
        use hew_parser::ast::{Block, FnDecl, Item, Param, TypeExpr};
        let mod_id = ModuleId::new(vec!["util2".to_string()]);
        let root_id = ModuleId::root();

        let make_infer_fn = |name: &str, span_start: usize| -> Spanned<Item> {
            let fd = FnDecl {
                attributes: vec![],
                is_async: false,
                is_generator: false,
                visibility: Visibility::Private,
                name: name.to_string(),
                type_params: None,
                params: vec![Param {
                    name: "a".to_string(),
                    ty: (TypeExpr::Infer, span_start..span_start + 1),
                    is_mutable: false,
                    is_consume: false,
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
            (Item::Function(fd), span_start..span_start + 30)
        };

        let non_root = Module {
            id: mod_id.clone(),
            items: vec![make_infer_fn("alpha", 0), make_infer_fn("beta", 40)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(non_root).unwrap();
        mg.topo_order = vec![mod_id, root_id];
        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.len() >= 2,
            "expected at least 2 InferenceFailed errors for two `_`-param fns in non-root module; got: {:?}",
            output.errors
        );
    }

    /// A non-root module body containing `let y = x as _` must fail closed:
    /// the unresolved `_` cast target must produce an `InferenceFailed` error.
    ///
    /// Regression test: deferred inference holes created during non-root module
    /// body checking (via `synthesize_cast`) were not reported by
    /// `report_unresolved_inference_holes` because the body-level deferred-hole
    /// list was only flushed for the *root* module's item walk, not for
    /// non-root module bodies.
    #[test]
    fn body_cast_infer_hole_fails_closed() {
        // fn foo(x: i64) { let y = x as _; }  — `_` cast target is unresolved
        let mod_id = ModuleId::new(vec!["castmod".to_string()]);
        let root_id = ModuleId::root();

        let cast_expr = Expr::Cast {
            expr: Box::new((Expr::Identifier("x".to_string()), 20..21)),
            ty: (TypeExpr::Infer, 25..26),
        };
        let let_stmt = Stmt::Let {
            pattern: (Pattern::Identifier("y".to_string()), 14..15),
            ty: None,
            value: Some((cast_expr, 18..26)),
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
                    7..10,
                ),
                is_mutable: false,
                is_consume: false,
            }],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![(let_stmt, 13..27)],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
            consumes_self: false,
        };

        let non_root = Module {
            id: mod_id.clone(),
            items: vec![(Item::Function(fn_decl), 0..30)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(non_root).unwrap();
        mg.topo_order = vec![mod_id, root_id];
        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            !errs.is_empty(),
            "expected InferenceFailed for unresolved `_` cast target in non-root module body; got errors: {:?}",
            output.errors
        );
    }

    /// A non-root module body with `let y: _ = 42` must resolve the `_`
    /// annotation to `i64` from the value — no spurious `InferenceFailed`.
    ///
    /// This is the positive-path counterpart to `body_cast_infer_hole_fails_closed`:
    /// a deferred annotation hole that IS constrained by body-checking must not
    /// produce a false diagnostic.  Before PR #756 added non-root body checking,
    /// this would have left the type-var unresolved and erroneously fired.
    #[test]
    fn body_let_annotation_infer_resolves_cleanly() {
        // fn bar() { let y: _ = 42; }  — `_` must resolve to i64 from the value
        let mod_id = ModuleId::new(vec!["letmod".to_string()]);
        let root_id = ModuleId::root();

        let let_stmt = Stmt::Let {
            pattern: (Pattern::Identifier("y".to_string()), 14..15),
            ty: Some((TypeExpr::Infer, 17..18)),
            value: Some(make_int_literal(42, 21..23)),
            else_block: None,
        };
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: "bar".to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![(let_stmt, 13..24)],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
            consumes_self: false,
        };

        let non_root = Module {
            id: mod_id.clone(),
            items: vec![(Item::Function(fn_decl), 0..30)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(non_root).unwrap();
        mg.topo_order = vec![mod_id, root_id];
        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.is_empty(),
            "let `_` annotation must resolve from value — no InferenceFailed expected; got: {errs:?}"
        );
        assert!(
            output.errors.is_empty(),
            "let `_` annotation must resolve cleanly; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn inferred_binding_without_annotation_fails_closed() {
        let source = "fn main() { let f = |x| x; }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.iter()
                .any(|err| err.message.contains("local binding `f`")),
            "expected InferenceFailed for unresolved inferred binding `f`; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn explicit_generic_lambda_binding_rejected() {
        // Generic lambda `<T>(params) => body` was removed in v0.5.
        // The parser must emit a typed E_CLOSURE_PIPE_SYNTAX diagnostic.
        let source = "fn main() { let id = <T>(x: T) => x; }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.iter().any(|e| {
                matches!(e.kind, hew_parser::ParseDiagnosticKind::ClosurePipeSyntax)
                    && e.message.contains("E_CLOSURE_PIPE_SYNTAX")
            }),
            "expected typed E_CLOSURE_PIPE_SYNTAX for removed generic lambda, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn unresolved_inferred_return_through_none_fails_closed() {
        let source = "fn maybe() -> _ { None }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);

        let errs = inference_failed_errors(&output);
        assert!(
            !errs.is_empty(),
            "expected InferenceFailed for unresolved `None` return; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn trait_default_method_explicit_infer_return_registers_qualified_signature() {
        use hew_parser::ast::{TraitDecl, TraitItem};

        let trait_decl = TraitDecl {
            visibility: Visibility::Private,
            name: "Answerer".to_string(),
            type_params: None,
            super_traits: None,
            items: vec![TraitItem::Method(TraitMethod {
                name: "answer".to_string(),
                type_params: None,
                params: vec![Param {
                    name: "value".to_string(),
                    ty: (
                        TypeExpr::Named {
                            name: "i64".to_string(),
                            type_args: None,
                        },
                        12..15,
                    ),
                    is_mutable: false,
                    is_consume: false,
                }],
                return_type: Some((TypeExpr::Infer, 10..11)),
                where_clause: None,
                body: Some(Block {
                    stmts: vec![],
                    trailing_expr: Some(Box::new((Expr::Identifier("value".to_string()), 20..25))),
                }),
                span: 0..0,
                doc_comment: None,
                lang_item: None,
            })],
            doc_comment: None,
            lang_item: None,
        };
        let program = Program {
            module_graph: None,
            items: vec![(Item::Trait(trait_decl), 0..30)],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        assert!(
            output.errors.is_empty(),
            "trait default `->_` return should resolve cleanly; got errors: {:?}",
            output.errors
        );
        assert_eq!(output.fn_sigs["Answerer::answer"].return_type, Ty::I64);
    }

    #[test]
    fn trait_default_method_unresolved_explicit_infer_return_fails_closed() {
        use hew_parser::ast::{TraitDecl, TraitItem};

        let trait_decl = TraitDecl {
            visibility: Visibility::Private,
            name: "Answerer".to_string(),
            type_params: None,
            super_traits: None,
            items: vec![TraitItem::Method(TraitMethod {
                name: "answer".to_string(),
                type_params: None,
                params: vec![],
                return_type: Some((TypeExpr::Infer, 10..11)),
                where_clause: None,
                body: Some(Block {
                    stmts: vec![],
                    trailing_expr: Some(Box::new((Expr::Identifier("None".to_string()), 20..24))),
                }),
                span: 0..0,
                doc_comment: None,
                lang_item: None,
            })],
            doc_comment: None,
            lang_item: None,
        };
        let program = Program {
            module_graph: None,
            items: vec![(Item::Trait(trait_decl), 0..30)],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.iter().any(|err| err
                .message
                .contains("signature of trait method `Answerer::answer`")),
            "expected InferenceFailed for unresolved trait default `->_`; got errors: {:?}",
            output.errors
        );
        assert!(
            !output.fn_sigs.contains_key("Answerer::answer"),
            "failing trait method signature should be stripped from checker output: {:?}",
            output.fn_sigs
        );
    }

    #[test]
    fn trait_default_method_explicit_infer_return_propagates_to_impl_method_signature() {
        use hew_parser::ast::{ImplDecl, TraitBound, TraitDecl, TraitItem, TypeDecl, TypeDeclKind};

        let trait_decl = TraitDecl {
            visibility: Visibility::Private,
            name: "Answerer".to_string(),
            type_params: None,
            super_traits: None,
            items: vec![TraitItem::Method(TraitMethod {
                name: "answer".to_string(),
                type_params: None,
                params: vec![Param {
                    name: "value".to_string(),
                    ty: (
                        TypeExpr::Named {
                            name: "i64".to_string(),
                            type_args: None,
                        },
                        12..15,
                    ),
                    is_mutable: false,
                    is_consume: false,
                }],
                return_type: Some((TypeExpr::Infer, 10..11)),
                where_clause: None,
                body: Some(Block {
                    stmts: vec![],
                    trailing_expr: Some(Box::new((Expr::Identifier("value".to_string()), 20..25))),
                }),
                span: 0..0,
                doc_comment: None,
                lang_item: None,
            })],
            doc_comment: None,
            lang_item: None,
        };
        let greeter = TypeDecl {
            visibility: Visibility::Private,
            kind: TypeDeclKind::Struct,
            name: "Greeter".to_string(),
            type_params: None,
            where_clause: None,
            body: vec![],
            doc_comment: None,
            wire: None,
            is_indirect: false,
            resource_marker: hew_parser::ast::ResourceMarker::None,
            is_opaque: false,
            consuming_methods: Vec::new(),
            lang_item: None,
        };
        let impl_decl = ImplDecl {
            type_params: None,
            trait_bound: Some(TraitBound {
                name: "Answerer".to_string(),
                type_args: None,
                assoc_type_bindings: vec![],
            }),
            target_type: (
                TypeExpr::Named {
                    name: "Greeter".to_string(),
                    type_args: None,
                },
                30..37,
            ),
            where_clause: None,
            type_aliases: vec![],
            methods: vec![],
        };
        let program = Program {
            module_graph: None,
            items: vec![
                (Item::Trait(trait_decl), 0..30),
                (Item::TypeDecl(greeter), 31..40),
                (Item::Impl(impl_decl), 41..60),
            ],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        assert!(
            output.errors.is_empty(),
            "default impl method signature should inherit resolved trait return type; got errors: {:?}",
            output.errors
        );
        assert_eq!(output.fn_sigs["Greeter::answer"].return_type, Ty::I64);
    }

    #[test]
    fn trait_default_method_with_concrete_receiver_keeps_implicit_impl_arity() {
        let source = r"
            type Greeter {
                id: i64;
            }

            trait Answerer {
                fn answer(g: Greeter) -> i64 {
                    42
                }
            }

            impl Answerer for Greeter {}
        ";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "unexpected parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);

        assert!(
            output.errors.is_empty(),
            "default impl method with a concrete receiver should typecheck cleanly; got errors: {:?}",
            output.errors
        );
        assert!(
            output.fn_sigs["Greeter::answer"].params.is_empty(),
            "default impl method should not expose the concrete receiver as an explicit argument: {:?}",
            output.fn_sigs["Greeter::answer"]
        );
    }

    #[test]
    fn inferred_binding_does_not_duplicate_lambda_hole_error() {
        let source = "fn main() { let f = |x: _| x; }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);

        let errs = inference_failed_errors(&output);
        assert_eq!(
            errs.len(),
            1,
            "expected only the lambda hole diagnostic, got: {:?}",
            output.errors
        );
        assert!(
            errs[0].message.contains("lambda parameter `x`"),
            "expected lambda hole diagnostic, got: {:?}",
            output.errors
        );
    }

    #[test]
    fn inferred_binding_does_not_duplicate_cast_hole_error() {
        let source = "fn main(x: i64) { let y = x as _; }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);

        let errs = inference_failed_errors(&output);
        assert_eq!(
            errs.len(),
            1,
            "expected only the cast hole diagnostic, got: {:?}",
            output.errors
        );
        assert!(
            errs[0].message.contains("cast target type"),
            "expected cast hole diagnostic, got: {:?}",
            output.errors
        );
    }

    #[test]
    fn bare_channel_handle_signature_stays_valid() {
        let source = concat!(
            "import std::channel::channel;\n",
            "fn close_sender(tx: channel.Sender) {\n",
            "    tx.close();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        let errs = inference_failed_errors(&output);
        assert!(
            errs.is_empty(),
            "bare channel handle signatures should not produce InferenceFailed: {errs:?}"
        );
        assert!(
            output.errors.is_empty(),
            "bare channel handle signatures should type-check cleanly: {:?}",
            output.errors
        );
    }

    /// A non-root module body containing a lambda with an unresolved `_`
    /// parameter type must fail closed: the deferred hole created during
    /// body checking must produce an `InferenceFailed` error.
    ///
    /// Regression: the lambda-param deferred-hole path
    /// (`expressions.rs::synthesize_lambda`) must also drain into
    /// `report_unresolved_inference_holes` for non-root module bodies.
    #[test]
    fn body_lambda_infer_param_hole_fails_closed() {
        // fn foo() { let f = |x: _| x; }  — lambda param `_` never constrained
        let mod_id = ModuleId::new(vec!["lambdamod".to_string()]);
        let root_id = ModuleId::root();

        // |x: _| x  — lambda with infer-typed parameter, no call site to resolve it
        let lambda_expr = Expr::Lambda {
            is_move: false,
            type_params: None,
            params: vec![LambdaParam {
                name: "x".to_string(),
                ty: Some((TypeExpr::Infer, 15..16)),
                name_span: 14..15,
            }],
            return_type: None,
            body: Box::new((Expr::Identifier("x".to_string()), 19..20)),
        };
        let let_stmt = Stmt::Let {
            pattern: (Pattern::Identifier("f".to_string()), 10..11),
            ty: None,
            value: Some((lambda_expr, 14..21)),
            else_block: None,
        };
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: "foo".to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![(let_stmt, 9..22)],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
            consumes_self: false,
        };

        let non_root = Module {
            id: mod_id.clone(),
            items: vec![(Item::Function(fn_decl), 0..30)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(non_root).unwrap();
        mg.topo_order = vec![mod_id, root_id];
        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            !errs.is_empty(),
            "expected InferenceFailed for unresolved lambda `_` param in non-root module body; got errors: {:?}",
            output.errors
        );
    }
}

// ── module_graph body typecheck parity (v0.3 blocker) ────────────────────────
//
// Non-root module_graph bodies must be typechecked, not just registered.
// A type error in an imported module body must not be silently missed.
// (make_program_with_module_graph helper is in tests/mod.rs)

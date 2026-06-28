// Re-export the parent check module's public items so that each submodule
// (and their inner `mod` blocks) can reach them via `use super::*`.
pub(super) use super::coerce::common_integer_type;
pub(super) use super::methods::RecordCloneAdmissibility;
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;
pub(super) use crate::check::registration::StdlibBarePublication;
pub(super) use crate::eq_eligibility::{ty_is_eq_eligible, EqEligibility};
pub(super) use crate::module_registry::ModuleRegistry;
pub(super) use crate::BuiltinType;
pub(super) use hew_parser::ast::IntRadix;
pub(super) use hew_parser::ast::{ImportName, TraitMethod, TypeExpr, Visibility};
pub(super) use hew_parser::module::{Module, ModuleGraph, ModuleId};

mod actor_fields;
mod basic;
mod builtins;
mod collections;
mod control_flow;
mod exhaustiveness;
mod extern_fn;
mod genblocks;
mod generics;
mod handles;
mod imports;
mod indexing;
mod infer;
mod intrinsics;
mod lints;
mod modules;
mod mut_receiver;
mod option_none;
mod output;
mod patterns;
mod records;
mod supervisor;
mod traits;
mod wasm;
mod wire;

// Shared test helpers used across multiple submodules.

/// Build a single-module program whose one module has the given dotted path
/// (e.g. `["std", "math"]`) and contains `source`'s items.
pub(super) fn check_source_in_module(source: &str, module_path: Vec<String>) -> TypeCheckOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "module source must parse cleanly, got: {:?}",
        parsed.errors
    );
    let root_id = ModuleId::root();
    let mod_id = ModuleId::new(module_path);
    let module = Module {
        id: mod_id.clone(),
        items: parsed.program.items,
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let mut mg = ModuleGraph::new(root_id.clone());
    mg.add_module(module).unwrap();
    mg.topo_order = vec![mod_id, root_id];
    let program = Program {
        module_graph: Some(mg),
        items: vec![],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.check_program(&program)
}

pub(super) fn make_int_literal(n: i64, span: Span) -> Spanned<Expr> {
    (
        Expr::Literal(Literal::Integer {
            value: n,
            radix: IntRadix::Decimal,
        }),
        span,
    )
}

pub(super) fn make_bool_literal(b: bool, span: Span) -> Spanned<Expr> {
    (Expr::Literal(Literal::Bool(b)), span)
}

/// Helper: parse + typecheck, return (errors, warnings).
pub(super) fn parse_and_check(source: &str) -> (Vec<TypeError>, Vec<TypeError>) {
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    (output.errors, output.warnings)
}

pub(super) fn parse_and_check_with_stdlib(source: &str) -> (Vec<TypeError>, Vec<TypeError>) {
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);
    (output.errors, output.warnings)
}

/// Helper: build a simple pub function declaration.
pub(super) fn make_pub_fn(name: &str, params: Vec<Param>, ret: Option<TypeExpr>) -> FnDecl {
    FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        name: name.to_string(),
        type_params: None,
        params,
        return_type: ret.map(|te| (te, 0..0)),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new(make_int_literal(0, 0..1))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    }
}

/// Helper: build a private (non-pub) function declaration.
pub(super) fn make_priv_fn(name: &str) -> FnDecl {
    FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        name: name.to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new(make_int_literal(0, 0..1))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    }
}

/// Helper: build an `ImportDecl` with resolved items.
pub(super) fn make_user_import(
    path: &[&str],
    spec: Option<ImportSpec>,
    items: Vec<Spanned<Item>>,
) -> ImportDecl {
    ImportDecl {
        path: path.iter().map(ToString::to_string).collect(),
        spec,
        module_alias: None,
        file_path: None,
        resolved_items: Some(items),
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    }
}

/// Helper: type-check a program with given items.
pub(super) fn check_items(items: Vec<Spanned<Item>>) -> TypeCheckOutput {
    let program = Program {
        module_graph: None,
        items,
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.check_program(&program)
}

/// Build a minimal `Checker` with a trait registered in `trait_defs`.
///
/// The trait is method-only (no associated types, no generic methods) unless
/// the caller opts in via the `with_assoc` / `with_generic_method` flags.
pub(super) fn make_checker_with_trait(
    trait_name: &str,
    method_names: &[&str],
    with_assoc: bool,
    with_generic_method: bool,
) -> Checker {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    let mut items: Vec<hew_parser::ast::TraitItem> = method_names
        .iter()
        .map(|name| {
            let type_params = if with_generic_method {
                Some(vec![TypeParam {
                    name: "U".to_string(),
                    bounds: vec![],
                }])
            } else {
                None
            };
            TraitItem::Method(TraitMethod {
                name: name.to_string(),
                type_params,
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
            })
        })
        .collect();

    if with_assoc {
        items.push(TraitItem::AssociatedType {
            name: "Output".to_string(),
            default: None,
            bounds: vec![],
            span: 0..0,
        });
    }

    let td = TraitDecl {
        visibility: hew_parser::ast::Visibility::Private,
        name: trait_name.to_string(),
        type_params: None,
        super_traits: None,
        items,
        doc_comment: None,
        lang_item: None,
    };

    let info = Checker::trait_info_from_decl(&td);
    checker.trait_defs.insert(trait_name.to_string(), info);
    checker
}

pub(super) fn make_test_type_def(
    name: &str,
    type_params: Vec<String>,
    methods: HashMap<String, FnSig>,
) -> TypeDef {
    TypeDef {
        kind: TypeDefKind::Struct,
        name: name.to_string(),
        type_params,
        bounds: HashMap::new(),
        fields: HashMap::new(),
        variants: HashMap::new(),
        methods,
        doc_comment: None,
        field_order: vec![],
        is_indirect: false,
    }
}

/// Build a minimal two-module `Program`: a root module (empty) and a single
/// non-root module `mymod` containing the supplied items.
pub(super) fn make_program_with_module_graph(non_root_items: Vec<Spanned<Item>>) -> Program {
    let root_id = ModuleId::root();
    let non_root_id = ModuleId::new(vec!["mymod".to_string()]);

    let root_module = Module {
        id: root_id.clone(),
        items: vec![],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let non_root_module = Module {
        id: non_root_id.clone(),
        items: non_root_items,
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };

    let mut mg = ModuleGraph::new(root_id.clone());
    mg.add_module(root_module).unwrap();
    mg.add_module(non_root_module).unwrap();
    // Dependency order: non-root first, then root (root depends on mymod).
    mg.topo_order = vec![non_root_id, root_id];

    Program {
        module_graph: Some(mg),
        items: vec![],
        module_doc: None,
    }
}

/// Module registry with the repo root as a search path, so stdlib
/// modules (e.g. `std::encoding::json`) can be loaded during tests.
pub(super) fn test_registry() -> ModuleRegistry {
    let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf();
    ModuleRegistry::new(vec![repo_root])
}

pub(super) fn check_source(source: &str) -> TypeCheckOutput {
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "check_source should parse cleanly, got: {:#?}",
        parse_result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.check_program(&parse_result.program)
}

#[test]
fn test_empty_program() {
    let output = check_source("");
    assert!(output.errors.is_empty());
}

#[test]
fn tuple_numeric_field_access_out_of_bounds_is_rejected() {
    let output = check_source("fn main() -> i64 { let t = (1, false); t.2 }");
    assert!(
        output.errors.iter().any(|error| {
            error.kind == TypeErrorKind::UndefinedField
                && error.message.contains("tuple index 2 out of range")
        }),
        "expected tuple index out-of-range UndefinedField error, got: {:#?}",
        output.errors
    );
}

#[test]
fn user_impl_drop_rejected_fail_closed() {
    let output = check_source(
        r"
        type Token { id: i64 }
        impl Drop for Token {
            fn drop(token: Token) {
                println(token.id);
            }
        }
        fn main() {
            let _token = Token { id: 1 };
        }
        ",
    );
    assert!(
        output.errors.iter().any(|error| {
            error.kind == TypeErrorKind::InvalidOperation
                && error
                    .message
                    .contains("`impl Drop` is not supported (its `drop` method would not run)")
        }),
        "expected fail-closed impl Drop diagnostic, got: {:#?}",
        output.errors
    );
}

#[test]
fn freshen_inner_recurses_into_pointer_pointee_vars() {
    let checker = Checker::new(ModuleRegistry::new(vec![]));
    let original = TypeVar::fresh();
    let ty = Ty::Pointer {
        is_mutable: false,
        pointee: Box::new(Ty::Var(original)),
    };
    let mut mapping = HashMap::new();

    let freshened = checker.freshen_inner(&ty, &mut mapping);

    let Ty::Pointer { pointee, .. } = freshened else {
        panic!("expected pointer type");
    };
    let Ty::Var(fresh) = *pointee else {
        panic!("expected freshened pointee var");
    };

    assert_ne!(fresh, original);
    assert_eq!(mapping.get(&original.0), Some(&Ty::Var(fresh)));
}

#[test]
fn freshen_inner_recurses_into_trait_object_bound_args() {
    let checker = Checker::new(ModuleRegistry::new(vec![]));
    let original = TypeVar::fresh();
    let ty = Ty::TraitObject {
        traits: vec![crate::ty::TraitObjectBound {
            trait_name: "Iterator".to_string(),
            args: vec![Ty::Var(original)],
            assoc_bindings: vec![],
        }],
    };
    let mut mapping = HashMap::new();

    let freshened = checker.freshen_inner(&ty, &mut mapping);

    let Ty::TraitObject { traits } = freshened else {
        panic!("expected trait object type");
    };
    let [bound] = traits.as_slice() else {
        panic!("expected one trait bound");
    };
    let [Ty::Var(fresh)] = bound.args.as_slice() else {
        panic!("expected freshened trait-object arg var");
    };

    assert_ne!(*fresh, original);
    assert_eq!(mapping.get(&original.0), Some(&Ty::Var(*fresh)));
}

#[test]
fn cancellation_token_local_and_is_cancelled_typecheck() {
    let output = check_source(
        r"
        fn observe(token: CancellationToken) -> bool {
            let t: CancellationToken = token;
            return t.is_cancelled();
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "CancellationToken local and is_cancelled() should type-check: {:#?}",
        output.errors
    );
    assert!(
        output
            .method_call_rewrites
            .values()
            .any(|rewrite| { matches!(rewrite, MethodCallRewrite::CancellationTokenIsCancelled) }),
        "is_cancelled() must record the checker-owned cancellation-token intrinsic"
    );
}

#[test]
fn cancellation_token_has_no_cancel_method() {
    let output = check_source(
        r"
        fn observe(token: CancellationToken) {
            token.cancel();
        }
        ",
    );

    assert!(
        output.errors.iter().any(|err| {
            matches!(err.kind, TypeErrorKind::UndefinedMethod)
                && err
                    .message
                    .contains("no method `cancel` on `CancellationToken`")
        }),
        "CancellationToken.cancel() must remain out of scope: {:#?}",
        output.errors
    );
}

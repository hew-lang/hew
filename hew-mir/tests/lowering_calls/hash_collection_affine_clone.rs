//! Monomorphized clone-totality gates for `HashMap` and `HashSet`.
//!
//! Generic collection bodies are checked while their K/V/T parameters are
//! abstract. MIR must re-prove every descriptor-backed clone after
//! substitution so resource/linear values never reach a runtime clone choke.

use std::collections::HashMap;

use hew_hir::{
    ids::IdGen, lower_program, HirBlock, HirExpr, HirExprKind, HirFn, HirItem, HirLiteral,
    HirModule, HirStmt, HirStmtKind, IntentKind, ResolutionCtx, ResourceMarker, ScopeId,
    TypeClassTable, ValueClass,
};
use hew_mir::{lower_hir_module, IrPipeline};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{
    BuiltinType, CallTarget, Checker, HashMapMethod, HashSetMethod, ImplId, MethodTargetFamily,
    ResolvedTy, TyPattern,
};

fn pipeline_allowing_mir_diagnostics(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
    let hir = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        hir.diagnostics
    );
    lower_hir_module(&hir.module)
}

fn pipeline(source: &str) -> IrPipeline {
    let pipeline = pipeline_allowing_mir_diagnostics(source);
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    pipeline
}

fn assert_affine_rejection(pipeline: &IrPipeline, operation: &str) {
    assert!(
        pipeline.diagnostics.iter().any(|diagnostic| {
            matches!(
                &diagnostic.kind,
                hew_mir::MirDiagnosticKind::NotYetImplemented { .. }
            ) && diagnostic.note.contains(operation)
                && diagnostic.note.contains("affine close contract")
        }),
        "missing affine {operation} rejection: {:#?}",
        pipeline.diagnostics
    );
}

fn constructed_affine_collection_call(
    receiver_ty: ResolvedTy,
    target_symbol: &str,
    target_family: MethodTargetFamily,
    type_args: Vec<TyPattern>,
) -> IrPipeline {
    let mut ids = IdGen::default();
    let receiver = HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: receiver_ty,
        value_class: ValueClass::Unknown,
        intent: IntentKind::Read,
        kind: HirExprKind::Literal(HirLiteral::Unit),
        span: 0..0,
    };
    let call = HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: ResolvedTy::Unit,
        value_class: ValueClass::Unknown,
        intent: IntentKind::Read,
        kind: HirExprKind::ResolvedImplCall {
            receiver: Box::new(receiver),
            target: CallTarget::RuntimeCollection(target_family),
            impl_id: ImplId(0),
            method_name: "clone-boundary".to_string(),
            target_symbol: target_symbol.to_string(),
            target_family,
            type_args,
            args: vec![],
            ret_ty: ResolvedTy::Unit,
        },
        span: 0..0,
    };
    let stmt = HirStmt {
        node: ids.node(),
        kind: HirStmtKind::Expr(call),
        span: 0..0,
    };
    let body = HirBlock {
        node: ids.node(),
        scope: ScopeId(0),
        statements: vec![stmt],
        tail: None,
        ty: ResolvedTy::Unit,
        span: 0..0,
    };
    let mut type_classes = TypeClassTable::default();
    type_classes.insert(
        "Token".to_string(),
        (ResourceMarker::Resource, Some("close".to_string())),
    );
    let module = HirModule {
        entry_exit_plan: None,
        items: vec![HirItem::Function(HirFn {
            id: ids.item(),
            node: ids.node(),
            declaration: hew_types::DefId::for_test("main"),
            name: "main".to_string(),
            type_params: vec![],
            params: vec![],
            return_ty: ResolvedTy::Unit,
            body,
            span: 0..0,
            is_generator: false,
            intrinsic_id: None,
        })],
        // Hand-built HIR intentionally has no checker-origin producer facts.
        produced_value_facts: HashMap::new(),
        diagnostic_source_modules: HashMap::new(),
        root_item_ids: std::collections::HashSet::new(),
        caller_visible_param_projections: std::collections::HashSet::new(),
        wire_layouts: std::sync::Arc::new(HashMap::new()),
        type_classes,
        monomorphisations: vec![],
        call_site_type_args: HashMap::new(),
        vec_generic_element_abi: HashMap::new(),
        record_layouts: vec![],
        enum_layouts: vec![],
        machine_instantiations: vec![],
        supervisor_child_slots: HashMap::new(),
        pool_accessor_sites: HashMap::new(),
        regex_literals: vec![],
    };
    lower_hir_module(&module)
}

fn token_ty() -> ResolvedTy {
    ResolvedTy::Named {
        name: "Token".to_string(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    }
}

const RESOURCE_TYPE: &str = r"
    #[resource]
    type Token { id: i64 }
    impl Token { fn close(self) {} }
";

#[test]
fn generic_hashmap_clone_rejects_resource_value_after_substitution() {
    let source = format!(
        r#"
        {RESOURCE_TYPE}
        fn duplicate<V>(values: HashMap<string, V>) -> HashMap<string, V> {{
            values.clone()
        }}
        fn main() {{
            var values: HashMap<string, Token> = HashMap.new();
            values.insert("live", Token {{ id: 1 }});
            let _copy = duplicate(values);
        }}
        "#
    );
    let pipeline = pipeline_allowing_mir_diagnostics(&source);
    assert_affine_rejection(&pipeline, "HashMap.clone()");
}

#[test]
fn hashmap_get_and_index_reject_resource_clone_out() {
    let get_source = format!(
        r#"
        {RESOURCE_TYPE}
        fn main() {{
            var values: HashMap<string, Token> = HashMap.new();
            values.insert("live", Token {{ id: 2 }});
            let _copy = values.get("live");
        }}
        "#
    );
    let get_pipeline = pipeline_allowing_mir_diagnostics(&get_source);
    assert_affine_rejection(&get_pipeline, "HashMap.get()");

    let index_source = format!(
        r#"
        {RESOURCE_TYPE}
        fn main() -> i64 {{
            var values: HashMap<string, Token> = HashMap.new();
            values.insert("live", Token {{ id: 3 }});
            let copy = values["live"];
            copy.id
        }}
        "#
    );
    let index_pipeline = pipeline_allowing_mir_diagnostics(&index_source);
    assert_affine_rejection(&index_pipeline, "HashMap indexing");
}

#[test]
fn mir_guards_affine_hash_projections_and_set_elements_if_upstream_admission_changes() {
    let hashmap_token_i64 = ResolvedTy::named_builtin(
        "HashMap",
        BuiltinType::HashMap,
        vec![token_ty(), ResolvedTy::I64],
    );
    let keys = constructed_affine_collection_call(
        hashmap_token_i64,
        "hew_hashmap_keys_layout",
        MethodTargetFamily::HashMap(HashMapMethod::Keys),
        vec![
            TyPattern::Primitive("Token".to_string()),
            TyPattern::Primitive("i64".to_string()),
        ],
    );
    assert_affine_rejection(&keys, "HashMap.keys()");

    let hashmap_i64_token = ResolvedTy::named_builtin(
        "HashMap",
        BuiltinType::HashMap,
        vec![ResolvedTy::I64, token_ty()],
    );
    let values = constructed_affine_collection_call(
        hashmap_i64_token,
        "hew_hashmap_values_layout",
        MethodTargetFamily::HashMap(HashMapMethod::Values),
        vec![
            TyPattern::Primitive("i64".to_string()),
            TyPattern::Primitive("Token".to_string()),
        ],
    );
    assert_affine_rejection(&values, "HashMap.values()");

    // `entries()` clones BOTH halves into the tuple, so an affine key or an
    // affine value must each be rejected on its own. Two cases, because a gate
    // wired with only one role set would still pass a single-sided probe.
    let entries_affine_key = constructed_affine_collection_call(
        ResolvedTy::named_builtin(
            "HashMap",
            BuiltinType::HashMap,
            vec![token_ty(), ResolvedTy::I64],
        ),
        "hew_hashmap_entries_layout",
        MethodTargetFamily::HashMap(HashMapMethod::Entries),
        vec![
            TyPattern::Primitive("Token".to_string()),
            TyPattern::Primitive("i64".to_string()),
        ],
    );
    assert_affine_rejection(&entries_affine_key, "HashMap.entries()");

    let entries_affine_value = constructed_affine_collection_call(
        ResolvedTy::named_builtin(
            "HashMap",
            BuiltinType::HashMap,
            vec![ResolvedTy::I64, token_ty()],
        ),
        "hew_hashmap_entries_layout",
        MethodTargetFamily::HashMap(HashMapMethod::Entries),
        vec![
            TyPattern::Primitive("i64".to_string()),
            TyPattern::Primitive("Token".to_string()),
        ],
    );
    assert_affine_rejection(&entries_affine_value, "HashMap.entries()");

    for (target_symbol, method, operation) in [
        (
            "hew_hashset_clone_layout",
            HashSetMethod::Clone,
            "HashSet.clone()",
        ),
        (
            "hew_hashset_to_vec_layout",
            HashSetMethod::ToVec,
            "HashSet.to_vec()",
        ),
    ] {
        let hashset_token =
            ResolvedTy::named_builtin("HashSet", BuiltinType::HashSet, vec![token_ty()]);
        let pipeline = constructed_affine_collection_call(
            hashset_token,
            target_symbol,
            MethodTargetFamily::HashSet(method),
            vec![TyPattern::Primitive("Token".to_string())],
        );
        assert_affine_rejection(&pipeline, operation);
    }
}

#[test]
fn legal_hash_collection_clone_out_and_remove_paths_remain_admitted() {
    pipeline(
        r#"
        fn duplicate<V>(values: HashMap<string, V>) -> HashMap<string, V> {
            values.clone()
        }

        fn main() -> i64 {
            var values: HashMap<string, i64> = HashMap.new();
            values.insert("live", 9);
            let copy = duplicate(values);
            let got = copy.get("live");
            let indexed = copy["live"];
            let keys = copy.keys();
            let vals = copy.values();
            let removed = copy.remove("live");

            var set: HashSet<i64> = HashSet.new();
            set.insert(10);
            let set_copy = set.clone();
            var total = indexed + keys.len() + vals.len() + set_copy.len();
            for value in set {
                total = total + value;
            }
            match got {
                .Some(value) => { total = total + value; }
                .None => {}
            }
            match removed {
                .Some(value) => { total = total + value; }
                .None => {}
            }
            total
        }
        "#,
    );
}

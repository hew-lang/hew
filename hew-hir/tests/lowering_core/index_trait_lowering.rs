use hew_hir::{lower_program, HirExprKind, HirItem, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:?}",
        tc_output.errors
    );
    lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    )
}

fn function_tail<'a>(output: &'a hew_hir::LowerOutput, name: &str) -> &'a hew_hir::HirExpr {
    output
        .module
        .items
        .iter()
        .find_map(|item| {
            if let HirItem::Function(func) = item {
                if func.name == name {
                    return func.body.tail.as_ref();
                }
            }
            None
        })
        .unwrap_or_else(|| panic!("function `{name}` must have a tail expression"))
}

#[test]
fn index_trait_user_impl_lowers_to_at_call() {
    let output = lower(
        r"
        type Grid {
            bias: i32,
        }

        impl Index for Grid {
            type Output = i32;

            fn get(g: Grid, index: i32) -> Option<i32> {
                .Some(g.bias + index)
            }

            fn at(g: Grid, index: i32) -> i32 {
                g.bias + index
            }
        }

        fn f() -> i32 {
            let g = Grid { bias: 40 };
            g[2]
        }
        ",
    );

    let tail = function_tail(&output, "f");
    let HirExprKind::Call { callee, args, .. } = &tail.kind else {
        panic!(
            "g[2] should lower to an Index::at call; got {:?}",
            tail.kind
        );
    };
    assert_eq!(args.len(), 2);
    assert!(
        matches!(
            &callee.kind,
            HirExprKind::BindingRef { name, .. } if name == "Grid::at"
        ),
        "callee should be Grid::at; got {:?}",
        callee.kind
    );
}

#[test]
fn dyn_index_lowers_to_vtable_call() {
    let output = lower("fn f(idx: dyn Index<Output = i32>) -> i32 { idx[2] }");
    let tail = function_tail(&output, "f");
    assert!(
        matches!(
            &tail.kind,
            HirExprKind::CallDynMethod {
                trait_name,
                method_name,
                ..
            } if trait_name == "Index" && method_name == "at"
        ),
        "dyn idx[2] should lower to a vtable call; got {:?}",
        tail.kind
    );
}

/// `[]` always dispatches `Index.at`, even when another bound of the trait
/// object declares a method named `at`.
#[test]
fn dyn_index_selects_index_at_beside_another_at() {
    let output = lower(
        r"
        trait Other { fn at(self, position: i64) -> i32; }
        fn f(idx: dyn (Index<Output = i32> + Other)) -> i32 { idx[2] }
        ",
    );
    let tail = function_tail(&output, "f");
    let HirExprKind::CallDynMethod {
        target: hew_types::CallTarget::DynamicVtable { method, slot, .. },
        ..
    } = &tail.kind
    else {
        panic!(
            "dyn idx[2] should lower to a vtable call; got {:?}",
            tail.kind
        );
    };
    // `Index` publishes `get` then `at`; `Other.at` follows them.
    assert_eq!(output.module.defs.path(*method), "std.builtins.Index::at");
    assert_eq!(*slot, 4);
}

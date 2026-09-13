//! Every catalogue runtime endpoint must belong to the shared source-declarable
//! export inventory, including contracts generated from stdlib declarations.
//! Compiler intrinsics and descriptor globals are outside this function-symbol
//! inventory. A missing endpoint indicates stale linkage or classification.

use hew_hir::stdlib_catalog::{entries, BuiltinLinkage};
use hew_types::{module_registry::ModuleRegistry, stdlib_catalog_identity, Checker, Ty};
use std::collections::HashSet;

#[test]
fn catalog_runtime_symbols_are_classified() {
    let stable = hew_types::jit_symbols::stable_symbols();
    assert!(
        !stable.is_empty(),
        "the shared source-declarable runtime inventory must not be empty"
    );

    let mut failures: Vec<String> = Vec::new();

    for entry in entries() {
        let symbol = match entry.linkage {
            BuiltinLinkage::RuntimeFfiShim { symbol }
            | BuiltinLinkage::ToStringShim { symbol }
            | BuiltinLinkage::StringCloneShim { symbol } => symbol,
            BuiltinLinkage::PrintIntercept { runtime_symbol, .. } => runtime_symbol,
            // CompilerIntrinsic entries do not name a C-ABI symbol; they map to
            // LLVM backend ops. CalleeNameDispatchOnly entries are
            // intercepted in codegen by callee name and never declare an LLVM
            // extern of their own. Always considered classified.
            // CompilerIntrinsic / CalleeNameDispatchOnly entries do not name a
            // C-ABI symbol exposed at the generated-code host boundary; LayoutDescriptorSymbol
            // entries name `#[no_mangle] pub static` descriptors in
            // `hew-runtime/src/layout_intrinsics.rs`, not extern "C" fns, and the
            // runtime export classification gate only enumerates fn exports (see
            // `scripts/verify-ffi-symbols.py:4`). All three are out of scope here.
            BuiltinLinkage::CompilerIntrinsic { .. }
            | BuiltinLinkage::CalleeNameDispatchOnly
            | BuiltinLinkage::LayoutDescriptorSymbol { .. } => {
                continue;
            }
            // NodeRegisterByPid declares two C-ABI symbols; check both.
            BuiltinLinkage::NodeRegisterByPid {
                register_symbol,
                pid_accessor,
            } => {
                for sym in [register_symbol, pid_accessor] {
                    if !stable.contains(sym) {
                        failures.push(format!(
                            "catalog row `{}` (linkage symbol `{sym}`) is not in the \
                             shared source-declarable runtime inventory",
                            entry.name,
                        ));
                    }
                }
                continue;
            }
        };

        // A shim symbol that names a typed runtime-call family (`vec.value.len`)
        // is lowered by codegen from the family descriptor, not linked against a
        // runtime export, so it carries no row in the export classification
        // table. A family key is spelled with a `.`, which no C identifier can
        // contain — the same structural test `builtin_function_names` uses.
        if symbol.contains('.')
            && hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(symbol).is_some()
        {
            continue;
        }

        if !stable.contains(symbol) {
            failures.push(format!(
                "catalog row `{}` (linkage symbol `{}`) is not in the \
                 shared source-declarable runtime inventory",
                entry.name, symbol
            ));
        }
    }

    if !failures.is_empty() {
        let list = failures.join("\n  ");
        panic!(
            "{} catalog row(s) name a runtime symbol absent from the classification table:\n  {}\n\n\
             To fix: classify the missing symbol in its owning declaration or export list, \
             or correct the catalog linkage.",
            failures.len(),
            list
        );
    }
}

#[test]
fn catalog_contains_bytes_constructor_and_method_targets() {
    let mut names: HashSet<&str> = HashSet::new();
    for entry in entries() {
        names.insert(entry.name);
        if let Some(symbol) = entry.linkage.runtime_symbol() {
            names.insert(symbol);
        }
    }
    for name in [
        "bytes::new",
        "hew_bytes_push",
        "hew_bytes_pop",
        "hew_bytes_get",
        "hew_bytes_set",
        "hew_bytes_is_empty",
        "hew_bytes_clear",
        "hew_bytes_contains",
        "hew_bytes_append",
    ] {
        assert!(names.contains(name), "missing bytes catalog row `{name}`");
    }
}

/// Keep the checker-side `CallTarget::Builtin` identity projection total for
/// every catalog entry that it can accept as a concrete ordinary call.  The
/// companion checker unit test invokes `call_target_for_signature` for every
/// identity below and proves it publishes `CallTarget::Builtin` rather than
/// `Unsupported`; this test owns the opposite direction because only HIR owns
/// the complete catalog/linkage inventory.
///
/// Generic surfaces and compiler intrinsics are deliberately out of scope:
/// they have dedicated type-driven or method-rewrite lowering paths, so an
/// ordinary monomorphic `CallTarget::Builtin` would be the wrong executable
/// carrier for them.
#[test]
fn checker_accepted_monomorphic_catalog_callables_have_target_identities() {
    let parsed = hew_parser::parse("");
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&parsed.program);
    assert!(
        output.errors.is_empty(),
        "empty builtin inventory program must typecheck: {:#?}",
        output.errors
    );

    let expected: HashSet<&str> = entries()
        .iter()
        .filter(|entry| {
            !matches!(
                entry.linkage,
                BuiltinLinkage::CompilerIntrinsic { .. }
                    | BuiltinLinkage::LayoutDescriptorSymbol { .. }
            )
        })
        .filter_map(|entry| {
            let signature = output.fn_sigs.get(entry.name)?;
            let concrete = signature.type_params.is_empty()
                && !signature.params.iter().any(Ty::has_inference_var)
                && !signature.return_type.has_inference_var();
            concrete.then_some(entry.name)
        })
        .collect();
    let published: HashSet<&str> = stdlib_catalog_identity::MONOMORPHIC_CALLABLE_IDENTITIES
        .iter()
        .copied()
        .collect();

    assert_eq!(
        published, expected,
        "every concrete catalog callable accepted by the checker must have one exact \
         checker CallTarget identity, and no generic/non-executable surface may claim one"
    );
}

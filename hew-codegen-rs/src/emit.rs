use hew_mir::model::{HewMlirFunction, HewMlirModule, HewMlirOp, Strategy};
use hew_types::ResolvedTy;

/// Emit a D2 textual / D7-style pre-MLIR proof of `module`.
///
/// This Stage 9 format is intentionally a Hew-IR trace, not a promise that the
/// text can round-trip through MLIR tooling before the Stage 4 TypeDescriptor /
/// THIR authority and dialect registration work is complete.
///
/// Type names follow the current D7-style canonical spelling convention:
/// - `!hew.string` for Hew strings
/// - `!hew.bytes` / `!hew.char` / `!hew.duration` for other Hew scalars
/// - Standard MLIR integer/float types (`i64`, `f32`, `i1`, …) for primitives
/// - `!hew.unit` for the unit type
/// - `!hew.never` for the diverging type
///
/// Value decision attributes use a string attribute-like form
/// `"StrategyName"` which is sufficient for the D2 textual proof.
#[must_use]
pub fn emit_mlir(module: &HewMlirModule) -> String {
    let mut out = String::new();
    for func in &module.functions {
        out.push_str(&emit_function(func));
    }
    out
}

fn emit_function(func: &HewMlirFunction) -> String {
    let mut out = format!("hew.func @{} {{\n", func.name);
    for op in &func.ops {
        out.push_str("  ");
        out.push_str(&emit_op(op));
        out.push('\n');
    }
    out.push_str("}\n");
    out
}

fn emit_op(op: &HewMlirOp) -> String {
    match op {
        HewMlirOp::Bind {
            name,
            ty,
            site,
            decision,
        } => format!(
            "hew.bind @{name} : {ty} {{hew.site_id = \"{site}\", hew.value_decision = \"{decision}\"}}",
            ty = emit_ty(ty),
            decision = emit_strategy(decision),
        ),
        HewMlirOp::Read { ty, site, decision } => format!(
            "hew.read : {ty} {{hew.site_id = \"{site}\", hew.value_decision = \"{decision}\"}}",
            ty = emit_ty(ty),
            decision = emit_strategy(decision),
        ),
        HewMlirOp::Return {
            ty,
            site: Some(site),
            decision: Some(decision),
        } => format!(
            "hew.return : {ty} {{hew.site_id = \"{site}\", hew.value_decision = \"{decision}\"}}",
            ty = emit_ty(ty),
            decision = emit_strategy(decision),
        ),
        HewMlirOp::Return {
            ty,
            site: None,
            decision: None,
        } => format!("hew.return : {}", emit_ty(ty)),
        HewMlirOp::Return { .. } => unreachable!("return MLIR op has mismatched site/decision"),
        HewMlirOp::Drop { name, ty } => {
            format!("hew.drop @{name} : {}", emit_ty(ty))
        }
    }
}

/// Map a [`ResolvedTy`] to a D7-style pre-MLIR type string.
///
/// Scalars use MLIR built-in integer/float types.  Hew-specific types use the
/// `!hew.*` dialect-style namespace so the canonical spelling is explicit.
fn emit_ty(ty: &ResolvedTy) -> String {
    match ty {
        ResolvedTy::I8 => "i8".to_string(),
        ResolvedTy::I16 => "i16".to_string(),
        ResolvedTy::I32 => "i32".to_string(),
        ResolvedTy::I64 => "i64".to_string(),
        ResolvedTy::U8 => "ui8".to_string(),
        ResolvedTy::U16 => "ui16".to_string(),
        ResolvedTy::U32 => "ui32".to_string(),
        ResolvedTy::U64 => "ui64".to_string(),
        ResolvedTy::F32 => "f32".to_string(),
        ResolvedTy::F64 => "f64".to_string(),
        ResolvedTy::Bool => "i1".to_string(),
        // Hew-specific types use the !hew.* namespace for D7-style canonical spelling.
        ResolvedTy::Char => "!hew.char".to_string(),
        ResolvedTy::String => "!hew.string".to_string(),
        ResolvedTy::Bytes => "!hew.bytes".to_string(),
        ResolvedTy::Duration => "!hew.duration".to_string(),
        ResolvedTy::Unit => "!hew.unit".to_string(),
        ResolvedTy::Never => "!hew.never".to_string(),
        ResolvedTy::Tuple(elems) => {
            let inner = elems.iter().map(emit_ty).collect::<Vec<_>>().join(", ");
            format!("!hew.tuple<{inner}>")
        }
        ResolvedTy::Array(elem, len) => {
            format!("!hew.array<{} x {}>", emit_ty(elem), len)
        }
        ResolvedTy::Slice(elem) => format!("!hew.slice<{}>", emit_ty(elem)),
        ResolvedTy::Named { name, .. } => {
            unreachable!(
                "D10 violation: Named/user type `{name}` reached Stage 9 textual emission; {}",
                "reject or lower user types before Stage 9 until Stage 4 TypeDescriptor/THIR authority is complete"
            )
        }
        ResolvedTy::Function { params, ret } => {
            let params_str = params.iter().map(emit_ty).collect::<Vec<_>>().join(", ");
            format!("!hew.fn<({params_str}) -> {}>", emit_ty(ret))
        }
        ResolvedTy::Closure {
            params,
            ret,
            captures,
        } => {
            let params_str = params.iter().map(emit_ty).collect::<Vec<_>>().join(", ");
            let cap_str = captures.iter().map(emit_ty).collect::<Vec<_>>().join(", ");
            format!(
                "!hew.closure<({params_str}) -> {} captures [{cap_str}]>",
                emit_ty(ret)
            )
        }
        ResolvedTy::Pointer {
            is_mutable,
            pointee,
        } => {
            let mutability = if *is_mutable { "mut" } else { "const" };
            format!("!hew.ptr<{mutability}, {}>", emit_ty(pointee))
        }
        ResolvedTy::TraitObject { traits } => {
            let bounds = traits
                .iter()
                .map(emit_trait_bound)
                .collect::<Vec<_>>()
                .join(" + ");
            format!("!hew.dyn<{bounds}>")
        }
    }
}

fn emit_trait_bound(bound: &hew_types::ResolvedTraitBound) -> String {
    if bound.args.is_empty() {
        bound.trait_name.clone()
    } else {
        let args = bound
            .args
            .iter()
            .map(emit_ty)
            .collect::<Vec<_>>()
            .join(", ");
        format!("{}<{args}>", bound.trait_name)
    }
}

/// Format a [`Strategy`] as a short camelCase string for the
/// `hew.value_decision` attribute.
fn emit_strategy(s: &Strategy) -> &'static str {
    match s {
        Strategy::BorrowRead => "BorrowRead",
        Strategy::Move => "Move",
        Strategy::CowShare => "CowShare",
        Strategy::EnsureUnique => "EnsureUnique",
        Strategy::Materialize => "Materialize",
        Strategy::ConsumeCall => "ConsumeCall",
        Strategy::Freeze => "Freeze",
        // UnknownBlocked must never reach this point per D10.  If it does, the
        // assert_ne in lower_elaborated_to_mlir should have already panicked.
        Strategy::UnknownBlocked => {
            unreachable!("D10 violation: UnknownBlocked strategy reached Stage 9 textual emission")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_mir::model::{HewMlirFunction, HewMlirModule, HewMlirOp};
    use hew_types::{ResolvedTraitBound, ResolvedTy};

    fn module_with_ops(name: &str, ops: Vec<HewMlirOp>) -> HewMlirModule {
        HewMlirModule {
            functions: vec![HewMlirFunction {
                name: name.to_string(),
                ops,
            }],
        }
    }

    #[test]
    fn emit_ty_maps_string_to_hew_string() {
        assert_eq!(emit_ty(&ResolvedTy::String), "!hew.string");
    }

    #[test]
    fn emit_ty_maps_scalars_to_mlir_builtins() {
        assert_eq!(emit_ty(&ResolvedTy::I64), "i64");
        assert_eq!(emit_ty(&ResolvedTy::I32), "i32");
        assert_eq!(emit_ty(&ResolvedTy::Bool), "i1");
        assert_eq!(emit_ty(&ResolvedTy::F32), "f32");
        assert_eq!(emit_ty(&ResolvedTy::F64), "f64");
    }

    #[test]
    fn emit_ty_maps_hew_specific_types() {
        assert_eq!(emit_ty(&ResolvedTy::Char), "!hew.char");
        assert_eq!(emit_ty(&ResolvedTy::Bytes), "!hew.bytes");
        assert_eq!(emit_ty(&ResolvedTy::Duration), "!hew.duration");
        assert_eq!(emit_ty(&ResolvedTy::Unit), "!hew.unit");
        assert_eq!(emit_ty(&ResolvedTy::Never), "!hew.never");
    }

    #[test]
    fn emit_ty_formats_composite_types() {
        let tuple = ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::String]);
        assert_eq!(emit_ty(&tuple), "!hew.tuple<i64, !hew.string>");

        let arr = ResolvedTy::Array(Box::new(ResolvedTy::I32), 4);
        assert_eq!(emit_ty(&arr), "!hew.array<i32 x 4>");

        let slice = ResolvedTy::Slice(Box::new(ResolvedTy::U8));
        assert_eq!(emit_ty(&slice), "!hew.slice<ui8>");
    }

    #[test]
    #[should_panic(
        expected = "D10 violation: Named/user type `Foo` reached Stage 9 textual emission"
    )]
    fn named_type_panics_in_textual_emitter() {
        let module = module_with_ops(
            "main",
            vec![HewMlirOp::Return {
                ty: ResolvedTy::Named {
                    name: "Foo".to_string(),
                    args: vec![ResolvedTy::I64],
                },
                site: None,
                decision: None,
            }],
        );
        let _ = emit_mlir(&module);
    }

    #[test]
    fn trait_object_args_survive_textual_emission() {
        let dyn_iterator = ResolvedTy::TraitObject {
            traits: vec![ResolvedTraitBound {
                trait_name: "Iterator".to_string(),
                args: vec![ResolvedTy::I64],
            }],
        };
        assert_eq!(emit_ty(&dyn_iterator), "!hew.dyn<Iterator<i64>>");
    }

    #[test]
    fn string_return_uses_hew_string_type() {
        let module = module_with_ops(
            "main",
            vec![HewMlirOp::Return {
                ty: ResolvedTy::String,
                site: None,
                decision: None,
            }],
        );
        let out = emit_mlir(&module);
        assert!(
            out.contains("hew.return : !hew.string"),
            "D7: String must map to !hew.string, got: {out}"
        );
        assert!(
            !out.contains("hew.return : String"),
            "D7: bare 'String' type must not appear in D7-compliant output"
        );
    }

    #[test]
    fn i64_return_uses_mlir_integer_type() {
        let module = module_with_ops(
            "add",
            vec![HewMlirOp::Return {
                ty: ResolvedTy::I64,
                site: None,
                decision: None,
            }],
        );
        let out = emit_mlir(&module);
        assert!(
            out.contains("hew.return : i64"),
            "D7: I64 must map to i64 (not 'int'), got: {out}"
        );
    }
}

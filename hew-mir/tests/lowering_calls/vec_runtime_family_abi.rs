//! Source-to-MIR regression for the concrete scalar Vec ABI matrix.
//!
//! This deliberately observes only typed `RuntimeCallFamily` and argument
//! arity.  It rejects a lowering that selects a neighbouring scalar entry,
//! changes a call's ABI shape, or loses runtime authority, while allowing
//! harmless block, local, ownership, and diagnostic presentation changes.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{
    lower_hir_module,
    model::{CallAuthority, ClosurePairVecKind, CompilerCallKind, IrPipeline, Terminator},
};
use hew_types::{
    module_registry::ModuleRegistry,
    runtime_call::{
        RuntimeCallFamily as Family, VecContainsScalarElem as Contains, VecScalarElem as Elem,
        VecScalarOp as Op,
    },
    Checker,
};

const SOURCE: &str = r#"
fn identity(x: i64) -> i64 {
    x
}

fn shared_families() {
    let left: Vec<i64> = Vec.new();
    let right: Vec<i64> = Vec.new();
    left.append(right);
    let copy = left.clone();
    let _ = copy.is_empty();
    copy.clear();
    let words: Vec<string> = Vec.new();
    words.push("a");
    let _joined = words.join(",");
}

fn i8_families() {
    let values: Vec<i8> = Vec.new();
    values.push(1 as i8);
    values.set(0, 2 as i8);
    let _ = values.pop();
    values.push(3 as i8);
    let _ = values.remove(0);
}

fn u8_families() {
    let values: Vec<u8> = Vec.new();
    values.push(1 as u8);
    values.set(0, 2 as u8);
    let _ = values.pop();
    values.push(3 as u8);
    let _ = values.remove(0);
}

fn i16_families() {
    let values: Vec<i16> = Vec.new();
    values.push(1 as i16);
    values.set(0, 2 as i16);
    let _ = values.pop();
    values.push(3 as i16);
    let _ = values.remove(0);
}

fn u16_families() {
    let values: Vec<u16> = Vec.new();
    values.push(1 as u16);
    values.set(0, 2 as u16);
    let _ = values.pop();
    values.push(3 as u16);
    let _ = values.remove(0);
}

fn i32_families() {
    let values: Vec<i32> = Vec.new();
    values.push(1 as i32);
    let _ = values.contains(1 as i32);
    let _ = values.pop();
    values.push(3 as i32);
    let _ = values.remove(0);
}

fn i64_families() {
    let values: Vec<i64> = Vec.new();
    values.push(1);
    values.set(0, 2);
    let _ = values.contains(2);
    let _ = values.pop();
    values.push(3);
    let _ = values.remove(0);
}

fn f32_families() {
    let values: Vec<f32> = Vec.new();
    values.push(1.0 as f32);
    values.set(0, 2.0 as f32);
    let _ = values.pop();
    values.push(3.0 as f32);
    let _ = values.remove(0);
}

fn f64_families() {
    let values: Vec<f64> = Vec.new();
    values.push(1.0);
    values.set(0, 2.0);
    let _ = values.contains(2.0);
    let _ = values.pop();
    values.push(3.0);
    let _ = values.remove(0);
}

fn string_families() {
    let values: Vec<string> = Vec.new();
    values.push("a");
    values.set(0, "b");
    let _ = values.contains("b");
    let _ = values.pop();
    values.push("c");
    let _ = values.remove(0);
}

fn pointer_families() {
    let values: Vec<fn(i64) -> i64> = Vec.new();
    values.push(identity);
    values.set(0, identity);
    let _ = values.pop();
    values.push(identity);
    let _ = values.remove(0);
}
"#;

#[derive(Debug, PartialEq, Eq)]
struct RuntimeCall {
    family: Family,
    arity: usize,
}

#[derive(Debug, PartialEq, Eq)]
struct CompilerCall {
    kind: ClosurePairVecKind,
    arity: usize,
}

const fn call(family: Family, arity: usize) -> RuntimeCall {
    RuntimeCall { family, arity }
}

const fn scalar(op: Op, elem: Elem, arity: usize) -> RuntimeCall {
    call(Family::VecScalar { op, elem }, arity)
}

fn pipeline() -> IrPipeline {
    let parsed = hew_parser::parse(SOURCE);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let typechecked = checker.check_program(&parsed.program);
    assert!(
        typechecked.errors.is_empty(),
        "type errors: {:#?}",
        typechecked.errors
    );

    let lowered = lower_program(
        &parsed.program,
        &typechecked,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        lowered.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        lowered.diagnostics
    );

    let pipeline = lower_hir_module(&lowered.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    pipeline
}

fn runtime_calls(pipeline: &IrPipeline, name: &str) -> Vec<RuntimeCall> {
    let function = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == name)
        .unwrap_or_else(|| panic!("lowered function `{name}` is missing"));

    function
        .blocks
        .iter()
        .filter_map(|block| match &block.terminator {
            Terminator::Call {
                callee,
                authority: CallAuthority::Runtime(family),
                args,
                ..
            } => {
                assert_eq!(
                    callee,
                    family.c_symbol(),
                    "runtime call identity must come from its typed family"
                );
                Some(call(*family, args.len()))
            }
            _ => None,
        })
        .collect()
}

fn closure_pair_vec_calls(pipeline: &IrPipeline, name: &str) -> Vec<CompilerCall> {
    let function = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == name)
        .unwrap_or_else(|| panic!("lowered function `{name}` is missing"));

    function
        .blocks
        .iter()
        .filter_map(|block| match &block.terminator {
            Terminator::Call {
                callee,
                authority: CallAuthority::Compiler(CompilerCallKind::ClosurePairVec(kind)),
                args,
                ..
            } => {
                assert_eq!(
                    callee,
                    kind.expected_callee(),
                    "closure-pair Vec call identity must come from its typed family"
                );
                Some(CompilerCall {
                    kind: *kind,
                    arity: args.len(),
                })
            }
            _ => None,
        })
        .collect()
}

fn shared_and_string_cases() -> Vec<(&'static str, Vec<RuntimeCall>)> {
    vec![
        (
            "shared_families",
            vec![
                call(Family::VecNew, 0),
                call(Family::VecNew, 0),
                call(Family::VecAppend, 2),
                call(Family::VecClone, 1),
                call(Family::VecIsEmpty, 1),
                call(Family::VecClear, 1),
                call(Family::VecNew, 0),
                scalar(Op::Push, Elem::Str, 2),
                call(Family::VecJoinStr, 2),
            ],
        ),
        (
            "string_families",
            vec![
                call(Family::VecNew, 0),
                scalar(Op::Push, Elem::Str, 2),
                scalar(Op::Set, Elem::Str, 3),
                call(Family::VecContainsScalar(Contains::Str), 2),
                scalar(Op::Pop, Elem::Str, 1),
                scalar(Op::Push, Elem::Str, 2),
                scalar(Op::RemoveAt, Elem::Str, 2),
            ],
        ),
        ("pointer_families", vec![call(Family::VecNew, 0)]),
    ]
}

fn signed_integer_cases() -> Vec<(&'static str, Vec<RuntimeCall>)> {
    vec![
        (
            "i8_families",
            vec![
                call(Family::VecNew, 0),
                scalar(Op::Push, Elem::I8, 2),
                scalar(Op::Set, Elem::I8, 3),
                scalar(Op::Pop, Elem::I8, 1),
                scalar(Op::Push, Elem::I8, 2),
                scalar(Op::RemoveAt, Elem::I8, 2),
            ],
        ),
        (
            "i16_families",
            vec![
                call(Family::VecNew, 0),
                scalar(Op::Push, Elem::I16, 2),
                scalar(Op::Set, Elem::I16, 3),
                scalar(Op::Pop, Elem::I16, 1),
                scalar(Op::Push, Elem::I16, 2),
                scalar(Op::RemoveAt, Elem::I16, 2),
            ],
        ),
        (
            "i32_families",
            vec![
                call(Family::VecNew, 0),
                scalar(Op::Push, Elem::I32, 2),
                call(Family::VecContainsScalar(Contains::I32), 2),
                scalar(Op::Pop, Elem::I32, 1),
                scalar(Op::Push, Elem::I32, 2),
                scalar(Op::RemoveAt, Elem::I32, 2),
            ],
        ),
        (
            "i64_families",
            vec![
                call(Family::VecNew, 0),
                scalar(Op::Push, Elem::I64, 2),
                scalar(Op::Set, Elem::I64, 3),
                call(Family::VecContainsScalar(Contains::I64), 2),
                scalar(Op::Pop, Elem::I64, 1),
                scalar(Op::Push, Elem::I64, 2),
                scalar(Op::RemoveAt, Elem::I64, 2),
            ],
        ),
    ]
}

fn unsigned_and_float_cases() -> Vec<(&'static str, Vec<RuntimeCall>)> {
    vec![
        (
            "u8_families",
            vec![
                call(Family::VecNew, 0),
                scalar(Op::Push, Elem::U8, 2),
                scalar(Op::Set, Elem::U8, 3),
                scalar(Op::Pop, Elem::U8, 1),
                scalar(Op::Push, Elem::U8, 2),
                scalar(Op::RemoveAt, Elem::U8, 2),
            ],
        ),
        (
            "u16_families",
            vec![
                call(Family::VecNew, 0),
                scalar(Op::Push, Elem::U16, 2),
                scalar(Op::Set, Elem::U16, 3),
                scalar(Op::Pop, Elem::U16, 1),
                scalar(Op::Push, Elem::U16, 2),
                scalar(Op::RemoveAt, Elem::U16, 2),
            ],
        ),
        (
            "f32_families",
            vec![
                call(Family::VecNew, 0),
                scalar(Op::Push, Elem::F32, 2),
                scalar(Op::Set, Elem::F32, 3),
                scalar(Op::Pop, Elem::F32, 1),
                scalar(Op::Push, Elem::F32, 2),
                scalar(Op::RemoveAt, Elem::F32, 2),
            ],
        ),
        (
            "f64_families",
            vec![
                call(Family::VecNew, 0),
                scalar(Op::Push, Elem::F64, 2),
                scalar(Op::Set, Elem::F64, 3),
                call(Family::VecContainsScalar(Contains::F64), 2),
                scalar(Op::Pop, Elem::F64, 1),
                scalar(Op::Push, Elem::F64, 2),
                scalar(Op::RemoveAt, Elem::F64, 2),
            ],
        ),
    ]
}

#[test]
fn concrete_vec_methods_select_the_typed_runtime_family_and_abi_arity() {
    let pipeline = pipeline();
    let cases = [
        shared_and_string_cases(),
        signed_integer_cases(),
        unsigned_and_float_cases(),
    ]
    .into_iter()
    .flatten();

    for (name, expected) in cases {
        assert_eq!(runtime_calls(&pipeline, name), expected, "{name}");
    }

    assert_eq!(
        closure_pair_vec_calls(&pipeline, "pointer_families"),
        vec![
            CompilerCall {
                kind: ClosurePairVecKind::Push,
                arity: 2,
            },
            CompilerCall {
                kind: ClosurePairVecKind::Set,
                arity: 3,
            },
            CompilerCall {
                kind: ClosurePairVecKind::Pop,
                arity: 1,
            },
            CompilerCall {
                kind: ClosurePairVecKind::Push,
                arity: 2,
            },
            CompilerCall {
                kind: ClosurePairVecKind::RemoveAt,
                arity: 2,
            },
        ],
        "pointer elements must retain compiler-owned closure-pair Vec ABI selection"
    );
}

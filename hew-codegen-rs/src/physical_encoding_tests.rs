//! Checked encoding calls use the actual runtime and exact native C carriers.

use super::*;
use hew_types::runtime_call::{EncodingFormat, EncodingOp};
use hew_types::RuntimeCallFamily;

#[path = "../../hew-mir/src/physical_encoding_fixture.rs"]
mod fixture;

fn physical_fixture(semantic: &hew_sir::SemModule, triple: &str) -> PhysicalModule {
    let target = physical_target_for_inventory(
        triple,
        &hew_mir::physical::physical_type_inventory(semantic),
    )
    .unwrap();
    hew_mir::lower_physical_module(semantic, target)
        .unwrap()
        .into_unverified()
}

#[test]
fn encoding_c_declarations_preserve_scalar_widths_and_void_mutation_across_targets() {
    for triple in [
        "x86_64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc",
        "aarch64-apple-darwin",
    ] {
        let ctx = Context::create();
        let ptr = ctx.ptr_type(AddressSpace::default()).into();
        let i32_ty = ctx.i32_type().into();
        let i64_ty = ctx.i64_type().into();
        let f64_ty = ctx.f64_type().into();
        for family in hew_types::runtime_call::all_runtime_call_families()
            .into_iter()
            .filter(|family| family.encoding_format().is_some())
        {
            let physical = physical_fixture(&fixture::operation(family), triple);
            let llvm = llvm(&ctx, &physical);
            let function = llvm.get_function(family.c_symbol()).unwrap();
            use EncodingOp as Op;
            let (params, result): (Vec<BasicMetadataTypeEnum<'_>>, Option<BasicTypeEnum<'_>>) =
                match family {
                    RuntimeCallFamily::JsonObjectKeys => (vec![ptr], Some(ptr.try_into().unwrap())),
                    RuntimeCallFamily::Encoding { op, .. } => match op {
                        Op::LastError | Op::ObjectNew | Op::ArrayNew | Op::FromNull => {
                            (vec![], Some(ptr.try_into().unwrap()))
                        }
                        Op::Parse | Op::Stringify | Op::FromString | Op::GetString | Op::Clone => {
                            (vec![ptr], Some(ptr.try_into().unwrap()))
                        }
                        Op::Type | Op::IntStatus | Op::GetBool | Op::ArrayLen => {
                            (vec![ptr], Some(ctx.i32_type().into()))
                        }
                        Op::GetInt | Op::GetU64 => (vec![ptr], Some(ctx.i64_type().into())),
                        Op::GetFloat => (vec![ptr], Some(ctx.f64_type().into())),
                        Op::FromBool => (vec![i32_ty], Some(ptr.try_into().unwrap())),
                        Op::FromInt | Op::FromU64 => (vec![i64_ty], Some(ptr.try_into().unwrap())),
                        Op::FromFloat => (vec![f64_ty], Some(ptr.try_into().unwrap())),
                        Op::Eq => (vec![ptr, ptr], Some(ctx.i32_type().into())),
                        Op::GetField => (vec![ptr, ptr], Some(ptr.try_into().unwrap())),
                        Op::ArrayGet => (vec![ptr, i32_ty], Some(ptr.try_into().unwrap())),
                        Op::ObjectSet => (vec![ptr, ptr, ptr], None),
                        Op::ArrayPush => (vec![ptr, ptr], None),
                        Op::Free => (vec![ptr], None),
                    },
                    _ => unreachable!(),
                };
            assert_eq!(
                function.get_type().get_param_types(),
                params,
                "{family:?}, {triple}"
            );
            assert_eq!(
                function.get_type().get_return_type(),
                result,
                "{family:?}, {triple}"
            );
        }
        for format in [EncodingFormat::Json, EncodingFormat::Yaml] {
            let physical = physical_fixture(&fixture::copy(format), triple);
            let llvm = llvm(&ctx, &physical);
            assert_eq!(
                llvm.get_function(EncodingOp::Clone.c_symbol(format))
                    .unwrap()
                    .get_type(),
                ctx.ptr_type(AddressSpace::default()).fn_type(&[ptr], false)
            );
        }
    }
}

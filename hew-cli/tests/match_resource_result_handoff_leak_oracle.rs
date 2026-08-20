//! Exact-value ownership oracle for resources selected out of call-result matches.
//!
//! Each resource representation runs the four active-payload shapes: consumed
//! and unconsumed `Ok`, and consumed and unconsumed `Err`. Exact zero-leak
//! measurements pin sibling payload cleanup; allocator scribbling pins the
//! opposite failure mode, where both the arm-local owner and carrier release
//! the same payload.

#![cfg(unix)]

mod support;

use std::path::Path;

use support::leak_slope::{
    compile_to_native, measure_leaks_exact, require_leaks_tool, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

const FRAMES: usize = 12;

#[derive(Clone, Copy)]
enum ResourceShape {
    Opaque,
    FieldBearingRecord,
}

impl ResourceShape {
    fn name(self) -> &'static str {
        match self {
            Self::Opaque => "opaque",
            Self::FieldBearingRecord => "record",
        }
    }

    fn declarations(self) -> &'static str {
        match self {
            Self::Opaque => {
                "#[resource]\n\
                 #[opaque]\n\
                 type Handle {}\n\
                 impl Handle {\n\
                 \x20   fn probe(self) -> i64 { 7 }\n\
                 \x20   fn close(self) { unsafe { hew_deque_free(self) }; }\n\
                 }\n\
                 extern \"C\" {\n\
                 \x20   fn hew_deque_new() -> Handle;\n\
                 \x20   fn hew_deque_free(consume handle: Handle);\n\
                 }\n\
                 fn fresh() -> Handle { unsafe { hew_deque_new() } }\n"
            }
            Self::FieldBearingRecord => {
                "#[opaque]\n\
                 type Raw {}\n\
                 #[resource]\n\
                 type Handle { raw: Raw; tag: i64; }\n\
                 impl Handle {\n\
                 \x20   fn probe(self) -> i64 { self.tag }\n\
                 \x20   fn close(self) { unsafe { hew_deque_free(self.raw) }; }\n\
                 }\n\
                 extern \"C\" {\n\
                 \x20   fn hew_deque_new() -> Raw;\n\
                 \x20   fn hew_deque_free(consume handle: Raw);\n\
                 }\n\
                 fn fresh() -> Handle { Handle { raw: unsafe { hew_deque_new() }, tag: 7 } }\n"
            }
        }
    }
}

#[derive(Clone, Copy)]
enum ActivePayload {
    OkConsumed,
    OkUnconsumed,
    ErrConsumed,
    ErrUnconsumed,
}

impl ActivePayload {
    fn name(self) -> &'static str {
        match self {
            Self::OkConsumed => "ok_consumed",
            Self::OkUnconsumed => "ok_unconsumed",
            Self::ErrConsumed => "err_consumed",
            Self::ErrUnconsumed => "err_unconsumed",
        }
    }

    fn body(self) -> &'static str {
        match self {
            Self::OkConsumed => {
                "let handle = match make(true) {\n\
                 \x20   Ok(value) => value,\n\
                 \x20   Err(_) => return 71,\n\
                 };\n\
                 handle.close();"
            }
            Self::OkUnconsumed => {
                "match make(true) {\n\
                 \x20   Ok(value) => { if value.probe() != 7 { return 76; } },\n\
                 \x20   Err(_) => return 72,\n\
                 };"
            }
            Self::ErrConsumed => {
                "let message = match make(false) {\n\
                 \x20   Ok(value) => { value.close(); return 73; },\n\
                 \x20   Err(value) => value,\n\
                 };\n\
                 if message.len() == 0 { return 74; }"
            }
            Self::ErrUnconsumed => {
                "match make(false) {\n\
                 \x20   Ok(value) => { value.close(); return 75; },\n\
                 \x20   Err(_) => {},\n\
                 };"
            }
        }
    }
}

fn source(shape: ResourceShape, active: ActivePayload) -> String {
    format!(
        "{}\n\
         fn make(ok: bool) -> Result<Handle, string> {{\n\
         \x20   if ok {{ Ok(fresh()) }} else {{ Err(\"handoff-error\".to_upper()) }}\n\
         }}\n\
         fn run_once() -> i64 {{\n\
         \x20   {}\n\
         \x20   0\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   for _ in 0..{FRAMES} {{\n\
         \x20       let status = run_once();\n\
         \x20       if status != 0 {{ return status; }}\n\
         \x20   }}\n\
         \x20   0\n\
         }}\n",
        shape.declarations(),
        active.body()
    )
}

fn assert_exact_zero_leaks(bin: &Path, case: &str) {
    require_leaks_tool();
    assert_eq!(
        measure_leaks_exact(bin),
        (0, 0),
        "{case} must report exactly zero leaked allocations and bytes"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "exact leak oracle and deterministic poisoned allocator require macOS"
)]
#[test]
fn resource_result_match_handoffs_drop_every_payload_exactly_once() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("match-resource-result-handoff-")
        .tempdir()
        .expect("tempdir");

    for shape in [ResourceShape::Opaque, ResourceShape::FieldBearingRecord] {
        for active in [
            ActivePayload::OkConsumed,
            ActivePayload::OkUnconsumed,
            ActivePayload::ErrConsumed,
            ActivePayload::ErrUnconsumed,
        ] {
            let case = format!("{}_{}", shape.name(), active.name());
            let bin = compile_to_native(&source(shape, active), dir.path(), &case);
            let output = run_under_malloc_scribble(&bin);
            assert!(
                output.status.success(),
                "{case} must not double-free or read freed storage under allocator scribbling:\n{}",
                describe_output(&output)
            );
            assert_exact_zero_leaks(&bin, &case);
        }
    }
}

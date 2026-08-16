//! Native ownership oracle for fluent `VecIter::filter().collect()`.

#![cfg(unix)]

mod support;

use std::process::Command;
use std::time::Duration;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, require_leaks_tool,
    run_under_malloc_scribble, try_measure_leaks_command,
};
use support::{describe_output, require_codegen};

const INSPECTION_PREFLIGHT_TIMEOUT: Duration = Duration::from_secs(5);

fn host_inspection_is_unavailable(error: &str) -> bool {
    error.contains("Couldn't get task port for pid")
        || error.contains("error acquiring target task port from parent")
        || error.contains("is not debuggable. Due to security restrictions")
        || (error.contains("MallocStackLogging: could not tag MSL-related memory as no_footprint")
            && error.contains("No such file or directory (2)"))
}

fn host_can_inspect_process() -> bool {
    require_leaks_tool();
    let mut command = Command::new("leaks");
    command.args(["--atExit", "--", "/usr/bin/true"]);

    match try_measure_leaks_command(
        command,
        "host-capability probe",
        INSPECTION_PREFLIGHT_TIMEOUT,
    ) {
        Ok(_) => true,
        Err(error) if host_inspection_is_unavailable(&error) => {
            eprintln!(
                "SKIP: owned record filter/collect leak-slope oracle: leaks(1) cannot create \
                 a usable inspection session on this macOS host:\n{error}"
            );
            false
        }
        Err(error) => panic!(
            "leaks(1) host-capability preflight failed without a recognized inspection \
             capability denial; refusing to skip the leak measurement:\n{error}"
        ),
    }
}

fn owned_record_filter_collect_source(frames: usize) -> String {
    format!(
        "import std.iter;\n\
         \n\
         record Claim {{\n\
         \x20   run_id: string,\n\
         \x20   amount: i64,\n\
         }}\n\
         \n\
         fn retained_total(frame: i64) -> i64 {{\n\
         \x20   let claims: Vec<Claim> = Vec::new();\n\
         \x20   claims.push(Claim {{ run_id: \"discard\", amount: -1 }});\n\
         \x20   claims.push(Claim {{ run_id: \"keep-a\", amount: 20 }});\n\
         \x20   claims.push(Claim {{ run_id: \"keep-b\", amount: 30 }});\n\
         \x20   let cursor = claims.into_iter();\n\
         \x20   let retained = cursor.filter(|claim: Claim| claim.amount >= 20);\n\
         \x20   let collected = iter::collect(retained);\n\
         \x20   var total: i64 = 0;\n\
         \x20   for claim in collected {{\n\
         \x20       total = total + claim.amount;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var checksum: i64 = 0;\n\
         \x20   for frame in 0..{frames} {{\n\
         \x20       let total = retained_total(frame);\n\
         \x20       if total != 50 {{ return 91; }}\n\
         \x20       checksum = checksum + total;\n\
         \x20       print(\"frame\");\n\
         \x20   }}\n\
         \x20   if checksum != {frames} * 50 {{ return 92; }}\n\
         \x20   0\n\
         }}\n"
    )
}

fn assert_no_double_free(source: &str) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("vec-iter-filter-collect-scribble-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), "vec_iter_filter_collect_scribble");
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "owned record filter/collect must release every record exactly once:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "frameframeframe",
        "the probe must retain exactly the two expected records per frame"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn owned_record_filter_collect_has_no_per_frame_leak_slope() {
    if !host_can_inspect_process() {
        return;
    }
    assert_frame_slope_below_tolerance(
        "vec_iter_owned_record_filter_collect",
        owned_record_filter_collect_source,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn owned_record_filter_collect_does_not_double_free() {
    assert_no_double_free(&owned_record_filter_collect_source(3));
}

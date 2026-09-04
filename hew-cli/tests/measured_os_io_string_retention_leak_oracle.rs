//! Darwin leak-slope witness for representative measured OS/I/O wrappers.
//!
//! The paired canary covers all eighteen symbols structurally. This probe
//! executes the materially different lowerings that can actually run in a
//! loop without external fixture setup: direct OS wrappers, the forwarded
//! `CommandOutput` stdout/stderr aggregate, and the take-and-clear compression
//! error wrapper. One `frame` line per iteration is the exact work witness.

#![cfg(unix)]

mod support;

use support::leak_slope::assert_frame_slope_below_tolerance_exact_lines;

const SOURCE: &str = r#"
import std.encoding.compress;
import std.os;
import std.process;

fn one_frame() -> i64 {
    let arg = match os.args().get(0) {
        .Some(value) => value,
        .None => return 91,
    };
    let env = os.env("PATH").unwrap_or("");
    let cwd = os.cwd();
    let home = os.home_dir();
    let host = os.hostname();
    let temp = os.temp_dir();
    let process_len = match process.run("printf stdout; printf stderr >&2") {
        .Ok(output) => output.stdout.len() + output.stderr.len(),
        .Err(_) => 0,
    };
    let codec_len = match compress.gzip_decompress("not-a-gzip".to_bytes(), 1024) {
        .Ok(data) => data.len(),
        .Err(reason) => reason.len(),
    };
    let process_discard = match process.run("printf discarded") {
        .Ok(_) => 1,
        .Err(_) => 0,
    };
    let codec_discard = match compress.gzip_decompress("not-a-gzip".to_bytes(), 1024) {
        .Ok(_) => 0,
        .Err(_) => 1,
    };
    arg.len() + env.len() + cwd.len() + home.len() + host.len() + temp.len() + process_len + codec_len + process_discard + codec_discard
}

fn main() -> i64 {
    var total = 0;
    for _ in 0..__FRAMES__ {
        total = total + one_frame();
        println("frame");
    }
    if total > 0 { 0 } else { 91 }
}
"#;

fn source(frames: usize) -> String {
    SOURCE.replace("__FRAMES__", &frames.to_string())
}

fn expected_lines(frames: usize) -> usize {
    frames
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn representative_os_io_wrappers_have_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "measured_os_io_string_retention",
        source,
        expected_lines,
    );
}

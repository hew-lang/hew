//! Codegen canary for every measured OS/I/O `-> string` return shape.
//!
//! The direct runtime probes establish transfer. This companion feeds every
//! shipped Hew wrapper through the real compiler and proves two things the
//! runtime tests cannot: the wrapper reaches its intended C-ABI producer, and
//! the caller-side freshness mint reaches generated `hew_string_drop` cleanup.

mod support;

use std::process::Command;

use tempfile::tempdir;

use support::{describe_output, hew_binary, repo_root, require_codegen};

const SOURCE: &str = r#"
import std::encoding::compress;
import std::fs;
import std::io;
import std::net::dns;
import std::os;
import std::path;
import std::process;

fn all_measured_wrappers() -> i64 {
    let arg = os.args(0);
    let env = os.env("PATH");
    let cwd = os.cwd();
    let home = os.home_dir();
    let host = os.hostname();
    let temp = os.temp_dir();
    let line = io.read_line();
    let all = io.read_all();
    let direct_file = fs.read("/tmp/hew-os-io-retention-input.txt");
    let streamed_file = match fs.try_read("/tmp/hew-os-io-retention-input.txt") {
        Ok(text) => text,
        Err(error) => fs.io_error_message(error),
    };
    let absolute = path.absolute(".");
    let glob_len = match path.try_glob("/tmp/hew-os-io-retention-*.txt") {
        Ok(matches) => {
            let entry = matches.try_get(0);
            matches.close();
            match entry {
                Some(text) => text.len(),
                None => 0,
            }
        },
        Err(error) => path.path_error_message(error).len(),
    };
    let dns_direct = dns.lookup_host("127.0.0.1");
    let dns_timed = dns.lookup_host_timed("127.0.0.1", 1000);
    let compressed_reason = match compress.try_gzip_decompress("not-a-gzip".to_bytes(), 1024) {
        Ok(data) => data.len(),
        Err(reason) => reason.len(),
    };
    let process_len = match process.try_run("printf stdout; printf stderr >&2") {
        Ok(output) => output.stdout.len() + output.stderr.len(),
        Err(_) => 0,
    };
    arg.len() + env.len() + cwd.len() + home.len() + host.len() + temp.len() + line.len() + all.len() + direct_file.len() + streamed_file.len() + absolute.len() + glob_len + dns_direct.len() + dns_timed.len() + compressed_reason + process_len
}

fn main() -> i64 {
    all_measured_wrappers()
}
"#;

const SYMBOLS: &[&str] = &[
    "hew_args_get",
    "hew_cwd",
    "hew_env_get",
    "hew_home_dir",
    "hew_hostname",
    "hew_temp_dir",
    "hew_io_read_all",
    "hew_io_read_line",
    "hew_stream_collect_string",
    "hew_process_result_stderr",
    "hew_process_result_stdout",
    "hew_file_read",
    "hew_glob_error",
    "hew_glob_get",
    "hew_path_absolute",
    "hew_dns_lookup_host",
    "hew_dns_lookup_host_timed",
    "hew_compress_last_error",
];

#[test]
fn shipped_os_io_wrappers_emit_all_measured_calls_and_caller_releases() {
    require_codegen();
    let dir = tempdir().expect("temporary emit directory");
    let source = dir.path().join("os_io_retention.hew");
    std::fs::write(&source, SOURCE).expect("write Hew wrapper witness");
    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.path().to_str().expect("emit directory is UTF-8"),
            source.to_str().expect("source path is UTF-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("compile measured wrapper witness");
    assert!(
        output.status.success(),
        "measured wrapper witness must compile:\n{}",
        describe_output(&output)
    );
    let ir = std::fs::read_to_string(dir.path().join("os_io_retention.ll"))
        .expect("read generated LLVM IR");
    for symbol in SYMBOLS {
        let call = format!("@{symbol}(");
        assert!(
            ir.matches(&call).count() >= 2,
            "{symbol} must appear as both declaration and emitted wrapper call; IR:\n{ir}"
        );
    }
    let releases = ir.matches("call void @hew_string_drop").count();
    assert!(
        releases >= SYMBOLS.len(),
        "all {count} fresh string results must reach generated caller cleanup; found only {releases} \
         `hew_string_drop` calls in the complete wrapper witness",
        count = SYMBOLS.len()
    );
}

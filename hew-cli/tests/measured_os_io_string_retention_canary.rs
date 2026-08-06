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
    "hew_file_read_stream_collect_string",
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

/// Final owner slots for fresh strings which the witness keeps live across
/// later calls. The CFG oracle below proves that every path from each mint to
/// a terminating block passes through its matching `hew_string_drop`.
const STRING_OWNER_SLOTS: &[&str] = &[
    // os.args, os.env, cwd, home_dir, hostname, temp_dir, stdin line/all,
    // fs.read, fs.try_read, path.absolute, path_error_message, dns direct /
    // timed, compression error, and the two process-output field clones.
    "local_2", "local_5", "local_7", "local_9", "local_11", "local_13", "local_15", "local_17",
    "local_20", "local_32", "local_35", "local_60", "local_65", "local_69", "local_82", "local_94",
    "local_96",
];

#[derive(Debug)]
struct BasicBlock {
    name: String,
    body: String,
    successors: Vec<String>,
    terminates: bool,
}

fn function_body<'a>(ir: &'a str, name: &str) -> &'a str {
    let start = ir
        .find(&format!("define internal i64 @{name}() {{"))
        .unwrap_or_else(|| panic!("generated IR must contain {name}"));
    let body_start = start + format!("define internal i64 @{name}() {{").len();
    let body_end = ir[body_start..]
        .find("\n}\n")
        .map(|offset| body_start + offset)
        .expect("generated function must close");
    &ir[body_start..body_end]
}

fn successor_labels(line: &str) -> Vec<String> {
    let mut labels = Vec::new();
    let mut rest = line;
    while let Some((_, after_label)) = rest.split_once("label %") {
        let name: String = after_label
            .chars()
            .take_while(|character| character.is_ascii_alphanumeric() || *character == '_')
            .collect();
        if !name.is_empty() {
            labels.push(name);
        }
        rest = after_label;
    }
    labels
}

fn basic_blocks(body: &str) -> Vec<BasicBlock> {
    let mut blocks = Vec::new();
    let mut name = None;
    let mut lines = Vec::new();
    let mut push_block = |name: Option<String>, lines: &mut Vec<&str>| {
        if let Some(name) = name {
            let body = lines.join("\n");
            let successors = body.lines().flat_map(successor_labels).collect();
            let terminates = body.lines().any(|line| {
                let instruction = line.trim_start();
                instruction.starts_with("ret ")
            });
            blocks.push(BasicBlock {
                name,
                body,
                successors,
                terminates,
            });
        }
        lines.clear();
    };

    for line in body.lines() {
        let label = (!line.starts_with(' '))
            .then(|| line.split_once(':').map(|(label, _)| label))
            .flatten()
            .filter(|label| {
                !label.is_empty()
                    && label
                        .chars()
                        .all(|character| character.is_ascii_alphanumeric() || character == '_')
            });
        if let Some(label) = label {
            push_block(name.take(), &mut lines);
            name = Some(label.to_owned());
        } else {
            lines.push(line);
        }
    }
    push_block(name, &mut lines);
    blocks
}

fn slot_is_assigned(block: &BasicBlock, slot: &str) -> bool {
    block
        .body
        .lines()
        .any(|line| line.contains("store ") && line.contains(&format!("ptr %{slot},")))
}

fn slot_release_count(block: &BasicBlock, slot: &str, drop_symbol: &str) -> usize {
    if drop_symbol == "hew_string_drop" {
        let marker = format!("load ptr, ptr %{slot},");
        return block
            .body
            .split(&marker)
            .skip(1)
            .filter(|after_load| {
                after_load
                    .lines()
                    .take(2)
                    .any(|line| line.contains("call void @hew_string_drop"))
            })
            .count();
    }
    usize::from(block.body.contains(&format!("@{drop_symbol}(ptr %{slot})")))
}

fn slot_is_released_by(block: &BasicBlock, slot: &str, drop_symbol: &str) -> bool {
    slot_release_count(block, slot, drop_symbol) != 0
}

fn unbalanced_cleanup_exits(blocks: &[BasicBlock], slot: &str, drop_symbol: &str) -> Vec<String> {
    let start = blocks
        .iter()
        .position(|block| slot_is_assigned(block, slot))
        .unwrap_or_else(|| panic!("witness must assign %{slot}"));
    let mut pending = vec![(start, false)];
    let mut seen = vec![[false; 2]; blocks.len()];
    let mut unbalanced = Vec::new();

    while let Some((index, released)) = pending.pop() {
        if seen[index][usize::from(released)] {
            continue;
        }
        seen[index][usize::from(released)] = true;
        let block = &blocks[index];
        let release_count = slot_release_count(block, slot, drop_symbol);
        if release_count > 1 || (released && release_count == 1) {
            unbalanced.push(format!("{} (duplicate cleanup)", block.name));
            continue;
        }
        let released = released || release_count == 1;
        if block.terminates && !released {
            unbalanced.push(format!("{} (missing cleanup)", block.name));
            continue;
        }
        for successor in &block.successors {
            let index = blocks
                .iter()
                .position(|block| block.name == *successor)
                .unwrap_or_else(|| panic!("{} branches to unknown {successor}", block.name));
            pending.push((index, released));
        }
    }
    unbalanced
}

fn remove_return_path_string_release(body: &str, blocks: &[BasicBlock], slot: &str) -> String {
    let cleanup_block = blocks
        .iter()
        .find(|block| block.terminates && slot_is_released_by(block, slot, "hew_string_drop"))
        .unwrap_or_else(|| panic!("witness must release %{slot} before returning"));
    let block_start = body
        .find(&format!("\n{}:", cleanup_block.name))
        .map_or_else(
            || panic!("witness must contain {}", cleanup_block.name),
            |offset| offset + 1,
        );
    let marker = format!("load ptr, ptr %{slot},");
    let start = body[block_start..].find(&marker).map_or_else(
        || panic!("witness must release %{slot}"),
        |offset| block_start + offset,
    );
    let call_start = body[start..]
        .find("call void @hew_string_drop")
        .map(|offset| start + offset)
        .expect("slot load must feed hew_string_drop");
    let mut altered = body.to_owned();
    altered.replace_range(
        call_start..call_start + "call void @hew_string_drop".len(),
        "call void @hew_string_clone",
    );
    altered
}

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
        let uses = ir.matches(&format!("@{symbol}(")).count();
        assert!(
            uses >= 2,
            "the complete wrapper witness must call {symbol}, not only declare it"
        );
    }

    let body = function_body(&ir, "all_measured_wrappers");
    let blocks = basic_blocks(body);
    assert!(
        blocks
            .iter()
            .any(|block| block.name.starts_with("cancel_exit")),
        "the witness must retain cancellation cleanup paths"
    );

    for slot in STRING_OWNER_SLOTS {
        let unbalanced = unbalanced_cleanup_exits(&blocks, slot, "hew_string_drop");
        assert!(
            unbalanced.is_empty(),
            "%{slot} has unbalanced caller cleanup paths: {unbalanced:?}"
        );
    }
    // Active enum/record alternatives move their payload to the direct owner
    // slots above. The in-place drops below are the complementary cleanup for
    // inactive alternatives, including the `try_get` None/error paths.
    for destructor in [
        "__hew_enum_drop_inplace_Result$$string$IoError",
        "__hew_enum_drop_inplace_Result$$GlobResult$PathError",
        "__hew_enum_drop_inplace_Option$$string",
        "__hew_record_drop_inplace_CommandOutput",
    ] {
        assert!(
            body.contains(destructor),
            "{destructor} must clean inactive measured-string alternatives"
        );
    }

    // This is deliberately a real-witness counterfactual: if an active
    // cancellation/error cleanup for an early string is removed, the CFG
    // oracle finds the now-unbalanced terminating path.
    let altered_blocks = basic_blocks(&remove_return_path_string_release(body, &blocks, "local_2"));
    assert!(
        !unbalanced_cleanup_exits(&altered_blocks, "local_2", "hew_string_drop").is_empty(),
        "the cleanup CFG oracle must reject a missing caller release"
    );
}

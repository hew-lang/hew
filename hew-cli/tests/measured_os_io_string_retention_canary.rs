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
import std.encoding.compress;
import std.fs;
import std.io;
import std.net.dns;
import std.os;
import std.path;
import std.process;

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
        Err(error) => to_string(error),
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
        Err(error) => to_string(error).len(),
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
///
/// The `fs.try_read` result binds through the owned-carrier release funnel,
/// which moves the selected `Ok(text)` / `Err(...)` payload into a fresh local
/// and neutralizes the carrier slot before releasing it on every exit.
const STRING_OWNER_SLOTS: &[&str] = &[
    // os.args, os.env, cwd, home_dir, hostname, temp_dir, stdin line/all,
    // fs.read, fs.try_read, path.absolute, the path-error Display dispatch,
    // dns direct / timed, and the two process-output field clones. These are
    // every slot the witness releases through `hew_string_drop` — the compress `Err(reason)`
    // and process `output.stdout`/`stderr` ORIGINALS are freed by their
    // composite's recursive `EnumInPlace`/record drop (asserted below), not a
    // caller `hew_string_drop`, so no separate compress-error owner slot exists.
    "local_2", "local_5", "local_7", "local_9", "local_11", "local_13", "local_15", "local_17",
    "local_20", "local_33", "local_36", "local_61", "local_66", "local_70", "local_95", "local_97",
];

#[derive(Debug)]
struct BasicBlock {
    name: String,
    body: String,
    successors: Vec<String>,
    is_cleanup_exit: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum SlotCleanupState {
    Uninitialized,
    Live,
    Released,
    Null,
}

impl SlotCleanupState {
    const fn index(self) -> usize {
        match self {
            Self::Uninitialized => 0,
            Self::Live => 1,
            Self::Released => 2,
            Self::Null => 3,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum SlotCleanupEvent {
    Assign,
    Drop,
    Clear,
}

fn function_body<'a>(ir: &'a str, name: &str) -> &'a str {
    let signature = format!("define internal i64 @{name}()");
    let start = ir
        .find(&signature)
        .unwrap_or_else(|| panic!("generated IR must contain {name}"));
    let body_start = ir[start..]
        .find('{')
        .map(|offset| start + offset + 1)
        .expect("generated function header must open");
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
            .take_while(|character| {
                character.is_ascii_alphanumeric() || matches!(character, '_' | '.')
            })
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
            // A live lexical owner must be released both when the function
            // returns normally and when structured unwinding leaves it.
            // `unreachable` is deliberately not a cleanup exit: it follows a
            // process-fatal trap/abort or an impossible path, where lexical
            // cleanup is neither observable nor expected.
            let is_cleanup_exit = body.lines().any(|line| {
                let instruction = line.trim_start();
                instruction.starts_with("ret ") || instruction.starts_with("resume ")
            });
            blocks.push(BasicBlock {
                name,
                body,
                successors,
                is_cleanup_exit,
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
                    && label.chars().all(|character| {
                        character.is_ascii_alphanumeric() || matches!(character, '_' | '.')
                    })
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

fn is_call_instruction(line: &str) -> bool {
    line.contains("call ") || line.contains("invoke ")
}

fn slot_cleanup_events(block: &BasicBlock, slot: &str, drop_symbol: &str) -> Vec<SlotCleanupEvent> {
    let lines: Vec<_> = block.body.lines().collect();
    let destination = format!(", ptr %{slot},");
    let load = format!("load ptr, ptr %{slot},");
    let drop = format!("@{drop_symbol}(");
    let mut events = Vec::new();

    for (index, line) in lines.iter().enumerate() {
        if line.contains("store ptr ") && line.contains(&destination) {
            let event = if line.trim_start().starts_with("store ptr null,") {
                SlotCleanupEvent::Clear
            } else {
                SlotCleanupEvent::Assign
            };
            events.push((index, event));
        }

        if line.contains(&load) {
            if let Some((offset, _)) = lines[index + 1..]
                .iter()
                .take(2)
                .enumerate()
                .find(|(_, line)| is_call_instruction(line) && line.contains(&drop))
            {
                events.push((index + offset + 1, SlotCleanupEvent::Drop));
            }
        }
    }

    events.sort_by_key(|(index, _)| *index);
    events.into_iter().map(|(_, event)| event).collect()
}

fn slot_is_assigned(block: &BasicBlock, slot: &str) -> bool {
    slot_cleanup_events(block, slot, "hew_string_drop").contains(&SlotCleanupEvent::Assign)
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
                    .any(|line| is_call_instruction(line) && line.contains("@hew_string_drop("))
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
    let mut pending = vec![(start, SlotCleanupState::Uninitialized)];
    let mut seen = vec![[false; 4]; blocks.len()];
    let mut unbalanced = Vec::new();

    while let Some((index, mut state)) = pending.pop() {
        if seen[index][state.index()] {
            continue;
        }
        seen[index][state.index()] = true;
        let block = &blocks[index];

        let mut invalid = None;
        for event in slot_cleanup_events(block, slot, drop_symbol) {
            state = match (state, event) {
                (
                    SlotCleanupState::Uninitialized
                    | SlotCleanupState::Null
                    | SlotCleanupState::Released,
                    SlotCleanupEvent::Assign,
                ) => SlotCleanupState::Live,
                (SlotCleanupState::Live, SlotCleanupEvent::Drop) => SlotCleanupState::Released,
                (SlotCleanupState::Null, SlotCleanupEvent::Drop | SlotCleanupEvent::Clear)
                | (
                    SlotCleanupState::Uninitialized | SlotCleanupState::Released,
                    SlotCleanupEvent::Clear,
                ) => SlotCleanupState::Null,
                (SlotCleanupState::Live, SlotCleanupEvent::Assign) => {
                    invalid = Some("overwritten before cleanup");
                    break;
                }
                (SlotCleanupState::Live, SlotCleanupEvent::Clear) => {
                    invalid = Some("cleared without cleanup");
                    break;
                }
                (SlotCleanupState::Released, SlotCleanupEvent::Drop) => {
                    invalid = Some("duplicate cleanup");
                    break;
                }
                (SlotCleanupState::Uninitialized, SlotCleanupEvent::Drop) => {
                    invalid = Some("cleanup before assignment");
                    break;
                }
            };
        }
        if let Some(reason) = invalid {
            unbalanced.push(format!("{} ({reason})", block.name));
            continue;
        }

        if block.is_cleanup_exit && state == SlotCleanupState::Live {
            unbalanced.push(format!("{} (missing cleanup)", block.name));
            continue;
        }
        for successor in &block.successors {
            let index = blocks
                .iter()
                .position(|block| block.name == *successor)
                .unwrap_or_else(|| panic!("{} branches to unknown {successor}", block.name));
            pending.push((index, state));
        }
    }
    unbalanced
}

fn remove_return_path_string_release(body: &str, blocks: &[BasicBlock], slot: &str) -> String {
    let cleanup_block = blocks
        .iter()
        .find(|block| block.is_cleanup_exit && slot_is_released_by(block, slot, "hew_string_drop"))
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
        .find("@hew_string_drop")
        .map(|offset| start + offset)
        .expect("slot load must feed hew_string_drop");
    let mut altered = body.to_owned();
    altered.replace_range(
        call_start..call_start + "@hew_string_drop".len(),
        "@hew_string_clone",
    );
    altered
}

#[test]
fn cleanup_cfg_treats_resume_as_an_owner_exit() {
    let missing_cleanup = basic_blocks(
        r"entry:
  store ptr %value, ptr %local_2, align 8
  br label %unwind

unwind:
  resume { ptr, i32 } zeroinitializer",
    );
    assert_eq!(
        unbalanced_cleanup_exits(&missing_cleanup, "local_2", "hew_string_drop"),
        vec!["unwind (missing cleanup)".to_owned()],
        "a live owner reaching resume must be rejected"
    );

    let released_before_resume = basic_blocks(
        r"entry:
  store ptr %value, ptr %local_2, align 8
  br label %unwind

unwind:
  %owned = load ptr, ptr %local_2, align 8
  call void @hew_string_drop(ptr %owned)
  resume { ptr, i32 } zeroinitializer",
    );
    assert!(
        unbalanced_cleanup_exits(&released_before_resume, "local_2", "hew_string_drop").is_empty(),
        "a resume path that releases its live owner must remain valid"
    );

    let process_fatal = basic_blocks(
        r"entry:
  store ptr %value, ptr %local_2, align 8
  br label %fatal

fatal:
  call void @llvm.trap()
  unreachable",
    );
    assert!(
        unbalanced_cleanup_exits(&process_fatal, "local_2", "hew_string_drop").is_empty(),
        "unreachable after a process-fatal trap is not a lexical cleanup exit"
    );
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
            "--emit-llvm",
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
    // LLVM's `print_to_file` (via `LLVMPrintModuleToFile`) opens the `.ll`
    // destination in Windows text mode, translating every emitted `\n` to
    // `\r\n` — a platform convention of LLVM's own IR-text writer, not a
    // codegen defect (the module's structure, including this function's
    // closing brace, is unaffected). Normalize once here so every
    // byte-oriented search below (`function_body`'s `"\n}\n"` scan in
    // particular) sees the same shape on every platform; `.lines()`-based
    // parsing elsewhere in this file already tolerates CRLF, so this is the
    // single place the raw text needs it.
    let ir = std::fs::read_to_string(dir.path().join("os_io_retention.ll"))
        .expect("read generated LLVM IR")
        .replace("\r\n", "\n");
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
    // Each heap-owning composite whose payload is a clone-drop-safe (string /
    // bytes / recursively-synthesizable record) leaf keeps its recursive
    // `EnumInPlace` drop, which frees the ACTIVE payload the arm binder only
    // borrowed (the `.len()` reads clone or borrow the leaves; the composite
    // owns and frees the originals). The `Result<CommandOutput, ProcessError>`
    // entry is the direct proof of the composite-field leak fix: the process
    // result's own `stdout`/`stderr` strings are freed through this enum drop's
    // recursion into `__hew_record_drop_inplace_std.process.CommandOutput`, not
    // leaked. `Result<GlobResult, PathError>` is deliberately absent: its
    // `#[resource]` payload is closed exactly once by `matches.close()`, so it
    // keeps no `EnumInPlace` drop (adding one would double-close the resource).
    for destructor in [
        "__hew_enum_drop_inplace_Result$$string$std$mfs$mIoError",
        "__hew_enum_drop_inplace_Result$$bytes$string",
        "__hew_enum_drop_inplace_Option$$string",
        "__hew_enum_drop_inplace_Result$$std$mprocess$mCommandOutput$std$mprocess$mProcessError",
    ] {
        assert!(
            body.contains(destructor),
            "{destructor} must clean the measured composite's owned payload"
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

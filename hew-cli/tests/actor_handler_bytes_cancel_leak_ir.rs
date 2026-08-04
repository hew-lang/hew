//! W60.114 follow-up — IR-structural teeth for the path-sensitive cancellation
//! authority `derive_bytes_actor_transfer_blocks` closes.
//!
//! A receive handler that does some pre-transfer work (forcing a
//! `CooperateKind::FunctionEntry` cancellation checkpoint at block 0 — a
//! handler whose FIRST block terminator is already the forwarding `send` gets
//! no checkpoint at all, the actor cooperates via the yield-equivalent send)
//! and then forwards its owned `bytes` parameter to another actor used to
//! leak that parameter on cancellation reached BEFORE the forward: the
//! escape scan that lets a genuinely transferred `bytes` value skip its own
//! scope-exit release excluded the binding WHOLE-FUNCTION, so the
//! cancellation branch — which fires in the prologue, strictly before the
//! rest of the handler body (including the forwarding `send`) ever runs —
//! inherited the exclusion too.
//!
//! Runtime cancellation timing is not deterministically reproducible in a
//! unit test (same rationale as `raii1_record_resource_field_leak_oracle`'s
//! `raii1_async_cancel_drop_spine_reaches_close`), so this pins the fix
//! structurally: the emitted `cancel_exit` block must call
//! `hew_bytes_drop`, the successful-forward path must not, and a
//! non-cooperating counterfactual (no checkpoint emitted at all) proves the
//! assertion is not satisfiable by a blanket "always drop" mutation.

mod support;

use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::path::Path;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Forwarding handler shaped exactly like Opus's counterexample: a `println`
/// before the forward forces a `FunctionEntry` cooperate checkpoint, so the
/// codegen'd handler branches on `hew_actor_cooperate()`'s result into a
/// `cancel_exit` block before ever reaching the `send`.
const COOPERATE_THEN_FORWARD_SOURCE: &str = r#"
actor Recipient {
    receive fn take(data: bytes) { println("DATA"); }
}
actor Forwarder {
    let recipient: LocalPid<Recipient>;
    receive fn forward(data: bytes) {
        println("forwarding");
        recipient.take(data);
    }
}
fn main() -> i64 { 0 }
"#;

/// Counterfactual: forwarding as the handler's ONLY statement is itself a
/// yield-equivalent first-block terminator (`compute_cooperate_sites`
/// suppresses the `FunctionEntry` site when the entry block already yields),
/// so no `cancel_exit` block is emitted at all — nothing to assert a drop
/// inside. Proves the positive assertion is anchored to a real cooperate
/// checkpoint, not a blanket emission.
const IMMEDIATE_FORWARD_SOURCE: &str = r#"
actor Recipient {
    receive fn take(data: bytes) { println("DATA"); }
}
actor Forwarder {
    let recipient: LocalPid<Recipient>;
    receive fn forward(data: bytes) { recipient.take(data); }
}
fn main() -> i64 { 0 }
"#;

/// Compile `source` and read the emitted `<name>.ll` LLVM IR text.
fn compile_and_read_ll(source: &str, dir: &Path, name: &str) -> String {
    let hew_src = dir.join(format!("{name}.hew"));
    std::fs::write(&hew_src, source).expect("write hew source");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.to_str().expect("emit-dir utf-8"),
            hew_src.to_str().expect("hew src utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");
    assert!(
        output.status.success(),
        "hew compile failed for {name}:\n{}",
        describe_output(&output)
    );

    let ll = dir.join(format!("{name}.ll"));
    std::fs::read_to_string(&ll).unwrap_or_else(|e| panic!("read emitted IR {}: {e}", ll.display()))
}

/// Extract a function's whole body: the lines from the `define` containing
/// `needle` up to and including the closing `}` line.
fn fn_body(ll: &str, needle: &str) -> Option<String> {
    let mut body = String::new();
    let mut in_fn = false;
    for line in ll.lines() {
        if !in_fn {
            if line.starts_with("define") && line.contains(needle) {
                in_fn = true;
                body.push_str(line);
                body.push('\n');
            }
            continue;
        }
        body.push_str(line);
        body.push('\n');
        if line == "}" {
            return Some(body);
        }
    }
    None
}

/// Isolate the `cancel_exit:` block's own instructions — from its label line
/// to the next label line (or the function's closing brace) — so the drop
/// assertion cannot be satisfied by a `hew_bytes_drop` call that lives on
/// some OTHER block (e.g. the normal post-send cleanup) of the same
/// function. LLVM textual IR writes a basic-block label flush-left, its
/// first whitespace-delimited token ending in `:` (often followed by a
/// `; preds = …` comment); every instruction line is indented, so the
/// boundary is unambiguous.
fn is_block_label(line: &str) -> bool {
    !line.starts_with(char::is_whitespace)
        && line
            .split_whitespace()
            .next()
            .is_some_and(|token| token.ends_with(':'))
}

/// Split an LLVM function body into its labelled basic blocks.
fn basic_blocks(body: &str) -> BTreeMap<String, String> {
    let mut blocks = BTreeMap::new();
    let mut current_name: Option<String> = None;
    let mut current_body = String::new();
    for line in body.lines() {
        if is_block_label(line) {
            if let Some(name) = current_name.take() {
                blocks.insert(name, std::mem::take(&mut current_body));
            }
            current_name = line
                .split_whitespace()
                .next()
                .and_then(|token| token.strip_suffix(':'))
                .map(str::to_owned);
        }
        if current_name.is_some() {
            current_body.push_str(line);
            current_body.push('\n');
        }
    }
    if let Some(name) = current_name {
        blocks.insert(name, current_body);
    }
    blocks
}

fn block_targets(block: &str) -> Vec<String> {
    block
        .split("label %")
        .skip(1)
        .filter_map(|tail| {
            let target = tail
                .split(|ch: char| ch == ',' || ch.is_whitespace())
                .next()
                .unwrap_or_default();
            (!target.is_empty()).then(|| target.to_owned())
        })
        .collect()
}

fn reachable_blocks(blocks: &BTreeMap<String, String>, root: &str) -> BTreeSet<String> {
    let mut reachable = BTreeSet::new();
    let mut pending = VecDeque::from([root.to_owned()]);
    while let Some(name) = pending.pop_front() {
        if !reachable.insert(name.clone()) {
            continue;
        }
        if let Some(block) = blocks.get(&name) {
            pending.extend(
                block_targets(block)
                    .into_iter()
                    .filter(|target| blocks.contains_key(target)),
            );
        }
    }
    reachable
}

fn local_drop_blocks(blocks: &BTreeMap<String, String>, local_operand: &str) -> BTreeSet<String> {
    blocks
        .iter()
        .filter(|(_, block)| {
            block.lines().any(|line| {
                line.contains("call void @hew_bytes_drop(")
                    && line_reads_local(block, line, local_operand)
            })
        })
        .map(|(name, _)| name.clone())
        .collect()
}

fn owner_is_false_on_entry(
    blocks: &BTreeMap<String, String>,
    block_name: &str,
    active_slot: &str,
) -> bool {
    let predecessors: Vec<_> = blocks
        .iter()
        .filter(|(_, block)| {
            block_targets(block)
                .iter()
                .any(|target| target == block_name)
        })
        .collect();
    !predecessors.is_empty()
        && predecessors.iter().all(|(_, predecessor)| {
            if predecessor.contains(&format!("store i1 false, ptr {active_slot}")) {
                return true;
            }
            let loaded_flag = predecessor.lines().find_map(|line| {
                let (name, tail) = line.trim().split_once(" = load i1, ptr ")?;
                tail.starts_with(active_slot).then_some(name)
            });
            loaded_flag.is_some_and(|flag| {
                predecessor.lines().any(|line| {
                    line.trim_start().starts_with(&format!("br i1 {flag},"))
                        && line.contains(&format!(", label %{block_name}"))
                })
            })
        })
}

#[test]
fn cooperate_then_forward_drops_bytes_in_the_cancel_exit_block() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("bytes-cancel-ir-")
        .tempdir()
        .expect("tempdir");
    let ll = compile_and_read_ll(
        COOPERATE_THEN_FORWARD_SOURCE,
        dir.path(),
        "cooperate_forward",
    );

    let body = fn_body(&ll, "@Forwarder__recv__forward")
        .expect("missing `Forwarder__recv__forward` definition");
    assert!(
        body.contains("cancel_exit"),
        "a handler with pre-transfer work must emit a function-entry cooperate \
         checkpoint (a `cancel_exit` block):\n{body}"
    );
    let blocks = basic_blocks(&body);
    let drops = local_drop_blocks(&blocks, "%local_0");
    let cancel_reachable = reachable_blocks(&blocks, "cancel_exit");
    let cancel_local_drops = drops.intersection(&cancel_reachable).count();
    assert_eq!(
        cancel_local_drops, 1,
        "the cancellation edge must retire its crash snapshot and release \
         `%local_0` exactly once:\n{body}"
    );
    let normal_reachable = reachable_blocks(&blocks, "after_cooperate");
    assert!(
        drops
            .intersection(&cancel_reachable)
            .all(|drop| !normal_reachable.contains(drop)),
        "the cancel-owned drop must not be reachable from the normal cooperate \
         continuation:\n{body}"
    );
}

#[test]
fn cooperate_then_forward_does_not_drop_bytes_on_the_normal_send_path() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("bytes-cancel-ir-")
        .tempdir()
        .expect("tempdir");
    let ll = compile_and_read_ll(
        COOPERATE_THEN_FORWARD_SOURCE,
        dir.path(),
        "cooperate_forward_ok",
    );

    let body = fn_body(&ll, "@Forwarder__recv__forward")
        .expect("missing `Forwarder__recv__forward` definition");
    // Everything OUTSIDE the cancel_exit block — the normal `after_cooperate`
    // continuation through the `send` and the function's return — must never
    // release `%local_0` (the handler's own `bytes` parameter slot): the
    // mailbox hand-off is its sole release. `actor_send_fail` legitimately
    // drops `%local_3` — the payload TEMP the transfer moved into — on a
    // failed delivery (a distinct, pre-existing mechanism this fix does not
    // touch), so the assertion is scoped to the parameter's own slot rather
    // than to every `hew_bytes_drop` call in the function.
    let blocks = basic_blocks(&body);
    let drops = local_drop_blocks(&blocks, "%local_0");
    let cancel_reachable = reachable_blocks(&blocks, "cancel_exit");
    let residual_drops: BTreeSet<_> = drops.difference(&cancel_reachable).cloned().collect();
    let guarded_return = reachable_blocks(&blocks, "helper_crash_cleanup_return_drop_0");
    let inactive_return = reachable_blocks(&blocks, "helper_crash_cleanup_return_inactive_0");
    assert!(
        !residual_drops.is_empty()
            && residual_drops.is_subset(&guarded_return)
            && residual_drops.is_disjoint(&inactive_return),
        "any residual return fallback must be reachable only while the crash \
         owner is active:\n{body}"
    );

    let (send_block_name, send_block) = blocks
        .iter()
        .find(|(_, block)| block.contains("call i32 @hew_actor_send_by_id"))
        .expect("forwarding handler must contain the mailbox send");
    assert!(
        send_block.contains("zeroinitializer, ptr %local_0")
            && owner_is_false_on_entry(&blocks, send_block_name, "%helper_crash_cleanup_active_0",),
        "the normal transfer must neutralize `%local_0` only after every edge \
         into the send block proves its crash owner inactive:\n{body}"
    );
}

/// True when the `hew_bytes_drop` call on `drop_line` is releasing a pointer
/// loaded from `local_operand`'s own alloca — i.e. this specific drop
/// targets that local, not some other `bytes` temp in the same function.
/// Walks backward from `drop_line` for the `getelementptr` that seeds the
/// dropped pointer and checks whether ITS base operand is `local_operand`.
fn line_reads_local(body: &str, drop_line: &str, local_operand: &str) -> bool {
    let drop_ptr = drop_line
        .rsplit("call void @hew_bytes_drop(ptr ")
        .next()
        .and_then(|rest| rest.strip_suffix(')'))
        .unwrap_or_default();
    let load_line = body
        .lines()
        .find(|l| l.contains(&format!("{drop_ptr} = load ptr")));
    let Some(load_line) = load_line else {
        return false;
    };
    let slot = load_line
        .split(", ptr ")
        .nth(1)
        .and_then(|rest| rest.split(',').next())
        .unwrap_or_default();
    body.lines()
        .find(|l| {
            l.trim_start()
                .starts_with(&format!("{slot} = getelementptr"))
        })
        .is_some_and(|gep_line| gep_line.contains(local_operand))
}

/// Mutation-resistance counterfactual: a handler that forwards as its ONLY
/// statement never emits a `FunctionEntry` cooperate checkpoint at all (its
/// first block already ends in the yield-equivalent `send`), so there is no
/// `cancel_exit` block to carry a drop. This proves the positive assertion
/// above is anchored to a real cooperate-then-transfer shape, not to a
/// blanket "always emit `hew_bytes_drop` somewhere in a forwarding handler"
/// mutation that would vacuously satisfy it.
#[test]
fn immediate_forward_emits_no_cancel_exit_block() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("bytes-cancel-ir-")
        .tempdir()
        .expect("tempdir");
    let ll = compile_and_read_ll(IMMEDIATE_FORWARD_SOURCE, dir.path(), "immediate_forward");

    let body = fn_body(&ll, "@Forwarder__recv__forward")
        .expect("missing `Forwarder__recv__forward` definition");
    assert!(
        !body.contains("cancel_exit"),
        "a handler whose first block already yields (the forwarding `send` \
         itself) must suppress the function-entry cooperate checkpoint \
         entirely — nothing to assert a cancel-path drop against:\n{body}"
    );
}

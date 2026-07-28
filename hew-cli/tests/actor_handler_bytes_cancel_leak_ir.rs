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

/// Split `body`'s lines into (inside the `cancel_exit:` block, everywhere
/// else), each rejoined as a newline-separated string.
fn split_on_cancel_exit(body: &str) -> (String, String) {
    let mut inside = String::new();
    let mut outside = String::new();
    let mut in_cancel_block = false;
    for line in body.lines() {
        if line.starts_with("cancel_exit:") {
            in_cancel_block = true;
        } else if in_cancel_block && (is_block_label(line) || line == "}") {
            in_cancel_block = false;
        }
        let dest = if in_cancel_block || line.starts_with("cancel_exit:") {
            &mut inside
        } else {
            &mut outside
        };
        dest.push_str(line);
        dest.push('\n');
    }
    (inside, outside)
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
    let (cancel_block, _outside) = split_on_cancel_exit(&body);
    assert!(
        cancel_block.contains("call void @hew_bytes_drop("),
        "cancellation reached before the forwarding send must release the \
         still-untransferred handler-owned `bytes` parameter in its OWN \
         cancel_exit block:\n{cancel_block}\n\nfull function:\n{body}"
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
    let (_cancel_block, outside_cancel) = split_on_cancel_exit(&body);
    let local_0_drop_outside_cancel = outside_cancel.lines().any(|line| {
        line.contains("call void @hew_bytes_drop(")
            && outside_cancel
                .lines()
                .take_while(|l| *l != line)
                .last()
                .is_some()
            && line_reads_local(&outside_cancel, line, "%local_0")
    });
    assert!(
        !local_0_drop_outside_cancel,
        "the successful-forward path transfers the sole `bytes` reference to \
         the recipient's mailbox and must not also drop the parameter slot \
         `%local_0`:\n{outside_cancel}"
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

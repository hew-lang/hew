//! Executed ownership-boundary regressions for user `#[resource]` values.
//!
//! These shapes used to agree in the checker and still duplicate a resource at
//! MIR/codegen boundaries: generic/local call carriers, `Arena<T>::insert`,
//! actor message transfer, closure environments, and a resource parameter
//! followed by an ordinary ABI argument. The stdout assertions make a user
//! close ritual an exact-count oracle; the raw-MIR assertions pin the transfer
//! authority that makes the runtime result structural rather than accidental.

#![cfg(unix)]

mod support;

use std::path::Path;
use std::process::{Command, Output};

use support::leak_slope::compile_to_native;
use support::{describe_output, hew_binary, repo_root, require_codegen, strip_ansi};

const TOKEN: &str = r#"
#[resource]
type Token {
    id: i64;
}

impl Token {
    fn close(self) {
        println(f"closing token id={self.id}");
    }
}
"#;

const IDENTITY_BODY: &str = r"
fn identity<T>(value: T) -> T {
    value
}

fn main() -> i64 {
    let token = identity(Token { id: 99 });
    token.close();
    0
}
";

const NESTED_IDENTITY_BODY: &str = r"
type Wrap {
    token: Token;
}

fn identity<T>(value: T) -> T {
    value
}

fn main() -> i64 {
    let _wrapped = identity(Wrap { token: Token { id: 88 } });
    0
}
";

const ARENA_BODY: &str = r#"
import std.arena;

fn main() -> i64 {
    let store: arena.Arena<Token> = arena.new();
    println("before insert");
    let _key = store.insert(Token { id: 41 });
    println("after insert");
    0
}
"#;

const ACTOR_BODY: &str = r"
actor ProbeSink {
    receive fn take(value: Token) {
        value.close();
    }

    receive fn fence() -> i64 {
        1
    }
}

fn main() -> i64 {
    let sink = spawn ProbeSink;
    let token = Token { id: 77 };
    sink.take(token);
    match await sink.fence() {
        Ok(_) => 0,
        Err(_) => 2,
    }
}
";

const PARAMETER_ORDER_BODY: &str = r"
fn store_or_drop(items: Vec<Token>, value: Token, store: bool) -> Vec<Token> {
    if store {
        items.push(value);
    }
    items
}

fn main() -> i64 {
    let items: Vec<Token> = Vec::new();
    let stored = store_or_drop(items, Token { id: 13 }, true);
    println(stored.len());
    0
}
";

const CLOSURE_BODY: &str = r"
fn close_later(value: Token) {
    let closer = move || {
        value.close();
    };
    closer();
}

fn main() -> i64 {
    close_later(Token { id: 5 });
    0
}
";

const CHANNEL_SENDER_CLONE_BODY: &str = r"
import std.channel.channel;

fn main() -> i64 {
    let (tx, _rx): (channel.Sender<Token>, channel.Receiver<Token>) = channel.new(4);
    let senders: Vec<channel.Sender<Token>> = [tx];
    let senders_copy = senders.clone();
    println(senders_copy.len());
    0
}
";

const CHANNEL_RECEIVER_CLONE_BODY: &str = r"
import std.channel.channel;

fn main() -> i64 {
    let (_tx, rx): (channel.Sender<Token>, channel.Receiver<Token>) = channel.new(4);
    let receivers: Vec<channel.Receiver<Token>> = [rx];
    let _receivers_copy = receivers.clone();
    0
}
";

/// Receiver slots have no clone thunk: this is the sole accepted construction
/// shape, which moves `rx` into the descriptor-backed Vec then lets the Vec
/// close it at scope exit.
const CHANNEL_RECEIVER_MOVE_BODY: &str = r"
import std.channel.channel;

fn main() -> i64 {
    let (_tx, rx): (channel.Sender<Token>, channel.Receiver<Token>) = channel.new(4);
    let receivers: Vec<channel.Receiver<Token>> = [rx];
    println(receivers.len());
    0
}
";

const CHANNEL_RECEIVER_GET_BODY: &str = r"
import std.channel.channel;

fn main() -> i64 {
    let (_tx, rx): (channel.Sender<Token>, channel.Receiver<Token>) = channel.new(4);
    let receivers: Vec<channel.Receiver<Token>> = [rx];
    let _item = receivers.get(0);
    0
}
";

const CHANNEL_RECEIVER_INDEX_BODY: &str = r"
import std.channel.channel;

fn main() -> i64 {
    let (_tx, rx): (channel.Sender<Token>, channel.Receiver<Token>) = channel.new(4);
    let receivers: Vec<channel.Receiver<Token>> = [rx];
    let _item = receivers[0];
    0
}
";

const CHANNEL_RECEIVER_SLICE_BODY: &str = r"
import std.channel.channel;

fn main() -> i64 {
    let (_tx, rx): (channel.Sender<Token>, channel.Receiver<Token>) = channel.new(4);
    let receivers: Vec<channel.Receiver<Token>> = [rx];
    let _slice = receivers[0..1];
    0
}
";

const CHANNEL_RECEIVER_ITER_BODY: &str = r"
import std.channel.channel;

fn main() -> i64 {
    let (_tx, rx): (channel.Sender<Token>, channel.Receiver<Token>) = channel.new(4);
    let receivers: Vec<channel.Receiver<Token>> = [rx];
    let _iter = receivers.iter();
    0
}
";

const CHANNEL_RECEIVER_COPY_PUSH_BODY: &str = r"
import std.channel.channel;

fn main() -> i64 {
    let (_tx, rx): (channel.Sender<Token>, channel.Receiver<Token>) = channel.new(4);
    var receivers: Vec<channel.Receiver<Token>> = Vec::new();
    receivers.push(rx);
    0
}
";

const CHANNEL_RECEIVER_COPY_SET_BODY: &str = r"
import std.channel.channel;

fn main() -> i64 {
    let (_tx1, rx1): (channel.Sender<Token>, channel.Receiver<Token>) = channel.new(4);
    let (_tx2, rx2): (channel.Sender<Token>, channel.Receiver<Token>) = channel.new(4);
    var receivers: Vec<channel.Receiver<Token>> = [rx1];
    receivers.set(0, rx2);
    0
}
";

fn source(body: &str) -> String {
    format!("{TOKEN}\n{body}")
}

fn compile_and_run(name: &str, body: &str) -> Output {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("affine-resource-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(&source(body), dir.path(), name);
    Command::new(bin)
        .current_dir(repo_root())
        .output()
        .unwrap_or_else(|error| panic!("run {name}: {error}"))
}

fn assert_exact_runtime(name: &str, body: &str, expected_stdout: &str) {
    let output = compile_and_run(name, body);
    assert!(
        output.status.success(),
        "{name} must exit cleanly:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        expected_stdout,
        "{name} must preserve exactly one resource authority:\n{}",
        describe_output(&output)
    );
}

fn raw_mir(name: &str, body: &str) -> String {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("affine-resource-mir-{name}-"))
        .tempdir()
        .expect("tempdir");
    let input = dir.path().join(format!("{name}.hew"));
    std::fs::write(&input, source(body)).expect("write MIR fixture");
    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--dump-mir",
            "raw",
            input.to_str().expect("fixture path utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew MIR dump");
    assert!(
        output.status.success(),
        "{name} raw MIR must compile:\n{}",
        describe_output(&output)
    );
    String::from_utf8_lossy(&output.stdout).into_owned()
}

fn compile_rejected(name: &str, body: &str) -> String {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("affine-resource-reject-{name}-"))
        .tempdir()
        .expect("tempdir");
    let input = dir.path().join(format!("{name}.hew"));
    std::fs::write(&input, source(body)).expect("write rejection fixture");
    let output = Command::new(hew_binary())
        .arg("compile")
        .arg(&input)
        .current_dir(repo_root())
        .output()
        .expect("invoke rejection compile");
    assert!(
        !output.status.success(),
        "{name} must be rejected before native code generation:\n{}",
        describe_output(&output)
    );
    strip_ansi(&String::from_utf8_lossy(&output.stderr))
}

fn function<'a>(dump: &'a str, header: &str) -> &'a str {
    let start = dump
        .find(header)
        .unwrap_or_else(|| panic!("missing `{header}` in MIR:\n{dump}"));
    let tail = &dump[start..];
    let end = tail[header.len()..]
        .find("\nfn ")
        .map_or(tail.len(), |offset| header.len() + offset);
    &tail[..end]
}

fn assert_ordered(haystack: &str, needles: &[&str]) {
    let mut cursor = 0;
    for needle in needles {
        let offset = haystack[cursor..]
            .find(needle)
            .unwrap_or_else(|| panic!("missing ordered `{needle}` in:\n{haystack}"));
        cursor += offset + needle.len();
    }
}

#[test]
fn generic_identity_transfers_direct_and_nested_resource_authority_once() {
    assert_exact_runtime("identity", IDENTITY_BODY, "closing token id=99\n");
    assert_exact_runtime(
        "nested_identity",
        NESTED_IDENTITY_BODY,
        "closing token id=88\n",
    );
}

#[test]
fn arena_insert_does_not_close_the_inserted_resource_carrier() {
    assert_exact_runtime(
        "arena_insert",
        ARENA_BODY,
        "before insert\nafter insert\nclosing token id=41\n",
    );
}

#[test]
fn actor_message_send_transfers_named_resource_authority_once() {
    assert_exact_runtime("actor_send", ACTOR_BODY, "closing token id=77\n");
}

#[test]
fn carrier_helper_locals_do_not_displace_trailing_abi_parameters() {
    assert_exact_runtime(
        "parameter_order",
        PARAMETER_ORDER_BODY,
        "1\nclosing token id=13\n",
    );
}

#[test]
fn raw_mir_carries_guards_and_preserves_the_parameter_prefix() {
    let identity = raw_mir("identity_guard", IDENTITY_BODY);
    let identity_fn = function(&identity, "fn identity$$Token(Token) -> Token");
    assert_ordered(
        identity_fn,
        &[
            "_1 = const.i64 0",
            "_2 = move _0",
            "neutralize_payload _0 -> _2 [WholeCarrierConsume]",
            "_1 = const.i64 1",
            "snapshot_drop _0 ty=Token plan=UserRecord { name: \"Token\" } boundary=LocalCall guard=_1",
        ],
    );

    let nested = raw_mir("nested_identity_guard", NESTED_IDENTITY_BODY);
    let nested_fn = function(&nested, "fn identity$$Wrap(Wrap) -> Wrap");
    assert!(
        nested_fn.contains(
            "snapshot_drop _0 ty=Wrap plan=UserRecord { name: \"Wrap\" } boundary=LocalCall guard=_1"
        ),
        "a resource nested below an unmarked record must retain the whole-carrier guard:\n{nested_fn}"
    );

    let arena = raw_mir("arena_guard", ARENA_BODY);
    let insert_fn = function(
        &arena,
        "fn std.arena.Arena::insert$$Token(std.arena.Arena<Token>, Token) -> std.arena.Key<Token>",
    );
    assert!(
        insert_fn.contains(
            "snapshot_drop _1 ty=Token plan=UserRecord { name: \"Token\" } boundary=LocalCall guard=_2"
        ),
        "Arena::insert's generic Token carrier must have a guarded terminal drop:\n{insert_fn}"
    );
    assert!(
        insert_fn.contains("call hew_vec_push_owned_move("),
        "Arena::insert's fresh Slot carrier must move into Vec storage:\n{insert_fn}"
    );
    assert_ordered(
        insert_fn,
        &[
            "neutralize_payload _1",
            "[WholeCarrierConsume]",
            "_2 = const.i64 1",
        ],
    );

    let actor = raw_mir("actor_guard", ACTOR_BODY);
    let actor_main = function(&actor, "fn main() -> i64");
    assert_ordered(
        actor_main,
        &[
            "token site=",
            "intent=Consume",
            "= const.i64 1",
            "[SendTransferLastUse]",
            "send actor0",
        ],
    );

    let order = raw_mir("parameter_prefix", PARAMETER_ORDER_BODY);
    let helper = function(
        &order,
        "fn store_or_drop(Vec<Token>, Token, bool) -> Vec<Token>",
    );
    assert_ordered(
        helper,
        &["_0: Vec<Token>", "_1: Token", "_2: bool", "_3: i64"],
    );
    assert!(
        helper.contains("branch _2 ?"),
        "the trailing bool argument must remain parameter local _2:\n{helper}"
    );
}

#[test]
fn consuming_a_resource_from_a_reusable_closure_fails_closed() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("affine-resource-closure-")
        .tempdir()
        .expect("tempdir");
    let input = dir.path().join("closure_consume.hew");
    std::fs::write(&input, source(CLOSURE_BODY)).expect("write closure fixture");
    let emit_dir = dir.path().join("emit");
    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            emit_dir.to_str().expect("emit path utf-8"),
            input.to_str().expect("fixture path utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew closure compile");
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        !output.status.success(),
        "a reusable closure must not byte-copy and consume its environment-owned resource"
    );
    assert!(
        stderr.contains("E_NOT_YET_IMPLEMENTED")
            && stderr.contains("whole-value move of captured generator/closure value `value`")
            && stderr.contains("cannot be moved out of the generator/closure environment"),
        "closure consumption must fail at the captured-resource authority:\n{stderr}"
    );
    assert!(
        !contains_native_artifact(&emit_dir),
        "closure rejection must happen before a native artifact is emitted"
    );
}

#[test]
fn channel_handle_clone_terminals_match_runtime_semantics() {
    assert_exact_runtime("channel_sender_clone", CHANNEL_SENDER_CLONE_BODY, "1\n");
    let sender_mir = raw_mir("channel_sender_clone", CHANNEL_SENDER_CLONE_BODY);
    assert!(
        sender_mir.contains("call hew_vec_clone_owned(")
            && sender_mir.contains("call hew_vec_push_owned_move(")
            && sender_mir.lines().any(|line| {
                line.contains(" tx ")
                    && line.contains("ty=Sender<Token>")
                    && line.contains("intent=Consume")
            })
            && !sender_mir.contains("call hew_vec_clone_layout(")
            && !sender_mir.contains("call hew_vec_push_ptr("),
        "Vec<Sender<AffineT>> must consume its source into, and clone through, the thunk-bearing owned descriptor lane:\n{sender_mir}"
    );

    let dir = tempfile::Builder::new()
        .prefix("affine-resource-channel-receiver-")
        .tempdir()
        .expect("tempdir");
    let input = dir.path().join("receiver_clone.hew");
    std::fs::write(&input, source(CHANNEL_RECEIVER_CLONE_BODY))
        .expect("write receiver clone fixture");
    let output = Command::new(hew_binary())
        .arg("compile")
        .arg(&input)
        .current_dir(repo_root())
        .output()
        .expect("invoke receiver clone compile");
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        !output.status.success()
            && stderr.contains("Vec<std.channel.Receiver<Token>>")
            && stderr.contains("cannot be cloned")
            && stderr.contains("affine close contract"),
        "Receiver has no dup helper and must be rejected by the shared affine-clone authority:\n{stderr}"
    );
}

#[test]
fn receiver_vec_move_is_descriptor_owned_and_read_copy_surfaces_reject() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("affine-resource-channel-receiver-move-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &source(CHANNEL_RECEIVER_MOVE_BODY),
        dir.path(),
        "receiver_move",
    );
    let output = Command::new(&bin)
        .current_dir(repo_root())
        .output()
        .expect("run receiver Vec move fixture");
    assert!(
        output.status.success(),
        "drop-only Receiver Vec move must run cleanly:\n{}",
        describe_output(&output)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "1\n");

    let mir = raw_mir("receiver_move", CHANNEL_RECEIVER_MOVE_BODY);
    assert!(
        mir.contains("call hew_vec_push_owned_move(")
            && mir.lines().any(|line| {
                line.contains(" rx ")
                    && line.contains("ty=Receiver<Token>")
                    && line.contains("intent=Consume")
            })
            && !mir.contains("call hew_vec_push_ptr("),
        "array construction must consume Receiver into the owned move lane:\n{mir}"
    );

    let ir = std::fs::read_to_string(dir.path().join("receiver_move.ll"))
        .expect("read drop-only Receiver Vec LLVM IR");
    assert!(
        ir.contains("@__hew_vec_elem_layout_channel_receiver_drop_only")
            && ir.contains("call ptr @hew_vec_new_with_elem_layout")
            && ir.contains("call void @hew_vec_push_owned_move")
            && ir.contains("call void @hew_vec_free_owned")
            && ir.contains("define internal void @__hew_vec_channel_receiver_drop_inplace")
            && ir.contains("call void @hew_channel_receiver_close")
            && ir.contains("ptr null, ptr @__hew_vec_channel_receiver_drop_inplace"),
        "Receiver Vec must emit clone-null descriptor, move ingress, and one close-on-free wrapper:\n{ir}"
    );

    for (name, body) in [
        ("receiver_get", CHANNEL_RECEIVER_GET_BODY),
        ("receiver_index", CHANNEL_RECEIVER_INDEX_BODY),
        ("receiver_slice", CHANNEL_RECEIVER_SLICE_BODY),
        ("receiver_iter", CHANNEL_RECEIVER_ITER_BODY),
        ("receiver_copy_push", CHANNEL_RECEIVER_COPY_PUSH_BODY),
        ("receiver_copy_set", CHANNEL_RECEIVER_COPY_SET_BODY),
    ] {
        let stderr = compile_rejected(name, body);
        assert!(
            (stderr.contains("drop-only")
                || stderr.contains("affine close contract")
                || stderr.contains("opaque/resource handle")
                || stderr.contains("owned handle"))
                && (stderr.contains("semantic clone")
                    || stderr.contains("clone an affine value")
                    || stderr.contains("cannot be cloned")
                    || stderr.contains("owned handle")),
            "{name} must reject the copy/read surface through the affine Receiver contract:\n{stderr}"
        );
    }
}

fn contains_native_artifact(dir: &Path) -> bool {
    dir.is_dir()
        && std::fs::read_dir(dir)
            .expect("read emit dir")
            .flatten()
            .any(|entry| entry.path().is_file())
}

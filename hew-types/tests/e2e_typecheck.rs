use std::fs;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use hew_types::error::TypeErrorKind;

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf()
}

fn new_networking_demo_checker() -> hew_types::Checker {
    hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![
        repo_root(),
    ]))
}

/// Typecheck an inline source string with full stdlib access.
fn typecheck_inline(source: &str) -> hew_types::TypeCheckOutput {
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "should parse cleanly, got: {:#?}",
        parse_result.errors
    );
    let mut checker = new_networking_demo_checker();
    checker.check_program(&parse_result.program)
}

#[test]
fn typecheck_all_examples() {
    let examples_dir = repo_root().join("examples");
    test_directory(&examples_dir, "examples");
}

#[test]
fn typecheck_all_codegen_examples() {
    let dir = repo_root()
        .join("hew-codegen")
        .join("tests")
        .join("examples");
    test_directory(&dir, "hew-codegen/tests/examples");
}

#[test]
fn typecheck_top_level_networking_demos() {
    for relative in ["examples/http_server.hew", "examples/static_server.hew"] {
        assert_typechecks(&repo_root().join(relative), relative);
    }
}

fn assert_typechecks(path: &Path, label: &str) {
    let source = fs::read_to_string(path).unwrap();
    let parse_result = hew_parser::parse(&source);
    assert!(
        parse_result.errors.is_empty(),
        "{label} should parse cleanly, got: {:#?}",
        parse_result.errors
    );

    let mut checker = new_networking_demo_checker();
    let output = checker.check_program(&parse_result.program);
    assert!(
        output.errors.is_empty(),
        "{label} should type-check cleanly, got: {:#?}",
        output.errors
    );
}

fn assert_inline_typechecks_cleanly(source: &str, context: &str) {
    let output = typecheck_inline(source);
    assert!(
        output.errors.is_empty(),
        "{context}: expected clean typecheck, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashmap_remove_typechecks_as_bool() {
    assert_inline_typechecks_cleanly(
        r#"
fn main() {
    let m: HashMap<String, int> = HashMap::new();
    m.insert("a", 1);
    let removed: bool = m.remove("a");
    let missing: bool = m.remove("a");
    if removed && !missing {
        println("ok");
    }
}
"#,
        "HashMap.remove should typecheck as bool",
    );
}

#[test]
fn hashmap_remove_no_longer_typechecks_as_option() {
    let output = typecheck_inline(
        r#"
fn main() {
    let m: HashMap<String, int> = HashMap::new();
    m.insert("a", 1);
    let removed: Option<int> = m.remove("a");
}
"#,
    );
    assert!(
        output.errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrorKind::Mismatch { expected, actual }
                if expected == "Option<int>" && actual == "bool"
        )),
        "expected HashMap.remove Option<int>/bool mismatch, got: {:#?}",
        output.errors
    );
}

#[test]
fn method_call_receiver_kinds_record_named_type_instance_dispatch() {
    let output = typecheck_inline(
        r"
type Widget {
    value: int;
}

impl Widget {
    fn value_plus_one(w: Widget) -> int {
        w.value + 1
    }
}

fn use_widget(w: Widget) -> int {
    w.value_plus_one()
}
",
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
    assert!(
        output
            .method_call_receiver_kinds
            .values()
            .any(|kind| matches!(
                kind,
                hew_types::MethodCallReceiverKind::NamedTypeInstance { type_name }
                    if type_name == "Widget"
            )),
        "expected named-type method call receiver metadata, got: {:?}",
        output.method_call_receiver_kinds
    );
}

#[test]
fn method_call_receiver_kinds_record_trait_object_dispatch() {
    let output = typecheck_inline(
        r"
trait Greeter {
    fn greet(g: Self) -> String;
}

type Bot {
    name: String;
}

impl Greeter for Bot {
    fn greet(bot: Bot) -> String {
        bot.name
    }
}

fn use_greeter(g: dyn Greeter) -> String {
    g.greet()
}
",
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
    assert!(
        output
            .method_call_receiver_kinds
            .values()
            .any(|kind| matches!(
                kind,
                hew_types::MethodCallReceiverKind::TraitObject { trait_name }
                    if trait_name == "Greeter"
            )),
        "expected trait-object method call receiver metadata, got: {:?}",
        output.method_call_receiver_kinds
    );
}

#[test]
fn assign_target_kinds_record_assignment_target_authority() {
    let output = typecheck_inline(
        r"
type Boxed {
    value: int;
}

fn mutate() {
    var local = 0;
    local = 1;

    var boxed = Boxed { value: 0 };
    boxed.value = 2;

    var nums = [1, 2];
    nums[0] = 3;
}

actor Counter {
    let total: int;

    receive fn set(v: int) {
        total = v;
    }
}
",
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
    assert_eq!(
        output.assign_target_kinds.len(),
        4,
        "expected four classified assignment targets, got: {:?}",
        output.assign_target_kinds
    );
    assert!(
        output
            .assign_target_kinds
            .values()
            .any(|kind| *kind == hew_types::check::AssignTargetKind::LocalVar),
        "expected local-variable assignment metadata, got: {:?}",
        output.assign_target_kinds
    );
    assert!(
        output
            .assign_target_kinds
            .values()
            .any(|kind| *kind == hew_types::check::AssignTargetKind::ActorField),
        "expected actor-field assignment metadata, got: {:?}",
        output.assign_target_kinds
    );
    assert!(
        output
            .assign_target_kinds
            .values()
            .any(|kind| *kind == hew_types::check::AssignTargetKind::FieldAccess),
        "expected field-access assignment metadata, got: {:?}",
        output.assign_target_kinds
    );
    assert!(
        output
            .assign_target_kinds
            .values()
            .any(|kind| *kind == hew_types::check::AssignTargetKind::Index),
        "expected indexed assignment metadata, got: {:?}",
        output.assign_target_kinds
    );
}

#[test]
fn assign_target_shapes_record_type_signedness() {
    let output = typecheck_inline(
        r"
fn f() {
    var a: u8 = 0;
    a = 1;

    var b: i32 = 0;
    b = 1;

    var c: u64 = 0;
    c = 2;

    var d: int = 0;
    d = 3;
}
",
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
    assert_eq!(
        output.assign_target_shapes.len(),
        4,
        "expected four shape entries, got: {:?}",
        output.assign_target_shapes
    );
    // u8 and u64 should be marked unsigned.
    let unsigned_count = output
        .assign_target_shapes
        .values()
        .filter(|s| s.is_unsigned)
        .count();
    assert_eq!(
        unsigned_count, 2,
        "expected 2 unsigned targets (u8, u64), got: {:?}",
        output.assign_target_shapes
    );
    // i32 and int should be marked signed.
    let signed_count = output
        .assign_target_shapes
        .values()
        .filter(|s| !s.is_unsigned)
        .count();
    assert_eq!(
        signed_count, 2,
        "expected 2 signed targets (i32, int), got: {:?}",
        output.assign_target_shapes
    );
}

#[test]
fn assign_target_shapes_accompanies_kinds_for_every_accepted_target() {
    // Verify that every span present in assign_target_kinds also has a
    // corresponding entry in assign_target_shapes.
    let output = typecheck_inline(
        r"
type Boxed {
    value: int;
}

fn mutate() {
    var local = 0;
    local = 1;

    var boxed = Boxed { value: 0 };
    boxed.value = 2;

    var nums = [1, 2];
    nums[0] = 3;
}

actor Counter {
    let total: int;

    receive fn set(v: int) {
        total = v;
    }
}
",
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
    // Every span in assign_target_kinds must have a shape entry.
    for key in output.assign_target_kinds.keys() {
        assert!(
            output.assign_target_shapes.contains_key(key),
            "assign_target_kinds span {key:?} is missing from assign_target_shapes",
        );
    }
    // Every span in assign_target_shapes must have a kind entry.
    for key in output.assign_target_shapes.keys() {
        assert!(
            output.assign_target_kinds.contains_key(key),
            "assign_target_shapes span {key:?} is missing from assign_target_kinds",
        );
    }
}

fn test_directory(dir: &Path, label: &str) {
    let mut parse_ok = 0;
    let mut parse_fail = 0;
    let mut tc_ok = 0;
    let mut tc_fail = 0;
    let mut tc_errors = Vec::new();

    let mut entries: Vec<_> = fs::read_dir(dir)
        .unwrap()
        .filter_map(Result::ok)
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|e| e == "hew"))
        .collect();
    entries.sort();

    for path in &entries {
        let source = fs::read_to_string(path).unwrap();
        let parse_result = hew_parser::parse(&source);
        if !parse_result.errors.is_empty() {
            parse_fail += 1;
            continue;
        }
        parse_ok += 1;

        let mut checker =
            hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let output = checker.check_program(&parse_result.program);
        if output.errors.is_empty() {
            tc_ok += 1;
        } else {
            tc_fail += 1;
            let first_err = &output.errors[0];
            tc_errors.push(format!(
                "  {} — {:?}",
                path.file_name().unwrap().to_string_lossy(),
                first_err
            ));
        }
    }

    let total = parse_ok + parse_fail;
    println!(
        "\n{label}: {parse_ok}/{total} parsed, {tc_ok}/{parse_ok} type-checked ({tc_fail} failed)"
    );
    if !tc_errors.is_empty() {
        println!("Type-check failures:");
        for e in &tc_errors {
            println!("{e}");
        }
    }
    // Informational — don't fail on type-check errors yet
}

// ===========================================================================
// Canonical qualified-spelling end-to-end tests
//
// These tests prove that the literal spellings `stream.Sink` and
// `channel.Receiver` are valid type annotations in Hew source.  The
// resolution path is: `canonical_named_builtin()` strips the module
// prefix and normalises to the short canonical name ("Sink", "Receiver"),
// then the checker's built-in method table handles method calls.  The
// `import` statements are required by the parser for qualified names to
// be accepted but are decorative here — type resolution and method
// dispatch are fully hardcoded and do not go through the stdlib module
// registry.  `typecheck_inline` (full stdlib access) is used so the
// imports are satisfied and no spurious "unknown module" errors occur.
// ===========================================================================

#[test]
fn stream_dot_sink_annotation_typechecks() {
    // A function whose parameter is explicitly spelled `stream.Sink<String>`.
    // Proves: the qualified spelling resolves to the canonical Sink<String>
    // type and its write/close methods are available.
    let output = typecheck_inline(
        r"
        import std::stream;

        fn flush_and_close(s: stream.Sink<String>, msg: String) {
            s.write(msg);
            s.close();
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "stream.Sink<String> annotation should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

#[test]
fn stream_dot_stream_invalid_int_method_reports_user_facing_int() {
    let output = typecheck_inline(
        r"
        import std::stream;

        fn close_numbers(s: stream.Stream<int>) {
            s.close();
        }
        ",
    );
    assert!(
        output.errors.iter().any(|e| {
            e.message.contains("`Stream<int>` is not supported")
                && !e.message.contains("Stream<i64>")
        }),
        "expected Stream<int> diagnostic, got: {:#?}",
        output.errors
    );
}

#[test]
fn stream_decode_fails_closed_before_codegen() {
    let output = typecheck_inline(
        r"
        import std::stream;

        wire type Message {
            id: int @1;
        }

        fn main() {
            let (_sink, input) = stream.bytes_pipe(4);
            let _decoded: stream.Stream<Message> = input.decode();
        }
        ",
    );
    assert!(
        output.errors.iter().any(|e| {
            e.message
                .contains("`decode()` is not available on `Stream<bytes>` yet")
        }),
        "expected explicit stream.decode fail-closed diagnostic, got: {:#?}",
        output.errors
    );
}

#[test]
fn sink_encode_fails_closed_before_codegen() {
    let output = typecheck_inline(
        r"
        import std::stream;

        wire type Message {
            id: int @1;
        }

        fn main() {
            let (sink, _input) = stream.bytes_pipe(4);
            let _encoded: stream.Sink<Message> = sink.encode();
        }
        ",
    );
    assert!(
        output.errors.iter().any(|e| {
            e.message
                .contains("`encode()` is not available on `Sink<bytes>` yet")
        }),
        "expected explicit sink.encode fail-closed diagnostic, got: {:#?}",
        output.errors
    );
}

#[test]
fn channel_dot_receiver_annotation_typechecks() {
    // A function whose parameter is explicitly spelled `channel.Receiver<String>`.
    // Proves: the qualified spelling resolves to the canonical Receiver<String>
    // type and its recv/close methods are available.
    let output = typecheck_inline(
        r"
        import std::channel;

        fn take_one(rx: channel.Receiver<String>) -> Option<String> {
            let v = rx.recv();
            rx.close();
            v
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "channel.Receiver<String> annotation should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

// ===========================================================================
// for-await fail-closed tests
// These cover the typechecker's new is_await validation in Stmt::For.
// ===========================================================================

/// `for await item in rx` over `Receiver<String>` must typecheck cleanly.
#[test]
fn for_await_receiver_string_ok() {
    let output = typecheck_inline(
        r#"
        import std::channel::channel;

        fn main() {
            let (tx, rx) = channel.new(4);
            tx.send("hello");
            tx.close();
            for await msg in rx {
                println(msg);
            }
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "for await over Receiver<String> should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

/// `for await val in rx` over `Receiver<int>` must typecheck cleanly.
#[test]
fn for_await_receiver_int_ok() {
    let output = typecheck_inline(
        r"
        import std::channel::channel;

        fn main() {
            let (tx, rx) = channel.new(4);
            tx.send(42);
            tx.close();
            for await val in rx {
                println(val);
            }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "for await over Receiver<int> should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

/// `for await _ in rx` over a bare `Receiver` annotation must fail closed
/// before serializer-time unresolved-type handling.
#[test]
fn for_await_receiver_missing_element_type_errors() {
    let output = typecheck_inline(
        r"
        import std::channel::channel;

        fn main() {
            let (tx, rx): (channel.Sender, channel.Receiver) = channel.new(4);
            tx.close();
            for await _ in rx {
                println(0);
            }
        }
        ",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("requires a resolved element type")
        ),
        "expected unresolved Receiver<T> for-await error, got: {:#?}",
        output.errors
    );
}

/// `for await item in rx` over `Receiver<Foo>` (unsupported struct) must error.
#[test]
fn for_await_receiver_unsupported_type_errors() {
    let output = typecheck_inline(
        r"
        import std::channel::channel;

        type Foo { x: int }

        fn make_foo() -> Foo { Foo { x: 1 } }

        fn main() {
            let (tx, rx): (channel.Sender<Foo>, channel.Receiver<Foo>) = channel.new(4);
            for await item in rx {
                println(item.x);
            }
        }
        ",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("not supported in `for await`")
        ),
        "expected InvalidOperation for Receiver<Foo> in for await, got: {:#?}",
        output.errors
    );
}

/// `for await item in input` over `Stream<Row>` must reuse the stream element
/// validation boundary instead of lowering through the text ABI.
#[test]
fn for_await_stream_unsupported_type_errors() {
    let output = typecheck_inline(
        r#"
        import std::stream;

        type Row { value: int }

        extern "C" {
            fn fake_stream() -> Stream<Row>;
        }

        fn main() {
            let input = unsafe { fake_stream() };
            for await row in input {
                println("seen");
            }
        }
        "#,
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("`Stream<Row>` is not supported")
        }),
        "expected InvalidOperation for Stream<Row> in for await, got: {:#?}",
        output.errors
    );
}

/// Unsupported first-class `Stream<T>` element types in `for await` must fail
/// closed without cascading into loop-body field/type errors.
#[test]
fn for_await_stream_unsupported_type_does_not_cascade() {
    let output = typecheck_inline(
        r#"
        type Row { value: int }

        extern "C" {
            fn fake_stream() -> Stream<Row>;
        }

        fn main() {
            let input = unsafe { fake_stream() };
            for await row in input {
                println(row.missing);
            }
        }
        "#,
    );
    assert_eq!(
        output.errors.len(),
        1,
        "expected only the fail-closed Stream<Row> error, got: {:#?}",
        output.errors
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("`Stream<Row>` is not supported")
        }),
        "expected InvalidOperation for Stream<Row> in for await, got: {:#?}",
        output.errors
    );
}

/// `for await item in input` over a bare `Stream` annotation must fail closed
/// instead of bypassing stream element validation and lowering as text.
#[test]
fn for_await_stream_missing_element_type_errors() {
    let output = typecheck_inline(
        r#"
        extern "C" { fn make_stream() -> Stream; }

        fn main() {
            let s = unsafe { make_stream() };
            for await x in s {
                println("bypassed!");
            }
        }
        "#,
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("requires a resolved element type")
        }),
        "expected InvalidOperation for bare Stream in for await, got: {:#?}",
        output.errors
    );
}

/// `for await item in actor.receive_gen()` must keep the actor mailbox path and
/// not reuse first-class `Stream<T>` element restrictions.
#[test]
fn for_await_receive_generator_int_stream_typechecks() {
    let output = typecheck_inline(
        r"
        actor Counter {
            receive gen fn count_up() -> int {
                yield 1;
            }
        }

        fn main() {
            let c = spawn Counter();
            for await val in c.count_up() {
                println(val);
            }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "for await over receive gen Stream<int> should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

/// Actor method calls in `for await` must target `receive gen fn`, even if the
/// method's return type is `Stream<T>`.
#[test]
fn for_await_actor_method_stream_requires_receive_gen() {
    let output = typecheck_inline(
        r#"
        extern "C" { fn fake_stream() -> Stream<String>; }

        actor Reader {
            receive fn lines() -> Stream<String> {
                unsafe { fake_stream() }
            }
        }

        fn main() {
            let r = spawn Reader();
            for await line in r.lines() {
                println(line);
            }
        }
        "#,
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("requires a `receive gen fn`")
        }),
        "expected InvalidOperation for actor method Stream<T> without receive gen, got: {:#?}",
        output.errors
    );
}

/// `for await item in vec` must error — Vec is a sync iterable.
#[test]
fn for_await_over_vec_errors() {
    let output = typecheck_inline(
        r"
        fn main() {
            let v: Vec<int> = Vec::new();
            for await item in v {
                println(item);
            }
        }
        ",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("`for await`")
        ),
        "expected InvalidOperation for `for await` over Vec, got: {:#?}",
        output.errors
    );
}

/// `for await i in 0..10` must error — Range is a sync iterable.
#[test]
fn for_await_over_range_errors() {
    let output = typecheck_inline(
        r"
        fn main() {
            for await i in 0..10 {
                println(i);
            }
        }
        ",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("`for await`")
        ),
        "expected InvalidOperation for `for await` over Range, got: {:#?}",
        output.errors
    );
}

/// Plain `for item in rx` (no await) over `Receiver<Foo>` should NOT trigger
/// the for-await guard (a different validation may apply elsewhere).
#[test]
fn for_no_await_over_receiver_no_for_await_error() {
    let output = typecheck_inline(
        r"
        import std::channel;

        fn consume(rx: channel.Receiver<String>) {
            for msg in rx {
                println(msg);
            }
        }
        ",
    );
    // The for-await guard must NOT fire on a plain `for` loop.
    assert!(
        output
            .errors
            .iter()
            .all(|e| !e.message.contains("not supported in `for await`")),
        "for-await guard must not fire on plain `for`, got errors: {:#?}",
        output.errors
    );
}

// ── Rc<T> surface tests ───────────────────────────────────────────────────────

/// Basic `Rc<T>` construction, clone, get, and `strong_count` must type-check clean.
#[test]
fn rc_construction_and_methods_typecheck() {
    let output = typecheck_inline(
        r"
        fn main() {
            let rc: Rc<int> = Rc::new(42);
            println(rc.get());
            let rc2 = rc.clone();
            println(rc2.get());
            println(rc.strong_count());
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "Rc<int> basic usage should type-check cleanly, got: {:#?}",
        output.errors
    );
}

/// `Rc<T>` must be rejected when sent across an actor boundary (non-Send).
#[test]
fn rc_rejected_at_actor_send_boundary() {
    let output = typecheck_inline(
        r"
        actor Sink {
            let _unused: int;
            receive fn consume(val: Rc<int>) {}
        }
        fn main() {
            let rc: Rc<int> = Rc::new(1);
            let a = spawn Sink(_unused: 0);
            a.consume(rc);
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::InvalidSend),
        "Rc<int> must be rejected at actor send boundary with InvalidSend, got: {:#?}",
        output.errors
    );
}

#[test]
fn lambda_actor_capture_must_be_send() {
    let output = typecheck_inline(
        r"
        fn main() {
            let rc: Rc<int> = Rc::new(1);
            let worker = spawn move (x: int) => {
                println(rc.strong_count());
                println(x);
            };
            worker.send(1);
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::InvalidSend),
        "spawned lambda actor must reject non-Send captures, got: {:#?}",
        output.errors
    );
}

#[test]
fn lambda_actor_send_method_requires_send_payload() {
    let output = typecheck_inline(
        r"
        fn main() {
            let worker = spawn (msg: Rc<int>) => {
                println(msg.strong_count());
            };
            let rc: Rc<int> = Rc::new(1);
            worker.send(rc);
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::InvalidSend),
        "lambda actor .send() must reject non-Send payloads, got: {:#?}",
        output.errors
    );
}

#[test]
fn actor_ref_send_method_requires_send_payload() {
    let output = typecheck_inline(
        r"
        actor Sink {
            let _unused: int;
        }

        fn main() {
            let sink = spawn Sink(_unused: 0);
            let rc: Rc<int> = Rc::new(1);
            sink.send(rc);
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::InvalidSend),
        "ActorRef.send() must reject non-Send payloads, got: {:#?}",
        output.errors
    );
}

/// `Rc::new` must accept non-Copy `T`; the codegen passes the real drop function.
/// `Rc::get()` must be rejected when `T` is not `Copy` (`LoadOp` semantics).
#[test]
fn rc_non_copy_construction_ok() {
    // String is non-Copy; Rc::new should accept it (codegen will pass a real
    // drop function instead of null).
    let output = typecheck_inline(r#"fn main() { let _rc: Rc<String> = Rc::new("hello"); }"#);
    assert!(
        output.errors.is_empty(),
        "Rc::new with a non-Copy inner type should succeed; got errors: {:#?}",
        output.errors
    );
}

#[test]
fn rc_get_non_copy_rejected() {
    // `rc.get()` performs a bitwise copy (LoadOp) which is only sound for
    // Copy types.  Calling it on Rc<String> must be rejected.
    let output = typecheck_inline(
        r#"fn main() { let rc: Rc<String> = Rc::new("hello"); let _ = rc.get(); }"#,
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::BoundsNotSatisfied),
        "Rc::get on a non-Copy inner type should fail with BoundsNotSatisfied, got: {:#?}",
        output.errors
    );
}

// ── HTTP respond() ergonomics ────────────────────────────────────────────────

/// `req.respond(status, content_type, body)` (3-arg form) must typecheck cleanly.
///
/// Confirms that `content_length` has been removed from the `respond` signature
/// so callers no longer need to supply it.
#[test]
fn http_respond_three_arg_typechecks() {
    let output = typecheck_inline(
        r#"
        import std::net::http;

        fn main() {
            let server = http.listen(":8080");
            let req = server.accept();
            req.respond(200, "text/plain", "Hello, Hew!");
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "http respond(status, content_type, body) should typecheck without errors, got: {:#?}",
        output.errors
    );
}

/// The old 4-arg form `req.respond(status, content_type, content_length, body)`
/// must now produce a type error, confirming that the removed parameter is no
/// longer accepted.
#[test]
fn http_respond_four_arg_rejected() {
    let output = typecheck_inline(
        r#"
        import std::net::http;

        fn main() {
            let server = http.listen(":8080");
            let req = server.accept();
            req.respond(200, "text/plain", 11, "Hello, Hew!");
        }
        "#,
    );
    assert!(
        !output.errors.is_empty(),
        "http respond with 4 args (old content_length form) should produce a type error"
    );
}

// ── SMTP send() ergonomics ───────────────────────────────────────────────────

/// `smtp.send(...)` and `smtp.send_html(...)` should typecheck as one-shot
/// convenience wrappers.
#[test]
fn smtp_one_shot_helpers_typecheck() {
    let output = typecheck_inline(
        r#"
        import std::net::smtp;

        fn main() {
            smtp.send(
                "smtp.example.com",
                587,
                "user",
                "pass",
                "from@example.com",
                "to@example.com",
                "Subject",
                "Body",
            );
            smtp.send_html(
                "smtp.example.com",
                587,
                "user",
                "pass",
                "from@example.com",
                "to@example.com",
                "Subject",
                "<h1>Hello</h1>",
            );
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "smtp one-shot helpers should typecheck without errors, got: {:#?}",
        output.errors
    );
}

/// Regression: the CLI injects a synthetic `import std::text::regex` when regex
/// literals appear in source. The type checker must mark that import as used when
/// synthesising the `regex.Pattern` type so no false-positive unused-import
/// warning is emitted.
#[test]
fn regex_literal_no_false_positive_unused_import_warning() {
    let output = typecheck_inline(
        r#"import std::text::regex;
fn main() {
    let pat = re"[0-9]+";
    if "hello123" =~ pat {
        println("match");
    }
}
"#,
    );
    let has_unused_regex = output.warnings.iter().any(|w| {
        w.kind == hew_types::error::TypeErrorKind::UnusedImport && w.message.contains("regex")
    });
    assert!(
        !has_unused_regex,
        "regex literal should mark the implicit `regex` import as used; got warnings: {:#?}",
        output.warnings
    );
}

// ── Range literal inference (PR #628 follow-up) ──────────────────────────────

/// Range bounds recorded with the correct element type when the for-loop body
/// constrains the induction variable to i32.
#[test]
fn for_range_infers_i32_from_body_usage() {
    let output = typecheck_inline(
        r"fn test() {
    var sum: i32 = 0;
    for i in 0..10 {
        sum = sum + i;
    }
}
",
    );
    assert!(
        output.errors.is_empty(),
        "for-range inferred i32 should type-check cleanly: {:#?}",
        output.errors
    );
    // Verify bound spans are recorded as I32, not I64, so codegen generates
    // the right constant width.
    let i32_count = output
        .expr_types
        .values()
        .filter(|ty| **ty == hew_types::Ty::I32)
        .count();
    let i64_bound_count = output
        .expr_types
        .values()
        .filter(|ty| **ty == hew_types::Ty::I64)
        .count();
    // The loop body constrains the range to i32; there should be no spurious
    // i64 entries for the bound literals (they should have been re-recorded).
    assert!(
        i32_count >= 2,
        "expected at least 2 i32 entries for range bounds `0` and `10`, got {i32_count}"
    );
    assert_eq!(
        i64_bound_count, 0,
        "expected no i64 entries after i32 inference, got {i64_bound_count}"
    );
}

/// Range literal passed to a function expecting `Range<i32>` should check
/// cleanly — the bounds are coerced to i32.
#[test]
fn range_literal_check_against_range_i32() {
    let output = typecheck_inline(
        r"fn consume(_r: Range<i32>) {}
fn test() {
    consume(0..10);
}
",
    );
    assert!(
        output.errors.is_empty(),
        "range literal passed to Range<i32> parameter should not error: {:#?}",
        output.errors
    );
}

/// Unconstrained range still defaults to i64.
#[test]
fn for_range_unconstrained_defaults_to_i64() {
    let output = typecheck_inline(
        r"fn test() {
    for i in 0..10 {
        let _: i64 = i;
    }
}
",
    );
    assert!(
        output.errors.is_empty(),
        "unconstrained range should default to i64: {:#?}",
        output.errors
    );
}

/// Range bound that doesn't fit in the inferred element type should error.
#[test]
fn for_range_bound_overflow_errors() {
    let output = typecheck_inline(
        r"fn test() {
    for i in 0..300 {
        let _: i8 = i;
    }
}
",
    );
    assert!(
        !output.errors.is_empty(),
        "range bound 300 does not fit in i8 — should produce a type error"
    );
}

/// `let r: Range<i32> = 0..10` should work without errors.
#[test]
fn range_literal_assigned_to_range_i32() {
    let output = typecheck_inline(
        r"fn test() {
    let r: Range<i32> = 0..10;
    let _: i32 = r.start;
}
",
    );
    assert!(
        output.errors.is_empty(),
        "Range<i32> assignment should type-check cleanly: {:#?}",
        output.errors
    );
}

/// Returning an Rc parameter as a bare identifier must be a fail-closed error
/// under borrow-on-call semantics (double-free at runtime).
#[test]
fn rc_param_return_errors_borrowed_rc() {
    // Trailing expression (implicit return) — bare identifier
    let output = typecheck_inline(
        r"
        fn identity(r: Rc<int>) -> Rc<int> {
            r
        }
        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn),
        "returning Rc param as trailing expr should emit BorrowedParamReturn error, got errors: {:#?}, warnings: {:#?}",
        output.errors, output.warnings
    );
}

/// Explicit `return rc_param` must also trigger the error.
#[test]
fn rc_param_explicit_return_errors_borrowed_rc() {
    let output = typecheck_inline(
        r"
        fn early(r: Rc<int>, flag: bool) -> Rc<int> {
            if flag {
                return r;
            }
            Rc::new(0)
        }
        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn),
        "explicit return of Rc param should emit BorrowedParamReturn error, got errors: {:#?}, warnings: {:#?}",
        output.errors, output.warnings
    );
}

/// `break <rc_param>` inside a loop must trigger the error — the broken value
/// escapes to the enclosing scope with the same aliasing hazard as `return`.
#[test]
fn rc_param_break_value_errors_borrowed_rc() {
    let output = typecheck_inline(
        r"
        fn escape(r: Rc<int>) -> Rc<int> {
            loop {
                break r;
            }
        }
        fn main() {}
        ",
    );
    // Note: the type checker currently types `loop { break v; }` as Unit,
    // so this also gets a ReturnTypeMismatch.  The BorrowedParamReturn error
    // must still fire independently of that.
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn),
        "break-with-value of Rc param should emit BorrowedParamReturn error, got errors: {:#?}",
        output.errors
    );
}

/// Block-wrapped return `{ r }` must be caught — `Expr::Block` descent.
#[test]
fn rc_param_block_wrapped_return_errors_borrowed_rc() {
    let output = typecheck_inline(
        r"
        fn wrapped(r: Rc<int>) -> Rc<int> {
            { r }
        }
        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn),
        "block-wrapped Rc param return should emit BorrowedParamReturn error, got errors: {:#?}",
        output.errors
    );
}

/// Returning `rc_param.clone()` should NOT trigger the error — it creates
/// an owned copy with an incremented refcount.
#[test]
fn rc_param_clone_return_no_error() {
    let output = typecheck_inline(
        r"
        fn safe_identity(r: Rc<int>) -> Rc<int> {
            r.clone()
        }
        fn main() {}
        ",
    );
    let rc_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert!(
        rc_errors.is_empty(),
        "returning rc.clone() should not error, got: {rc_errors:#?}",
    );
}

/// Passing an Rc to a function that reads it (borrow) should be clean — no
/// `BorrowedParamReturn` errors.
#[test]
fn rc_pass_to_fn_borrow_clean() {
    let output = typecheck_inline(
        r"
        fn read_rc(r: Rc<int>) -> int {
            r.get()
        }
        fn main() {
            let rc = Rc::new(42);
            println(read_rc(rc));
        }
        ",
    );
    let rc_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert!(
        rc_errors.is_empty(),
        "Rc borrow (read-only callee) should not emit BorrowedParamReturn, got: {rc_errors:#?}",
    );
}

// ── Aggregate escape tests ────────────────────────────────────────────────────

/// Wrapping an Rc param in `Some(r)` is an aggregate escape — the borrowed
/// pointer is embedded in the Option and escapes the function.
#[test]
fn rc_param_some_wrap_errors_borrowed_rc() {
    let output = typecheck_inline(
        r"
        fn wrap(r: Rc<int>) -> Option<Rc<int>> {
            Some(r)
        }
        fn main() {}
        ",
    );
    let rc_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert_eq!(
        rc_errors.len(),
        1,
        "Some(r) should trigger BorrowedParamReturn, got: {rc_errors:#?}",
    );
}

/// Wrapping an Rc param in a tuple `(r, 0)` is an aggregate escape.
#[test]
fn rc_param_tuple_wrap_errors_borrowed_rc() {
    let output = typecheck_inline(
        r"
        fn wrap(r: Rc<int>) -> (Rc<int>, int) {
            (r, 0)
        }
        fn main() {}
        ",
    );
    let rc_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert_eq!(
        rc_errors.len(),
        1,
        "(r, 0) should trigger BorrowedParamReturn, got: {rc_errors:#?}",
    );
}

/// Struct init with an Rc param field is an aggregate escape.
#[test]
fn rc_param_struct_init_errors_borrowed_rc() {
    let output = typecheck_inline(
        r"
        type Holder {
            val: Rc<int>,
        }
        fn wrap(r: Rc<int>) -> Holder {
            Holder { val: r }
        }
        fn main() {}
        ",
    );
    let rc_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert_eq!(
        rc_errors.len(),
        1,
        "Holder {{ val: r }} should trigger BorrowedParamReturn, got: {rc_errors:#?}",
    );
}

/// `Some(r.clone())` is safe — the Rc is cloned before embedding.
#[test]
fn rc_param_some_clone_no_error() {
    let output = typecheck_inline(
        r"
        fn wrap(r: Rc<int>) -> Option<Rc<int>> {
            Some(r.clone())
        }
        fn main() {}
        ",
    );
    let rc_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert!(
        rc_errors.is_empty(),
        "Some(r.clone()) should not error, got: {rc_errors:#?}",
    );
}

/// Returning `Rc::new(val)` should not trigger — it's a fresh allocation.
#[test]
fn rc_new_in_return_no_error() {
    let output = typecheck_inline(
        r"
        fn fresh(_r: Rc<int>) -> Rc<int> {
            Rc::new(0)
        }
        fn main() {}
        ",
    );
    let rc_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert!(
        rc_errors.is_empty(),
        "Rc::new(0) should not error, got: {rc_errors:#?}",
    );
}

/// Returning `return Some(r)` via explicit return also triggers the diagnostic.
#[test]
fn rc_param_explicit_return_some_errors_borrowed_rc() {
    let output = typecheck_inline(
        r"
        fn wrap(r: Rc<int>) -> Option<Rc<int>> {
            return Some(r);
        }
        fn main() {}
        ",
    );
    let rc_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert_eq!(
        rc_errors.len(),
        1,
        "return Some(r) should trigger BorrowedParamReturn, got: {rc_errors:#?}",
    );
}

/// Passing an Rc param to a regular (lowercase) function call that returns
/// a non-Rc type should NOT trigger — regular calls are borrows, not escapes.
#[test]
fn rc_param_passed_to_regular_fn_no_error() {
    let output = typecheck_inline(
        r"
        fn extract(r: Rc<int>) -> int {
            r.get()
        }
        fn delegate(r: Rc<int>) -> int {
            extract(r)
        }
        fn main() {}
        ",
    );
    let rc_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert!(
        rc_errors.is_empty(),
        "passing Rc param to a regular function should not error, got: {rc_errors:#?}",
    );
}

// ── Collection-store / taint-tracking tests ─────────────────────────────────

/// `v.push(r); v` — Rc param stored in collection via method call then returned.
#[test]
fn rc_method_call_store_and_return_errors() {
    let output = typecheck_inline(
        r"
        fn bad(r: Rc<int>) -> Vec<Rc<int>> {
            var v = Vec::new();
            v.push(r);
            v
        }
        fn main() {}
        ",
    );
    let rc_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert_eq!(
        rc_errors.len(),
        1,
        "returning v after v.push(r) must fire BorrowedParamReturn, got: {rc_errors:#?}",
    );
    assert!(
        rc_errors[0].message.contains("`v`") && rc_errors[0].message.contains("`r`"),
        "error should mention both `v` and `r`, got: {}",
        rc_errors[0].message
    );
}

/// `v.push(r); return v;` — explicit return of tainted local.
#[test]
fn rc_method_call_store_explicit_return_errors() {
    let output = typecheck_inline(
        r"
        fn bad(r: Rc<int>) -> Vec<Rc<int>> {
            var v = Vec::new();
            v.push(r);
            return v;
        }
        fn main() {}
        ",
    );
    let rc_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert_eq!(
        rc_errors.len(),
        1,
        "explicit `return v` after v.push(r) must fire, got: {rc_errors:#?}",
    );
}

/// `let v = r; v` — direct alias of Rc param then returned.
#[test]
fn rc_direct_alias_return_errors() {
    let output = typecheck_inline(
        r"
        fn bad(r: Rc<int>) -> Rc<int> {
            let v = r;
            v
        }
        fn main() {}
        ",
    );
    let rc_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert!(
        !rc_errors.is_empty(),
        "returning alias `v = r` must fire BorrowedParamReturn, got: {:#?}",
        output.errors
    );
}

/// `let v = Some(r); v` — aggregate alias then returned.
#[test]
fn rc_aggregate_alias_return_errors() {
    let output = typecheck_inline(
        r"
        fn bad(r: Rc<int>) -> Option<Rc<int>> {
            let v = Some(r);
            v
        }
        fn main() {}
        ",
    );
    let rc_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert!(
        !rc_errors.is_empty(),
        "returning alias `v = Some(r)` must fire BorrowedParamReturn, got: {:#?}",
        output.errors
    );
}

/// Transitive taint: `let a = r; v.push(a); v` — taint propagates through alias.
#[test]
fn rc_transitive_taint_errors() {
    let output = typecheck_inline(
        r"
        fn bad(r: Rc<int>) -> Vec<Rc<int>> {
            let a = r;
            var v = Vec::new();
            v.push(a);
            v
        }
        fn main() {}
        ",
    );
    let rc_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert!(
        !rc_errors.is_empty(),
        "transitive taint (a = r; v.push(a); v) must fire, got: {:#?}",
        output.errors
    );
}

/// v.push(r) where v is NOT returned — no error (only return-path escapes
/// are flagged by this check; drop-ordering issues are separate).
#[test]
fn rc_method_call_store_no_return_no_error() {
    let output = typecheck_inline(
        r"
        fn not_returned(r: Rc<int>) -> int {
            var v = Vec::new();
            v.push(r);
            42
        }
        fn main() {}
        ",
    );
    let rc_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert!(
        rc_errors.is_empty(),
        "v.push(r) without returning v should not fire, got: {rc_errors:#?}",
    );
}

/// v.push(r.clone()) then return v — safe because clone increments refcount.
#[test]
fn rc_method_call_store_clone_no_error() {
    let output = typecheck_inline(
        r"
        fn safe(r: Rc<int>) -> Vec<Rc<int>> {
            var v = Vec::new();
            v.push(r.clone());
            v
        }
        fn main() {}
        ",
    );
    let rc_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert!(
        rc_errors.is_empty(),
        "v.push(r.clone()) then return v should be clean, got: {rc_errors:#?}",
    );
}

/// Taint inside an if-branch is propagated unconditionally (fail-closed).
#[test]
fn rc_method_call_store_in_branch_errors() {
    let output = typecheck_inline(
        r"
        fn bad(r: Rc<int>, cond: bool) -> Vec<Rc<int>> {
            var v = Vec::new();
            if cond {
                v.push(r);
            }
            v
        }
        fn main() {}
        ",
    );
    let rc_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert!(
        !rc_errors.is_empty(),
        "v.push(r) inside branch then return v must fire, got: {:#?}",
        output.errors
    );
}

/// Assignment-based taint: `v = r;` then return v.
#[test]
fn rc_assignment_taint_return_errors() {
    let output = typecheck_inline(
        r"
        fn bad(r: Rc<int>) -> Rc<int> {
            var v: Rc<int> = Rc::new(0);
            v = r;
            v
        }
        fn main() {}
        ",
    );
    let rc_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert!(
        !rc_errors.is_empty(),
        "`v = r;` then return v must fire BorrowedParamReturn, got: {:#?}",
        output.errors
    );
}

/// Read-only method `contains(r)` must NOT taint the receiver.
/// Under borrow-on-call, the method borrows `r` and returns independently.
#[test]
fn rc_readonly_method_does_not_taint() {
    let output = typecheck_inline(
        r"
        fn ok(r: Rc<int>) -> Vec<int> {
            let v = Vec::new();
            v.contains(r.get());
            v
        }
        fn main() {}
        ",
    );
    let rc_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert!(
        rc_errors.is_empty(),
        "v.contains(...) is read-only and must not taint v, got: {rc_errors:#?}",
    );
}

/// Field-assignment escape: `s.field = r; return s;` stores a borrowed
/// param into a struct field, then returns the struct — aliasing double-free.
#[test]
fn rc_field_assignment_escape_errors() {
    let output = typecheck_inline(
        r"
        type Wrapper {
            value: Rc<int>,
        }
        fn bad(r: Rc<int>) -> Wrapper {
            var s = Wrapper { value: Rc::new(0) };
            s.value = r;
            s
        }
        fn main() {}
        ",
    );
    let rc_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert!(
        !rc_errors.is_empty(),
        "s.value = r; return s must fire BorrowedParamReturn, got: {:#?}",
        output.errors
    );
}

// ═══════════════════════════════════════════════════════════════════════════════
//  Unsupported Vec array elements
// ═══════════════════════════════════════════════════════════════════════════════

fn assert_invalid_operation_contains(source: &str, needle: &str, context: &str) {
    let output = typecheck_inline(source);
    let hits: Vec<_> = output
        .errors
        .iter()
        .filter(|e| {
            e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains(needle)
        })
        .collect();
    assert!(
        !hits.is_empty(),
        "{context}: expected InvalidOperation containing `{needle}`, got: {:#?}",
        output.errors
    );
}

#[test]
fn vec_array_annotation_rejected() {
    assert_invalid_operation_contains(
        r"
        fn main() {
            let v: Vec<[int; 2]> = Vec::new();
            println(v.len());
        }",
        "Vec<[int; 2]> is not supported",
        "annotated Vec<[int; 2]>",
    );
}

#[test]
fn vec_from_array_elements_rejected() {
    assert_invalid_operation_contains(
        r"
        fn main() {
            let v = Vec::from([[1, 2]]);
            println(v.len());
        }",
        "Vec<[int; 2]> is not supported",
        "Vec::from([[1, 2]])",
    );
}

#[test]
fn vec_nested_vec_array_annotation_rejected() {
    assert_invalid_operation_contains(
        r"
        fn main() {
            let v: Vec<Vec<[int; 2]>> = Vec::new();
            println(v.len());
        }",
        "Vec<[int; 2]> is not supported",
        "annotated Vec<Vec<[int; 2]>>",
    );
}

#[test]
fn vec_tuple_with_array_elements_rejected() {
    assert_invalid_operation_contains(
        r"
        fn main() {
            let v = Vec::new();
            v.push((1, [2, 3]));
            println(v.len());
        }",
        "Vec<(int, [int; 2])> is not supported",
        "Vec tuple element with nested array",
    );
}

#[test]
fn vec_generic_wrapper_array_annotation_rejected() {
    assert_invalid_operation_contains(
        r"
        type Box<T> {
            value: T,
        }

        fn main() {
            let v: Vec<Box<[int; 2]>> = Vec::new();
            println(v.len());
        }",
        "Vec<Box<[int; 2]>> is not supported",
        "annotated Vec<Box<[int; 2]>>",
    );
}

#[test]
fn vec_option_wrapper_array_annotation_rejected() {
    assert_invalid_operation_contains(
        r"
        fn main() {
            let v: Vec<Option<[int; 2]>> = Vec::new();
            println(v.len());
        }",
        "Vec<Option<[int; 2]>> is not supported",
        "annotated Vec<Option<[int; 2]>>",
    );
}

#[test]
fn vec_result_wrapper_array_annotation_rejected() {
    assert_invalid_operation_contains(
        r"
        fn main() {
            let v: Vec<Result<[int; 2], String>> = Vec::new();
            println(v.len());
        }",
        "Vec<Result<[int; 2], String>> is not supported",
        "annotated Vec<Result<[int; 2], String>>",
    );
}

// ═══════════════════════════════════════════════════════════════════════════════
//  UnsafeCollectionElement — Rc<T> in collections
// ═══════════════════════════════════════════════════════════════════════════════

/// Helper: assert that source produces at least one `UnsafeCollectionElement` error.
fn assert_unsafe_collection_element(source: &str, context: &str) {
    let output = typecheck_inline(source);
    let hits: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::UnsafeCollectionElement)
        .collect();
    assert!(
        !hits.is_empty(),
        "{context}: expected UnsafeCollectionElement error, got: {:#?}",
        output.errors
    );
}

/// Helper: assert that source produces NO `UnsafeCollectionElement` error.
fn assert_no_unsafe_collection_element(source: &str, context: &str) {
    let output = typecheck_inline(source);
    let hits: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::UnsafeCollectionElement)
        .collect();
    assert!(
        hits.is_empty(),
        "{context}: unexpected UnsafeCollectionElement error(s): {hits:#?}"
    );
}

#[test]
fn rc_vec_push_rejected() {
    assert_unsafe_collection_element(
        r"
        fn main() {
            let r = Rc::new(42);
            var v = Vec::new();
            v.push(r);
        }",
        "Vec.push(Rc<int>)",
    );
}

#[test]
fn rc_vec_set_rejected() {
    assert_unsafe_collection_element(
        r"
        fn main() {
            var v: Vec<Rc<int>> = Vec::new();
            let r = Rc::new(99);
            v.set(0, r);
        }",
        "Vec.set(_, Rc<int>)",
    );
}

#[test]
fn rc_vec_extend_rejected() {
    assert_unsafe_collection_element(
        r"
        fn main() {
            var v: Vec<Rc<int>> = Vec::new();
            let w: Vec<Rc<int>> = Vec::new();
            v.extend(w);
        }",
        "Vec.extend(Vec<Rc<int>>)",
    );
}

#[test]
fn rc_vec_pop_rejected() {
    assert_unsafe_collection_element(
        r"
        type Holder { v: Vec<Rc<int>> }
        fn extract(h: Holder) -> Rc<int> {
            h.v.pop()
        }",
        "Vec.pop() on Vec<Rc<int>>",
    );
}

#[test]
fn rc_vec_get_rejected() {
    assert_unsafe_collection_element(
        r"
        type Holder { v: Vec<Rc<int>> }
        fn extract(h: Holder) -> Rc<int> {
            h.v.get(0)
        }",
        "Vec.get(_) on Vec<Rc<int>>",
    );
}

#[test]
fn rc_vec_remove_rejected() {
    assert_unsafe_collection_element(
        r"
        type Holder { v: Vec<Rc<int>> }
        fn extract(h: Holder) -> Rc<int> {
            h.v.remove(0)
        }",
        "Vec.remove(_) on Vec<Rc<int>>",
    );
}

#[test]
fn rc_hashmap_insert_value_rejected() {
    assert_unsafe_collection_element(
        r#"
        fn main() {
            var m = HashMap::new();
            let r = Rc::new(42);
            m.insert("key", r);
        }"#,
        "HashMap.insert(_, Rc<int>)",
    );
}

#[test]
fn rc_hashmap_insert_key_rejected() {
    assert_unsafe_collection_element(
        r#"
        fn main() {
            var m = HashMap::new();
            let r = Rc::new(42);
            m.insert(r, "val");
        }"#,
        "HashMap.insert(Rc<int>, _)",
    );
}

#[test]
fn rc_hashset_insert_rejected() {
    assert_unsafe_collection_element(
        r"
        fn main() {
            var s = HashSet::new();
            let r = Rc::new(42);
            s.insert(r);
        }",
        "HashSet.insert(Rc<int>)",
    );
}

#[test]
fn rc_nested_in_vec_element_rejected() {
    assert_unsafe_collection_element(
        r"
        fn main() {
            var v = Vec::new();
            let r = Rc::new(42);
            v.push(Some(r));
        }",
        "Vec.push(Option<Rc<int>>)",
    );
}

#[test]
fn rc_tuple_in_vec_element_rejected() {
    assert_unsafe_collection_element(
        r"
        fn main() {
            var v = Vec::new();
            let r = Rc::new(42);
            v.push((r, 0));
        }",
        "Vec.push((Rc<int>, int))",
    );
}

// ── Safe patterns: collections with Copy / primitive types ──────────────

#[test]
fn vec_int_push_ok() {
    assert_no_unsafe_collection_element(
        r"
        fn main() {
            var v = Vec::new();
            v.push(42);
        }",
        "Vec<int> push should be fine",
    );
}

#[test]
fn vec_string_push_ok() {
    assert_no_unsafe_collection_element(
        r#"
        fn main() {
            var v = Vec::new();
            v.push("hello");
        }"#,
        "Vec<String> push should be fine",
    );
}

#[test]
fn hashmap_string_string_insert_ok() {
    assert_no_unsafe_collection_element(
        r#"
        fn main() {
            var m = HashMap::new();
            m.insert("key", "value");
        }"#,
        "HashMap<String, String> insert should be fine",
    );
}

#[test]
fn hashset_int_insert_ok() {
    assert_no_unsafe_collection_element(
        r"
        fn main() {
            var s = HashSet::new();
            s.insert(42);
        }",
        "HashSet<int> insert should be fine",
    );
}

#[test]
fn slice_param_annotation_rejected_before_codegen() {
    let output = typecheck_inline(
        r"
        fn take(xs: [i32]) {}

        fn main() {}",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("slice annotations are not supported")
        ),
        "expected slice parameter annotation to fail before lowering, got: {:#?}",
        output.errors
    );
}

#[test]
fn loader_registered_module_slice_signature_rejected_before_registration() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock should be monotonic")
        .as_nanos();
    let search_root = repo_root()
        .join("target")
        .join(format!("issue856-loader-{unique}"));
    let module_dir = search_root.join("std").join("issue856mod");
    fs::create_dir_all(&module_dir).expect("create module dir");
    fs::write(
        module_dir.join("issue856mod.hew"),
        "pub fn take(xs: [i32]) {}\n",
    )
    .expect("write module file");

    let parse_result = hew_parser::parse("import std::issue856mod;\n\nfn main() {}\n");
    assert!(
        parse_result.errors.is_empty(),
        "parse errors: {:#?}",
        parse_result.errors
    );

    let mut checker =
        hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![
            search_root.clone(),
        ]));
    let output = checker.check_program(&parse_result.program);
    let _ = fs::remove_dir_all(&search_root);

    assert!(
        output.errors.iter().any(|e| {
            e.kind == hew_types::error::TypeErrorKind::UnresolvedImport
                && e.message.contains(
                    "unsupported slice annotations in signature(s): public function `take`",
                )
        }),
        "expected registry-loaded module slice signature to fail closed at import, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashset_bool_insert_rejected_before_codegen() {
    let output = typecheck_inline(
        r"
        fn main() {
            let s: HashSet<bool> = HashSet::new();
            s.insert(true);
        }",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("HashSet<bool> is not supported")
        ),
        "expected HashSet<bool> to fail before lowering, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashset_i32_insert_rejected_before_codegen() {
    let output = typecheck_inline(
        r"
        fn main() {
            let s: HashSet<i32> = HashSet::new();
            s.insert(7);
        }",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("HashSet<i32> is not supported")
        ),
        "expected HashSet<i32> to fail before lowering, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashset_u32_insert_rejected_before_codegen() {
    let output = typecheck_inline(
        r"
        fn main() {
            let s: HashSet<u32> = HashSet::new();
            s.insert(7);
        }",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("HashSet<u32> is not supported")
        ),
        "expected HashSet<u32> to fail before lowering, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashset_i32_len_rejected_before_codegen() {
    let output = typecheck_inline(
        r"
        fn main() {
            let s: HashSet<i32> = HashSet::new();
            println(s.len());
        }",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("HashSet<i32> is not supported")
        ),
        "expected HashSet<i32>.len() to fail before lowering, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashset_bool_type_field_rejected_before_codegen() {
    let output = typecheck_inline(
        r"
        type Config {
            flags: HashSet<bool>;
        }

        fn main() {}",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("HashSet<bool> is not supported")
        ),
        "expected HashSet<bool> field annotation to fail before lowering, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashset_bool_param_annotation_rejected_before_codegen() {
    let output = typecheck_inline(
        r"
        fn count(flags: HashSet<bool>) -> i64 {
            flags.len()
        }

        fn main() {
            println(count(HashSet::new()));
        }",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("HashSet<bool> is not supported")
        ),
        "expected HashSet<bool> parameter annotation to fail before lowering, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashset_bool_return_annotation_rejected_before_codegen() {
    let output = typecheck_inline(
        r"
        fn make() -> HashSet<bool> {
            HashSet::new()
        }

        fn main() {
            println(make().len());
        }",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("HashSet<bool> is not supported")
        ),
        "expected HashSet<bool> return annotation to fail before lowering, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashset_bool_nested_in_vec_annotation_rejected_before_codegen() {
    let output = typecheck_inline(
        r"
        fn main() {
            let xs: Vec<HashSet<bool>> = Vec::new();
            println(xs.len());
        }",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("HashSet<bool> is not supported")
        ),
        "expected Vec<HashSet<bool>> annotation to fail before lowering, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashset_i32_nested_in_option_param_rejected_before_codegen() {
    let output = typecheck_inline(
        r"
        fn count(flags: Option<HashSet<i32>>) -> i64 {
            0
        }

        fn main() {
            println(count(None));
        }",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("HashSet<i32> is not supported")
        ),
        "expected Option<HashSet<i32>> parameter annotation to fail before lowering, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashset_bool_nested_in_result_return_rejected_before_codegen() {
    let output = typecheck_inline(
        r#"
        fn make() -> Result<HashSet<bool>, String> {
            Err("nope")
        }

        fn main() {
            println(make().is_err());
        }"#,
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("HashSet<bool> is not supported")
        ),
        "expected Result<HashSet<bool>, String> return annotation to fail before lowering, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashset_i32_nested_in_type_field_rejected_before_codegen() {
    let output = typecheck_inline(
        r"
        type Config {
            flags: HashMap<String, HashSet<i32>>;
        }

        fn main() {}",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("HashSet<i32> is not supported")
        ),
        "expected HashMap<String, HashSet<i32>> field annotation to fail before lowering, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashset_bool_nested_in_actor_field_rejected_before_codegen() {
    let output = typecheck_inline(
        r"
        actor Worker {
            let flags: Vec<HashSet<bool>>;

            receive fn ping() {}
        }

        fn main() {}",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("HashSet<bool> is not supported")
        ),
        "expected actor field annotation with nested HashSet<bool> to fail before lowering, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashset_i32_nested_in_machine_state_field_rejected_before_codegen() {
    let output = typecheck_inline(
        r"
        machine Traffic {
            state Red { flags: Option<HashSet<i32>>; }
            event Tick;
            on Tick: Red -> Red;
        }

        fn main() {}",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("HashSet<i32> is not supported")
        ),
        "expected machine state field annotation with nested HashSet<i32> to fail before lowering, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashset_bool_in_wire_enum_tuple_variant_rejected_before_codegen() {
    let output = typecheck_inline(
        r"
        wire enum Packet {
            Flags(HashSet<bool>);
        }

        fn main() {}",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("HashSet<bool> is not supported")
        ),
        "expected wire enum tuple variant with HashSet<bool> to fail before lowering, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashset_i32_nested_in_wire_enum_struct_variant_rejected_before_codegen() {
    let output = typecheck_inline(
        r"
        wire enum Packet {
            Flags { values: Vec<HashSet<i32>> };
        }

        fn main() {}",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("HashSet<i32> is not supported")
        ),
        "expected wire enum struct variant with nested HashSet<i32> to fail before lowering, got: {:#?}",
        output.errors
    );
}

#[test]
fn vec_clone_method_typechecks_and_returns_vec() {
    assert_inline_typechecks_cleanly(
        r"
        fn main() {
            let v: Vec<int> = Vec::new();
            let c = v.clone();
            c.push(42);
            println(c.len());
        }",
        "Vec.clone() should resolve and return a usable Vec",
    );
}

#[test]
fn hashmap_clone_method_typechecks_and_returns_hashmap() {
    assert_inline_typechecks_cleanly(
        r#"
        fn main() {
            let m: HashMap<String, int> = HashMap::new();
            let c = m.clone();
            c.insert("key", 7);
            println(c.contains_key("key"));
        }"#,
        "HashMap.clone() should resolve and return a usable HashMap",
    );
}

#[test]
fn hashset_clone_method_typechecks_and_returns_hashset() {
    assert_inline_typechecks_cleanly(
        r#"
        fn main() {
            let s: HashSet<String> = HashSet::new();
            let c = s.clone();
            c.insert("value");
            println(c.contains("value"));
        }"#,
        "HashSet.clone() should resolve and return a usable HashSet",
    );
}

// ── Named-wrapper transitive Rc rejection ───────────────────────────────────────
//
// Regression coverage for the named-wrapper hole: a struct/enum that
// *contains* Rc<T> in its fields must be rejected from collections even when
// the type argument list of the outer container shows no Rc directly.

#[test]
fn rc_in_named_struct_field_vec_push_rejected() {
    assert_unsafe_collection_element(
        r"
        type Holder {
            val: Rc<int>
        }
        fn main() {
            var v = Vec::new();
            let h = Holder { val: Rc::new(1) };
            v.push(h);
        }",
        "Vec.push(Holder { val: Rc<int> }) — named struct wrapping Rc should be rejected",
    );
}

#[test]
fn rc_in_named_struct_field_hashmap_value_rejected() {
    assert_unsafe_collection_element(
        r#"
        type Holder {
            val: Rc<int>
        }
        fn main() {
            var m = HashMap::new();
            let h = Holder { val: Rc::new(1) };
            m.insert("k", h);
        }"#,
        "HashMap.insert(_, Holder { val: Rc<int> }) — named struct wrapping Rc should be rejected",
    );
}

#[test]
fn rc_in_doubly_nested_named_struct_vec_push_rejected() {
    assert_unsafe_collection_element(
        r"
        type Inner {
            x: Rc<int>
        }
        type Outer {
            inner: Inner
        }
        fn main() {
            var v = Vec::new();
            let o = Outer { inner: Inner { x: Rc::new(1) } };
            v.push(o);
        }",
        "Vec.push(Outer wrapping Inner wrapping Rc<int>) — doubly-nested named struct should be rejected",
    );
}

#[test]
fn plain_named_struct_no_rc_vec_push_ok() {
    assert_no_unsafe_collection_element(
        r"
        type Point {
            x: int,
            y: int
        }
        fn main() {
            var v = Vec::new();
            let p = Point { x: 1, y: 2 };
            v.push(p);
        }",
        "Vec<Point> with no Rc should be fine",
    );
}

// ── Known limitations of BorrowedParamReturn ────────────────────────────────────
//
// The following patterns are NOT caught by the current syntactic scanner and
// are explicitly deferred to a future escape-analysis pass:
//
// 1. Generic passthrough: `fn id<T>(x: T) -> T { x }` is safe when called
//    with value types (int, String, structs) but unsound when `T = Rc<U>`.
//    Definition-site checking was removed because it rejects all generic
//    identity patterns.  Needs call-site / monomorphisation-time checking.
//
// 2. Inter-procedural storage: `let v = wrap(r); return v;` where `wrap`
//    stores `r` into a container.  Requires cross-function analysis.
//
// 3. Deeply nested non-constructor call chains are not caught.
//
// These are tracked as future escape-analysis work.

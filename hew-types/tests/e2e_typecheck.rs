use std::fs;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use hew_parser::ast::{Expr, Item, Stmt};
use hew_types::check::SpanKey;
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

fn parse_and_typecheck_inline(
    source: &str,
) -> (hew_parser::ast::Program, hew_types::TypeCheckOutput) {
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "should parse cleanly, got: {:#?}",
        parse_result.errors
    );
    let mut checker = new_networking_demo_checker();
    let output = checker.check_program(&parse_result.program);
    (parse_result.program, output)
}

fn typecheck_inline_wasm(source: &str) -> hew_types::TypeCheckOutput {
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "should parse cleanly, got: {:#?}",
        parse_result.errors
    );
    let mut checker = new_networking_demo_checker();
    checker.enable_wasm_target();
    checker.check_program(&parse_result.program)
}

fn platform_limitation_error_count(output: &hew_types::TypeCheckOutput, fragment: &str) -> usize {
    output
        .errors
        .iter()
        .filter(|error| {
            error.kind == TypeErrorKind::PlatformLimitation && error.message.contains(fragment)
        })
        .count()
}

fn main_call_spans(program: &hew_parser::ast::Program) -> Vec<hew_parser::ast::Span> {
    let main_fn = program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(fd) if fd.name == "main" => Some(fd),
            _ => None,
        })
        .expect("main function should exist");

    main_fn
        .body
        .stmts
        .iter()
        .filter_map(|(stmt, _)| match stmt {
            Stmt::Let {
                value: Some((Expr::Call { .. }, span)),
                ..
            }
            | Stmt::Expression((Expr::Call { .. }, span)) => Some(span.clone()),
            _ => None,
        })
        .collect()
}

fn assert_single_unknown_return_error(
    output: &hew_types::TypeCheckOutput,
    context: &str,
    actual: &str,
) {
    assert_eq!(
        output.errors.len(),
        1,
        "{context}: expected exactly one error, got: {:#?}",
        output.errors
    );
    assert!(
        output.errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrorKind::Mismatch { expected, actual: found }
                if expected == "UnknownType" && found == actual
        )),
        "{context}: expected a single UnknownType/{actual} mismatch, got: {:#?}",
        output.errors
    );
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
fn hashmap_i64_key_remove_rejected_before_codegen() {
    let output = typecheck_inline(
        r"
fn main() {
    let m: HashMap<i64, i64> = HashMap::new();
    let _ = m.remove(1);
}
",
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == TypeErrorKind::InvalidOperation
                && e.message.contains("HashMap<int, int> is not supported")
                && e.message.contains("String keys and scalar/string values")
        }),
        "expected HashMap<i64, i64>::remove to fail before lowering, got: {:#?}",
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
fn method_call_receiver_kinds_record_handle_dispatch() {
    let output = typecheck_inline(
        r"
import std::net;

fn close_conn(conn: net.Connection) -> int {
    conn.close()
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
                hew_types::MethodCallReceiverKind::HandleInstance { type_name }
                    if type_name == "net.Connection"
            )),
        "expected handle method call receiver metadata, got: {:?}",
        output.method_call_receiver_kinds
    );
}

#[test]
fn method_call_rewrites_record_builtin_runtime_dispatch() {
    let output = typecheck_inline(
        r"
fn consume(s: Stream<String>) {
    let _ = s.next();
}
",
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol }
                if c_symbol == "hew_stream_next"
        )),
        "expected checker-owned builtin rewrite metadata, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn method_call_rewrites_record_deferred_stream_lowering() {
    let output = typecheck_inline(
        r"
fn consume(s: Stream<bytes>) {
    let _ = s.take(1);
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
            .method_call_rewrites
            .values()
            .any(|rewrite| matches!(rewrite, hew_types::MethodCallRewrite::DeferToLowering)),
        "expected checker-owned deferred rewrite metadata, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn method_call_receiver_kinds_record_string_stream_dispatch() {
    let output = typecheck_inline(
        r"
fn consume(s: Stream<String>) {
    let _ = s.map((item) => item);
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
                hew_types::MethodCallReceiverKind::StreamInstance { element_kind }
                    if element_kind == "string"
            )),
        "expected string stream receiver metadata, got: {:?}",
        output.method_call_receiver_kinds
    );
}

#[test]
fn method_call_receiver_kinds_record_bytes_stream_dispatch() {
    let output = typecheck_inline(
        r"
fn consume(s: Stream<bytes>) {
    let _ = s.filter((item) => item.len() > 0);
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
                hew_types::MethodCallReceiverKind::StreamInstance { element_kind }
                    if element_kind == "bytes"
            )),
        "expected bytes stream receiver metadata, got: {:?}",
        output.method_call_receiver_kinds
    );
}

#[test]
fn method_call_rewrites_record_handle_runtime_dispatch() {
    let output = typecheck_inline(
        r#"
import std::net::http;

fn respond(req: http.Request) -> int {
    req.respond_text(200, "ok")
}
"#,
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol }
                if c_symbol == "hew_http_respond_text"
        )),
        "expected checker-owned handle rewrite metadata, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn module_qualified_call_rewrites_record_registry_c_symbol_metadata() {
    let output = typecheck_inline(
        r#"
import std::fs;

fn main() {
    let _ = fs.read("test.txt");
}
"#,
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            hew_types::MethodCallRewrite::RewriteModuleQualifiedToFunction { c_symbol }
                if c_symbol == "hew_file_read"
        )),
        "expected checker-owned module-qualified rewrite metadata, got: {:?}",
        output.method_call_rewrites
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

#[test]
fn lambda_actor_send_operator_requires_send_payload() {
    let output = typecheck_inline(
        r"
        fn main() {
            let worker = spawn (msg: Rc<int>) => {
                println(msg.strong_count());
            };
            let rc: Rc<int> = Rc::new(1);
            worker <- rc;
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::InvalidSend),
        "`<-` must reject non-Send payloads, got: {:#?}",
        output.errors
    );
}

#[test]
fn lambda_actor_send_operator_allows_send_payload() {
    let output = typecheck_inline(
        r"
        fn main() {
            let worker = spawn (msg: int) => {
                println(msg);
            };
            worker <- 1;
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "`<-` with Send payload should typecheck cleanly, got: {:#?}",
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
fn rc_copy_struct_construction_ok() {
    let output = typecheck_inline(
        r"
        type Point {
            x: int
            y: int
        }

        fn main() {
            let _rc = Rc::new(Point { x: 1, y: 2 });
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "Rc::new with a Copy struct payload should succeed; got errors: {:#?}",
        output.errors
    );
}

#[test]
fn rc_nested_payload_construction_ok() {
    let output =
        typecheck_inline(r#"fn main() { let _rc: Rc<Rc<String>> = Rc::new(Rc::new("hello")); }"#);
    assert!(
        output.errors.is_empty(),
        "Rc::new with nested supported Rc payloads should succeed; got errors: {:#?}",
        output.errors
    );
}

#[test]
fn rc_owned_option_payload_rejected() {
    let output = typecheck_inline(r#"fn main() { let _rc = Rc::new(Some("hello")); }"#);
    assert!(
        output.errors.iter().any(|e| {
            e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message
                    .contains("does not recursively drop owned contents")
        }),
        "Rc::new with Option<String> should fail closed, got: {:#?}",
        output.errors
    );
}

#[test]
fn rc_owned_struct_payload_rejected() {
    let output = typecheck_inline(
        r#"
        type Labelled {
            name: String
        }

        fn main() {
            let _rc = Rc::new(Labelled { name: "hello" });
        }
        "#,
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message
                    .contains("does not recursively drop owned contents")
        }),
        "Rc::new with a struct containing owned fields should fail closed, got: {:#?}",
        output.errors
    );
}

#[test]
fn rc_user_drop_payload_rejected() {
    let output = typecheck_inline(
        r"
        type Token {
            id: int
        }

        impl Drop for Token {
            fn drop(token: Token) {
                print(token.id);
            }
        }

        fn main() {
            let _rc = Rc::new(Token { id: 1 });
        }
        ",
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("`Rc<Token>` is not currently supported")
        }),
        "Rc::new with an explicit Drop payload should fail closed, got: {:#?}",
        output.errors
    );
}

#[test]
fn rc_owned_payload_annotation_rejected() {
    let output = typecheck_inline(r"fn borrow(_r: Rc<Option<String>>) {}");
    assert!(
        output.errors.iter().any(|e| {
            e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message
                    .contains("does not recursively drop owned contents")
        }),
        "Rc<Option<String>> annotations should fail closed, got: {:#?}",
        output.errors
    );
}

#[test]
fn rc_generic_wrapper_payload_rejected() {
    let output = typecheck_inline(
        r#"
        type Labelled {
            name: String
        }

        fn wrap<T>(val: T) -> Rc<T> {
            Rc::new(val)
        }

        fn main() {
            let _ = wrap(Labelled { name: "hello" });
        }
        "#,
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("Rc only accepts Copy payloads")
        }),
        "generic Rc<T> wrappers should fail closed until payload support is proven, got: {:#?}",
        output.errors
    );
}

#[test]
fn rc_generic_lambda_payload_rejected() {
    let output = typecheck_inline(
        r#"
        type Labelled {
            name: String
        }

        fn main() {
            let wrap = <T>(val: T) -> Rc<T> => Rc::new(val);
            let _ = wrap(Labelled { name: "hello" });
        }
        "#,
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("Rc only accepts Copy payloads")
        }),
        "generic Rc<T> lambdas should fail closed until payload support is proven, got: {:#?}",
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

#[test]
fn wasm_http_server_surface_rejected_before_codegen() {
    let output = typecheck_inline_wasm(
        r#"
        import std::net::http;

        fn inspect(server: http.Server, req: http.Request) -> String {
            let _next = server.accept();
            req.respond_text(200, "ok");
            server.close();
            req.path()
        }

        fn main() {
            let _ = http.listen(":8080");
        }
        "#,
    );
    let count = platform_limitation_error_count(&output, "HTTP server operations");
    assert!(
        count >= 4,
        "expected http server module and handle surfaces to be rejected on WASM, got: {:#?}",
        output.errors
    );
}

#[test]
fn wasm_tcp_networking_surface_rejected_before_codegen() {
    let output = typecheck_inline_wasm(
        r#"
        import std::net;

        fn tune(listener: net.Listener, conn: net.Connection) -> int {
            let _accepted = listener.accept();
            conn.set_read_timeout(10);
            conn.close()
        }

        fn main() {
            let _ = net.listen(":9000");
            let _ = net.connect("127.0.0.1:9000");
        }
        "#,
    );
    let count = platform_limitation_error_count(&output, "TCP networking operations");
    assert!(
        count >= 4,
        "expected tcp networking module and handle surfaces to be rejected on WASM, got: {:#?}",
        output.errors
    );
}

#[test]
fn http_request_body_encoding_arg_checked_via_fallback() {
    let output = typecheck_inline(
        r"
        import std::net::http;

        fn inspect(req: http.Request) {
            req.body(42);
        }
        ",
    );
    assert!(
        output.errors.iter().any(|error| matches!(
            &error.kind,
            TypeErrorKind::Mismatch { expected, .. } if expected == "String"
        )),
        "expected http.Request::body encoding arg to be checked via fallback, got: {:#?}",
        output.errors
    );
}

#[test]
fn net_listener_close_resolves_via_fallback() {
    let output = typecheck_inline(
        r"
        import std::net;

        fn close_listener(listener: net.Listener) -> int {
            listener.close()
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected net.Listener::close to resolve cleanly via fallback, got: {:#?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol }
                if c_symbol == "hew_tcp_listener_close"
        )),
        "expected net.Listener::close fallback rewrite, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn http_request_free_resolves_via_fallback() {
    let output = typecheck_inline(
        r"
        import std::net::http;

        fn release(req: http.Request) {
            req.free();
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected http.Request::free to resolve cleanly via fallback, got: {:#?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol }
                if c_symbol == "hew_http_request_free"
        )),
        "expected http.Request::free fallback rewrite, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn http_request_unknown_method_is_undefined() {
    let output = typecheck_inline(
        r"
        import std::net::http;

        fn inspect(req: http.Request) {
            req.nonexistent();
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::UndefinedMethod),
        "expected http.Request unknown method to report UndefinedMethod, got: {:#?}",
        output.errors
    );
}

#[test]
fn net_connection_write_arg_type_checked() {
    let output = typecheck_inline(
        r#"
        import std::net;

        fn send(conn: net.Connection) {
            conn.write("wrong_type");
        }
        "#,
    );
    assert!(
        output.errors.iter().any(|error| matches!(
            &error.kind,
            TypeErrorKind::Mismatch { expected, .. } if expected == "bytes"
        )),
        "expected net.Connection::write arg to be checked via fallback, got: {:#?}",
        output.errors
    );
}

#[test]
fn wasm_process_execution_surface_rejected_before_codegen() {
    let output = typecheck_inline_wasm(
        r#"
        import std::process;

        fn await_child(child: process.Child) -> int {
            child.wait()
        }

        fn main() {
            let _ = process.run("echo hi");
            let _ = process.start("sleep 1");
        }
        "#,
    );
    let count = platform_limitation_error_count(&output, "Process execution operations");
    assert!(
        count >= 3,
        "expected process module and child surfaces to be rejected on WASM, got: {:#?}",
        output.errors
    );
}

#[test]
fn builtin_string_to_int_typechecks_as_int() {
    let output = typecheck_inline(
        r#"
        fn parse() -> int {
            let value: int = string_to_int("9223372036854775807");
            value
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
}

#[test]
fn process_child_methods_typecheck_and_preserve_rewrite_path() {
    let output = typecheck_inline(
        r"
        import std::process;

        fn manage(child: process.Child) -> int {
            let waited: int = child.wait();
            waited + child.kill()
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol }
                if c_symbol == "hew_process_wait"
        )),
        "expected process.Child.wait rewrite, got: {:?}",
        output.method_call_rewrites
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol }
                if c_symbol == "hew_process_kill"
        )),
        "expected process.Child.kill rewrite, got: {:?}",
        output.method_call_rewrites
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

#[test]
fn http_client_module_helpers_typecheck_natively() {
    let output = typecheck_inline(
        r#"
        import std::net::http::http_client;

        fn main() {
            let headers: Vec<(String, String)> = Vec::new();
            http_client.set_timeout(250);
            let _body = http_client.request_string("GET", "https://example.com", "", headers);
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "http_client helper calls should typecheck natively, got: {:#?}",
        output.errors
    );
}

#[test]
fn http_client_module_helpers_rejected_on_wasm() {
    let output = typecheck_inline_wasm(
        r#"
        import std::net::http::http_client;

        fn main() {
            let headers: Vec<(String, String)> = Vec::new();
            http_client.set_timeout(250);
            let _body = http_client.request_string("GET", "https://example.com", "", headers);
        }
        "#,
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == TypeErrorKind::PlatformLimitation
                && e.message
                    .contains("std::net::http::http_client operations are not supported on WASM32")
        }),
        "expected http_client wasm rejection, got: {:#?}",
        output.errors
    );
}

#[test]
fn http_client_response_methods_rejected_on_wasm() {
    let output = typecheck_inline_wasm(
        r#"
        import std::net::http::http_client;

        extern "C" {
            fn fake_response() -> http_client.Response;
        }

        fn main() {
            let resp = unsafe { fake_response() };
            let _status = resp.status();
            resp.free();
        }
        "#,
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == TypeErrorKind::PlatformLimitation
                && e.message
                    .contains("std::net::http::http_client operations are not supported on WASM32")
        }),
        "expected http_client.Response wasm rejection, got: {:#?}",
        output.errors
    );
}

#[test]
fn smtp_module_helpers_rejected_on_wasm() {
    let output = typecheck_inline_wasm(
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
        }
        "#,
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == TypeErrorKind::PlatformLimitation
                && e.message
                    .contains("std::net::smtp operations are not supported on WASM32")
        }),
        "expected smtp wasm rejection, got: {:#?}",
        output.errors
    );
}

#[test]
fn smtp_conn_methods_rejected_on_wasm() {
    let output = typecheck_inline_wasm(
        r#"
        import std::net::smtp;

        extern "C" {
            fn fake_conn() -> smtp.Conn;
        }

        fn main() {
            let conn = unsafe { fake_conn() };
            let _ = conn.send(
                "from@example.com",
                "to@example.com",
                "Subject",
                "Body",
            );
            conn.close();
        }
        "#,
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == TypeErrorKind::PlatformLimitation
                && e.message
                    .contains("std::net::smtp operations are not supported on WASM32")
        }),
        "expected smtp.Conn wasm rejection, got: {:#?}",
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
fn rc_vec_map_rejected() {
    assert_unsafe_collection_element(
        r"
        type Holder { v: Vec<Rc<int>> }
        fn extract(h: Holder) -> Vec<Rc<int>> {
            h.v.map((x: Rc<int>) => x)
        }",
        "Vec.map(_) on Vec<Rc<int>>",
    );
}

#[test]
fn rc_vec_filter_rejected() {
    assert_unsafe_collection_element(
        r"
        type Holder { v: Vec<Rc<int>> }
        fn extract(h: Holder) -> Vec<Rc<int>> {
            h.v.filter((x: Rc<int>) => x.get() > 0)
        }",
        "Vec.filter(_) on Vec<Rc<int>>",
    );
}

#[test]
fn rc_vec_fold_rejected() {
    assert_unsafe_collection_element(
        r"
        type Holder { v: Vec<Rc<int>> }
        fn extract(h: Holder) -> Rc<int> {
            h.v.fold(Rc::new(0), (acc: Rc<int>, x: Rc<int>) => x)
        }",
        "Vec.fold(_, _) on Vec<Rc<int>>",
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
fn rc_hashmap_get_value_rejected() {
    assert_unsafe_collection_element(
        r#"
        type Holder {
            items: HashMap<String, Rc<int>>
        }
        fn leak(h: Holder) -> Option<Rc<int>> {
            h.items.get("key")
        }"#,
        "HashMap.get() on HashMap<String, Rc<int>>",
    );
}

#[test]
fn rc_hashmap_remove_value_rejected() {
    assert_unsafe_collection_element(
        r#"
        type Holder {
            items: HashMap<String, Rc<int>>
        }
        fn remove_key(h: Holder) -> bool {
            h.items.remove("key")
        }"#,
        "HashMap.remove() on HashMap<String, Rc<int>>",
    );
}

#[test]
fn rc_hashmap_keys_rejected_when_value_type_is_rc() {
    assert_unsafe_collection_element(
        r"
        type Holder {
            items: HashMap<String, Rc<int>>
        }
        fn leak(h: Holder) -> Vec<String> {
            h.items.keys()
        }",
        "HashMap.keys() on HashMap<String, Rc<int>>",
    );
}

#[test]
fn rc_hashmap_values_rejected() {
    assert_unsafe_collection_element(
        r"
        type Holder {
            items: HashMap<String, Rc<int>>
        }
        fn leak(h: Holder) -> Vec<Rc<int>> {
            h.items.values()
        }",
        "HashMap.values() on HashMap<String, Rc<int>>",
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
    let output = typecheck_inline(
        r"
        fn main() {
            var s = HashSet::new();
            s.insert(42);
        }",
    );
    assert!(
        output.errors.is_empty(),
        "expected inferred HashSet<int> insert to typecheck cleanly, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashset_inferred_literal_then_i16_insert_rejected_before_codegen() {
    let output = typecheck_inline(
        r"
        fn main() {
            var s = HashSet::new();
            s.insert(42);
            let x: i16 = 7;
            s.insert(x);
        }",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("HashSet<i16> is not supported")
        ),
        "expected inferred HashSet narrowed to i16 to fail before lowering, got: {:#?}",
        output.errors
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
fn hashset_small_integer_inserts_rejected_before_codegen() {
    for elem_ty in ["i8", "u8", "i16", "u16"] {
        let source = format!(
            r"
        fn main() {{
            let s: HashSet<{elem_ty}> = HashSet::new();
            s.insert(7);
        }}"
        );
        let expected = format!("HashSet<{elem_ty}> is not supported");
        let output = typecheck_inline(&source);
        assert!(
            output.errors.iter().any(|e| {
                e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                    && e.message.contains(&expected)
            }),
            "expected HashSet<{elem_ty}> to fail before lowering, got: {:#?}",
            output.errors
        );
    }
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
fn hashmap_string_i64_annotation_typechecks_before_codegen() {
    assert_inline_typechecks_cleanly(
        r#"
        fn main() {
            let m: HashMap<String, i64> = HashMap::new();
            m.insert("answer", 42);
            println(m.len());
        }"#,
        "HashMap<String, i64> should stay within the current ABI",
    );
}

#[test]
fn inferred_hashmap_string_string_map_literal_typechecks_before_codegen() {
    assert_inline_typechecks_cleanly(
        r#"
        fn main() {
            let env = {"HOST": "localhost", "PORT": "8080"};
            println(env.contains_key("HOST"));
        }"#,
        "inferred HashMap<String, String> map literal should stay within the current ABI",
    );
}

#[test]
fn hashmap_i64_key_len_rejected_before_codegen() {
    let output = typecheck_inline(
        r"
        fn main() {
            let m: HashMap<i64, i64> = HashMap::new();
            println(m.len());
        }",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("HashMap<int, int> is not supported")
                && e.message.contains("String keys and scalar/string values")
        ),
        "expected HashMap<i64, i64> to fail before lowering, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashmap_tuple_value_annotation_rejected_before_codegen() {
    let output = typecheck_inline(
        r"
        type Config {
            points: HashMap<String, (i32, i32)>;
        }

        fn main() {}",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message
                    .contains("HashMap<String, (i32, i32)> is not supported")
        ),
        "expected HashMap<String, (i32, i32)> field annotation to fail before lowering, got: {:#?}",
        output.errors
    );
}

#[test]
fn inferred_hashmap_tuple_value_map_literal_rejected_before_codegen() {
    let output = typecheck_inline(
        r#"
        fn main() {
            let x: i32 = 1;
            let y: i32 = 2;
            let points = {"origin": (x, y)};
            println(points.len());
        }"#,
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message
                    .contains("HashMap<String, (i32, i32)> is not supported")
        ),
        "expected inferred HashMap<String, (i32, i32)> literal to fail before lowering, got: {:#?}",
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
fn rc_in_named_enum_variant_vec_push_rejected() {
    assert_unsafe_collection_element(
        r"
        enum MaybeHolder {
            Some(Rc<int>);
            None;
        }
        fn main() {
            var v = Vec::new();
            v.push(MaybeHolder::Some(Rc::new(1)));
        }",
        "Vec.push(MaybeHolder::Some(Rc<int>)) — named enum wrapping Rc should be rejected",
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

// ── IfLet trailing-expression escape paths ───────────────────────────────────

/// An `if let` body that ends with a bare Rc param as its trailing expression
/// must be diagnosed — the escape hole exists whether the pattern matches or not.
#[test]
fn rc_param_iflet_body_trailing_expr_errors() {
    let output = typecheck_inline(
        r"
        fn escape(r: Rc<int>, opt: Option<int>) -> Rc<int> {
            if let Some(_v) = opt {
                r
            } else {
                Rc::new(0)
            }
        }
        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn),
        "if-let body trailing-expr Rc escape should emit BorrowedParamReturn, got errors: {:#?}, warnings: {:#?}",
        output.errors, output.warnings
    );
}

/// An `if let` else block that ends with a bare Rc param as its trailing
/// expression must also be diagnosed.
#[test]
fn rc_param_iflet_else_trailing_expr_errors() {
    let output = typecheck_inline(
        r"
        fn escape(r: Rc<int>, opt: Option<int>) -> Rc<int> {
            if let Some(_v) = opt {
                Rc::new(0)
            } else {
                r
            }
        }
        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn),
        "if-let else trailing-expr Rc escape should emit BorrowedParamReturn, got errors: {:#?}, warnings: {:#?}",
        output.errors, output.warnings
    );
}

/// Both branches of an `if let` returning Rc param must each be diagnosed.
#[test]
fn rc_param_iflet_both_branches_trailing_expr_errors() {
    let output = typecheck_inline(
        r"
        fn escape(r: Rc<int>, opt: Option<int>) -> Rc<int> {
            if let Some(_v) = opt {
                r
            } else {
                r
            }
        }
        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
            .count()
            >= 2,
        "both branches of if-let returning Rc param should emit >= 2 BorrowedParamReturn errors, got: {:#?}",
        output.errors
    );
}

/// A local binding that shadows an Rc parameter must suppress the borrowed
/// return diagnostic for that scope.
#[test]
fn rc_param_shadowing_local_return_is_clean() {
    let output = typecheck_inline(
        r"
        fn shadow(r: Rc<int>) -> Rc<int> {
            let r = Rc::new(99);
            r
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
        "shadowing local Rc should not emit BorrowedParamReturn, got: {rc_errors:#?}",
    );
}

/// Shadowing only suppresses the borrowed return diagnostic inside the shadowed
/// scope; other branches that still return the original parameter must warn.
#[test]
fn rc_param_iflet_shadow_only_suppresses_inner_scope() {
    let output = typecheck_inline(
        r"
        fn shadow_then_escape(r: Rc<int>, opt: Option<int>) -> Rc<int> {
            if let Some(v) = opt {
                let r = Rc::new(v);
                r
            } else {
                r
            }
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
        "only the else branch should emit BorrowedParamReturn, got: {rc_errors:#?}",
    );
}

// ── Known limitations of BorrowedParamReturn ────────────────────────────────────
//
// The following patterns are NOT caught by the current syntactic scanner and
// are explicitly deferred to a future escape-analysis pass:
//
// 1. Generic passthrough: `fn id<T>(x: T) -> T { x }` is safe when called
//    with value types (int, String, String, structs) but unsound when `T = Rc<U>`.
//    Definition-site checking was removed because it rejects all generic
//    identity patterns.  Needs call-site / monomorphisation-time checking.
//
// 2. Inter-procedural storage: `let v = wrap(r); return v;` where `wrap`
//    stores `r` into a container.  Requires cross-function analysis.
//
// 3. Deeply nested non-constructor call chains are not caught.
//
// These are tracked as future escape-analysis work.

// ── HashMap admission fail-closed ────────────────────────────────────────────
//
// Regression tests for the fix that ensures Ty::Var and Ty::Error in HashMap
// key/value positions fail closed at the checker boundary rather than leaking
// into C++ codegen.

#[test]
fn hashmap_unresolved_key_type_fails_closed_at_boundary() {
    // `m.len()` is called before anything constrains the key type.  The inline
    // check defers; finalize_hashmap_admission must fail closed.
    let output = typecheck_inline(
        r"
        fn main() {
            var m = HashMap::new();
            let _ = m.len();
        }",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InferenceFailed),
        "expected InferenceFailed for unresolved HashMap key/value type, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashmap_unresolved_val_type_fails_closed_at_boundary() {
    // Key is constrained (String), value is not.
    let output = typecheck_inline(
        r"
        fn main() {
            var m = HashMap::new();
            let _ = m.is_empty();
        }",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InferenceFailed),
        "expected InferenceFailed for fully-unresolved HashMap, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashmap_error_key_type_fails_closed_silently() {
    // An already-errored key type (from an undefined name) should not produce
    // a *second* diagnostic about the HashMap admission — only one error.
    let output = typecheck_inline(
        r#"
        fn main() {
            let k = undefined_fn();
            var m = HashMap::new();
            m.insert(k, "val");
        }"#,
    );
    // There must be at least one error (the undefined_fn reference).
    assert!(
        !output.errors.is_empty(),
        "expected at least one error for undefined function"
    );
    // But there must be NO InvalidOperation about HashMap admission cascaded
    // on top of the existing error.
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation && e.message.contains("HashMap")),
        "unexpected cascading HashMap admission error, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashmap_valid_string_key_int_val_not_rejected() {
    // Ensure the deferred path does not incorrectly reject well-typed HashMaps.
    let output = typecheck_inline(
        r#"
        fn main() {
            var m = HashMap::new();
            m.insert("key", 42);
            let _ = m.len();
        }"#,
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors for HashMap<String, int>, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashmap_annotation_with_infer_hole_key_fails_closed() {
    // A HashMap annotation with an explicit inference hole (`_`) for the key
    // that is never constrained must fail closed.
    let output = typecheck_inline(
        r"
        fn main() {
            let m: HashMap<_, String> = HashMap::new();
        }",
    );
    assert!(
        !output.errors.is_empty(),
        "expected error for HashMap<_, String> with unresolved key, got no errors"
    );
}

// ── HashSet admission fail-closed ────────────────────────────────────────────
//
// Regression tests for the fix that ensures Ty::Var and Ty::Error in HashSet
// element positions fail closed at the checker boundary rather than leaking
// into codegen.

#[test]
fn hashset_unresolved_element_type_fails_closed_at_boundary() {
    // `s.len()` is called before anything constrains the element type.
    // finalize_lowering_facts must emit exactly one InferenceFailed about
    // the HashSet element type; finalize_hashset_admission must be silent
    // (deferred entry was evicted by record_hashset_lowering_fact).
    let output = typecheck_inline(
        r"
        fn main() {
            var s = HashSet::new();
            let _ = s.len();
        }",
    );
    // At least one InferenceFailed must exist (fail-closed).
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InferenceFailed),
        "expected InferenceFailed for unresolved HashSet element type, got: {:#?}",
        output.errors
    );
    // No two InferenceFailed at the same span — finalize_hashset_admission
    // must not fire alongside the lowering-fact finalizer.
    let mut span_counts: std::collections::HashMap<String, usize> =
        std::collections::HashMap::new();
    for e in output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::InferenceFailed)
    {
        *span_counts.entry(format!("{:?}", e.span)).or_insert(0) += 1;
    }
    for (span, count) in &span_counts {
        assert_eq!(
            *count, 1,
            "duplicate InferenceFailed ({count}) at span {span}: {:#?}",
            output.errors
        );
    }
    // Exactly one InferenceFailed whose message is about the HashSet element
    // type (the lowering-fact boundary error).
    let lowering_fact_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::InferenceFailed && e.message.contains("HashSet"))
        .collect();
    assert_eq!(
        lowering_fact_errors.len(),
        1,
        "expected exactly one HashSet InferenceFailed, got: {lowering_fact_errors:#?}"
    );
}

#[test]
fn hashset_error_element_type_fails_closed_silently() {
    // An already-errored element type (from an undefined name) should not
    // produce a *second* diagnostic about HashSet admission — only one error.
    let output = typecheck_inline(
        r"
        fn main() {
            let e = undefined_fn();
            var s = HashSet::new();
            s.insert(e);
        }",
    );
    // There must be at least one error (the undefined_fn reference).
    assert!(
        !output.errors.is_empty(),
        "expected at least one error for undefined function"
    );
    // But there must be NO InvalidOperation about HashSet admission cascaded
    // on top of the existing error.
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation && e.message.contains("HashSet")),
        "unexpected cascading HashSet admission error, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashset_valid_string_element_not_rejected() {
    // Ensure the deferred path does not incorrectly reject well-typed HashSets.
    let output = typecheck_inline(
        r#"
        fn main() {
            var s = HashSet::new();
            s.insert("hello");
            let _ = s.len();
        }"#,
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors for HashSet<String>, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashset_valid_i64_element_not_rejected() {
    // Ensure that HashSet<i64> (the other supported element type) passes cleanly.
    let output = typecheck_inline(
        r"
        fn main() {
            var s = HashSet::new();
            s.insert(42);
            let _ = s.contains(42);
        }",
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors for HashSet<i64>, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashset_annotation_with_infer_hole_fails_closed() {
    // A HashSet annotation with an explicit inference hole (`_`) for the element
    // that is never constrained must fail closed with exactly one error.
    // The inference-holes path is the sole authority; finalize_hashset_admission
    // must not add a duplicate.
    let output = typecheck_inline(
        r"
        fn main() {
            let s: HashSet<_> = HashSet::new();
        }",
    );
    assert!(
        !output.errors.is_empty(),
        "expected error for HashSet<_> with unresolved element type, got no errors"
    );
    // No two InferenceFailed errors at the same span.
    let mut seen_spans: std::collections::HashSet<String> = std::collections::HashSet::new();
    for e in output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::InferenceFailed)
    {
        let key = format!("{:?}", e.span);
        assert!(
            seen_spans.insert(key.clone()),
            "duplicate InferenceFailed at span {key} for HashSet<_> annotation: {:#?}",
            output.errors
        );
    }
}

#[test]
fn hashset_unresolved_element_multiple_method_calls_no_duplicate_diagnostic() {
    // Multiple method calls on the same unresolved HashSet must not spray one
    // InferenceFailed per call site.  finalize_lowering_facts deduplicates by
    // TypeVar identity, so exactly one lowering-fact InferenceFailed should
    // appear (for the shared unresolved root var).  The binding-level
    // InferenceFailed from report_unresolved_inference_holes is distinct and
    // legitimate, but still only one.
    let output = typecheck_inline(
        r"
        fn main() {
            var s = HashSet::new();
            let _ = s.len();
            let _ = s.is_empty();
        }",
    );
    // Exactly one InferenceFailed whose message mentions the HashSet
    // lowering boundary (not the binding-level error).
    let hashset_lowering_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::InferenceFailed && e.message.contains("HashSet"))
        .collect();
    assert_eq!(
        hashset_lowering_errors.len(),
        1,
        "expected exactly one HashSet lowering InferenceFailed (TypeVar dedup), \
         got {}: {:#?}",
        hashset_lowering_errors.len(),
        output.errors
    );
    // No span should appear twice in the full InferenceFailed set.
    let mut seen_spans: std::collections::HashSet<String> = std::collections::HashSet::new();
    for e in output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::InferenceFailed)
    {
        let key = format!("{:?}", e.span);
        assert!(
            seen_spans.insert(key.clone()),
            "duplicate InferenceFailed at span {key}: {:#?}",
            output.errors
        );
    }
}

#[test]
fn hashset_annotation_only_unresolved_fails_closed_without_lowering_fact() {
    // A HashSet annotation with no method calls means no lowering fact is
    // recorded.  The inference-holes path catches the unresolved `_` and emits
    // exactly one InferenceFailed; finalize_hashset_admission must remain
    // silent because validate_named_collection now returns Some(true) for
    // Ty::Var args — it does not queue a deferred entry.
    let output = typecheck_inline(
        r"
        fn main() {
            var s: HashSet<_> = HashSet::new();
        }",
    );
    // Exactly one InferenceFailed total (the binding-level hole).
    let inference_failed: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::InferenceFailed)
        .collect();
    assert_eq!(
        inference_failed.len(),
        1,
        "expected exactly 1 InferenceFailed for annotation-only unresolved HashSet, \
         got {}: {:#?}",
        inference_failed.len(),
        output.errors
    );
    // That one error must be the binding-level hole, not a HashSet-specific one.
    assert!(
        !inference_failed[0].message.contains("HashSet"),
        "expected binding-level InferenceFailed, not a HashSet message: {}",
        inference_failed[0].message
    );
}

// ── HashMap annotation-hole duplicate-diagnostic regressions ─────────────────
//
// PR #957 introduced HashMap deferred admission; the same annotation-hole
// duplicate path applies there.  These tests pin that it is also fixed.

#[test]
fn hashmap_annotation_key_hole_no_duplicate_inference_failed() {
    // HashMap<_, String> with an unresolved key hole must produce at most one
    // InferenceFailed per span.  finalize_hashmap_admission must not add a
    // second error on top of the inference-holes diagnostic.
    let output = typecheck_inline(
        r"
        fn main() {
            let m: HashMap<_, String> = HashMap::new();
        }",
    );
    assert!(
        !output.errors.is_empty(),
        "expected error for HashMap<_, String> with unresolved key, got no errors"
    );
    let mut seen_spans: std::collections::HashSet<String> = std::collections::HashSet::new();
    for e in output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::InferenceFailed)
    {
        let key = format!("{:?}", e.span);
        assert!(
            seen_spans.insert(key.clone()),
            "duplicate InferenceFailed at span {key} for HashMap<_, String>: {errors:#?}",
            errors = output.errors
        );
    }
}

#[test]
fn hashmap_annotation_val_hole_no_duplicate_inference_failed() {
    // HashMap<String, _> with an unresolved value hole must produce at most one
    // InferenceFailed per span.
    let output = typecheck_inline(
        r"
        fn main() {
            let m: HashMap<String, _> = HashMap::new();
        }",
    );
    assert!(
        !output.errors.is_empty(),
        "expected error for HashMap<String, _> with unresolved value, got no errors"
    );
    let mut seen_spans: std::collections::HashSet<String> = std::collections::HashSet::new();
    for e in output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::InferenceFailed)
    {
        let key = format!("{:?}", e.span);
        assert!(
            seen_spans.insert(key.clone()),
            "duplicate InferenceFailed at span {key} for HashMap<String, _>: {errors:#?}",
            errors = output.errors
        );
    }
}

// ── Registration-time annotation-hole duplicate-diagnostic regressions ───────
//
// Functions/methods/type-fields with `_` holes in HashSet/HashMap annotations
// go through resolve_registered_annotation_ty → validate_concrete_collection_types
// → validate_named_collection.  validate_named_collection now returns Some(true)
// for Ty::Var/Ty::Error args, preventing deferred admission entries from being
// created.  Only report_unresolved_inference_in_items fires, producing exactly
// one InferenceFailed per unresolved signature hole.

#[test]
fn registration_fn_param_hashset_hole_single_error() {
    // `fn f(x: HashSet<_>) {}` must produce exactly one InferenceFailed.
    // Before the fix, validate_named_collection deferred the Ty::Var element
    // into deferred_hashset_admission; finalize_hashset_admission then fired
    // alongside report_unresolved_inference_in_items → duplicate.
    let output = typecheck_inline("fn f(x: HashSet<_>) {}");
    let inference_failed: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::InferenceFailed)
        .collect();
    assert_eq!(
        inference_failed.len(),
        1,
        "expected exactly one InferenceFailed for fn f(x: HashSet<_>), \
         got {}: {:#?}",
        inference_failed.len(),
        output.errors
    );
}

#[test]
fn registration_fn_param_hashmap_key_hole_single_error() {
    // `fn f(x: HashMap<_, String>) {}` must produce exactly one InferenceFailed.
    let output = typecheck_inline("fn f(x: HashMap<_, String>) {}");
    let inference_failed: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::InferenceFailed)
        .collect();
    assert_eq!(
        inference_failed.len(),
        1,
        "expected exactly one InferenceFailed for fn f(x: HashMap<_, String>), \
         got {}: {:#?}",
        inference_failed.len(),
        output.errors
    );
}

#[test]
fn registration_fn_param_hashmap_val_hole_single_error() {
    // `fn f(x: HashMap<String, _>) {}` must produce exactly one InferenceFailed.
    let output = typecheck_inline("fn f(x: HashMap<String, _>) {}");
    let inference_failed: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::InferenceFailed)
        .collect();
    assert_eq!(
        inference_failed.len(),
        1,
        "expected exactly one InferenceFailed for fn f(x: HashMap<String, _>), \
         got {}: {:#?}",
        inference_failed.len(),
        output.errors
    );
}

// ── HashMap repeated-method-call spray regression ────────────────────────────

#[test]
fn hashmap_unresolved_multiple_method_calls_no_duplicate_diagnostic() {
    // Multiple method calls on the same unresolved HashMap must not spray one
    // InferenceFailed per call site.  finalize_hashmap_admission deduplicates
    // by (key_TypeVar, val_TypeVar) pair, so exactly one admission
    // InferenceFailed appears regardless of how many methods are called.
    let output = typecheck_inline(
        r"
        fn main() {
            var m = HashMap::new();
            let _ = m.len();
            let _ = m.is_empty();
        }",
    );
    let hashmap_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::InferenceFailed && e.message.contains("HashMap"))
        .collect();
    assert_eq!(
        hashmap_errors.len(),
        1,
        "expected exactly one HashMap InferenceFailed (var-pair dedup), \
         got {}: {:#?}",
        hashmap_errors.len(),
        output.errors
    );
}

// ── type_defs output-contract regressions ────────────────────────────────────

#[test]
fn type_def_with_error_field_is_pruned_from_output() {
    let output = typecheck_inline(
        r"
        type Broken {
            value: [int];
            ok: int;
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation && e.message.contains("slice")),
        "expected slice annotation to produce Ty::Error, got: {:#?}",
        output.errors
    );
    assert!(
        !output.type_defs.contains_key("Broken"),
        "type_defs must prune type shapes containing Ty::Error fields: {:#?}",
        output.type_defs
    );
}

#[test]
fn enum_with_error_variant_payload_is_pruned_from_output() {
    let output = typecheck_inline(
        r"
        enum Broken {
            Bad([int]);
            Good(int);
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation && e.message.contains("slice")),
        "expected slice annotation to produce Ty::Error, got: {:#?}",
        output.errors
    );
    assert!(
        !output.type_defs.contains_key("Broken"),
        "type_defs must prune enum variants containing Ty::Error payloads: {:#?}",
        output.type_defs
    );
}

#[test]
fn type_def_method_with_error_param_is_pruned_from_output() {
    let output = typecheck_inline(
        r"
        type Widget {
            value: int;
        }

        impl Widget {
            fn good(w: Widget) -> int {
                w.value
            }

            fn broken(w: Widget, bad: [int]) -> int {
                w.value
            }
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation && e.message.contains("slice")),
        "expected slice annotation to produce Ty::Error, got: {:#?}",
        output.errors
    );
    let widget = output
        .type_defs
        .get("Widget")
        .expect("type_defs should retain Widget when only a method signature is errored");
    assert!(
        widget.methods.contains_key("good"),
        "clean methods must survive output-contract pruning: {:#?}",
        widget.methods
    );
    assert!(
        !widget.methods.contains_key("broken"),
        "methods with Ty::Error params must be pruned from type_defs: {:#?}",
        widget.methods
    );
}

#[test]
fn type_def_method_with_error_return_is_pruned_from_output() {
    let output = typecheck_inline(
        r"
        type Widget {
            value: int;
        }

        impl Widget {
            fn good(w: Widget) -> int {
                w.value
            }

            fn broken(w: Widget) -> [int] {
                w.value
            }
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation && e.message.contains("slice")),
        "expected slice annotation to produce Ty::Error, got: {:#?}",
        output.errors
    );
    let widget = output
        .type_defs
        .get("Widget")
        .expect("type_defs should retain Widget when only a method signature is errored");
    assert!(
        widget.methods.contains_key("good"),
        "clean methods must survive output-contract pruning: {:#?}",
        widget.methods
    );
    assert!(
        !widget.methods.contains_key("broken"),
        "methods with Ty::Error returns must be pruned from type_defs: {:#?}",
        widget.methods
    );
}

#[test]
fn fn_unknown_return_annotation_single_error() {
    let output = typecheck_inline("fn f() -> UnknownType {}");
    assert_single_unknown_return_error(&output, "unknown fn return annotation", "()");
}

#[test]
fn lambda_unknown_return_annotation_single_error() {
    let output = typecheck_inline(
        r"
        fn main() {
            let _f = () -> UnknownType => 1;
        }
        ",
    );
    assert_single_unknown_return_error(&output, "unknown lambda return annotation", "int");
}

#[test]
fn receive_fn_unknown_return_annotation_single_error() {
    let output = typecheck_inline(
        r"
        actor Worker {
            receive fn run() -> UnknownType {}
        }

        fn main() {}
        ",
    );
    assert_single_unknown_return_error(&output, "unknown receive-fn return annotation", "()");
}

#[test]
fn call_type_args_failed_generic_call_pruned_at_boundary() {
    let source = r"
        fn id<T>(x: T) -> T { x }

        fn main() {
            let _ = id(None);
        }
    ";

    let (program, output) = parse_and_typecheck_inline(source);
    let call_spans = main_call_spans(&program);
    assert_eq!(call_spans.len(), 1, "expected one call site in main");
    let call_key = SpanKey::from(&call_spans[0]);

    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InferenceFailed),
        "expected generic call inference failure, got: {:#?}",
        output.errors
    );
    assert!(
        !output.expr_types.contains_key(&call_key),
        "failed generic call span must be pruned from expr_types: {:#?}",
        output.expr_types
    );
    assert!(
        !output.call_type_args.contains_key(&call_key),
        "failed generic call span must be pruned from call_type_args: {:#?}",
        output.call_type_args
    );
}

// ===========================================================================
// Deferred channel method rewrite tests
//
// These tests cover the post-inference symbol-selection fix for
// Sender<T>::send, Receiver<T>::recv, and Receiver<T>::try_recv when the
// inner type T is only constrained *after* the call site.  The correct
// type-specific C symbol must be selected even when the call is visited before
// the surrounding context has narrowed T.
// ===========================================================================

/// `recv()` on a `Receiver<int>` channel where the element type is inferred
/// from a downstream `let v: int = rx.recv()` annotation must emit the int-
/// specific symbol (`hew_channel_recv_int`) rather than the string variant.
#[test]
fn deferred_channel_recv_int_constrained_after_call() {
    let output = typecheck_inline(
        r"
        import std::channel::channel;

        fn take_one() -> Option<int> {
            let (tx, rx) = channel.new(4);
            let v: Option<int> = rx.recv();
            tx.close();
            v
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol }
                if c_symbol == "hew_channel_recv_int"
        )),
        "expected hew_channel_recv_int rewrite after deferred resolution, got: {:?}",
        output.method_call_rewrites
    );
    // The string variant must NOT be recorded for this call site.
    assert!(
        !output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol }
                if c_symbol == "hew_channel_recv"
        )),
        "hew_channel_recv (string variant) must not be recorded for int channel: {:?}",
        output.method_call_rewrites
    );
}

/// `recv()` on a `Receiver<String>` channel where the element type is inferred
/// from the usage of the received value must emit `hew_channel_recv`.
#[test]
fn deferred_channel_recv_string_constrained_after_call() {
    let output = typecheck_inline(
        r"
        import std::channel::channel;

        fn take_one() -> Option<String> {
            let (tx, rx) = channel.new(4);
            let v: Option<String> = rx.recv();
            tx.close();
            v
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol }
                if c_symbol == "hew_channel_recv"
        )),
        "expected hew_channel_recv rewrite after deferred resolution, got: {:?}",
        output.method_call_rewrites
    );
}

/// `try_recv()` on a `Receiver<int>` where the element type is constrained
/// after the call site must resolve to the int-specific symbol.
#[test]
fn deferred_channel_try_recv_int_constrained_after_call() {
    let output = typecheck_inline(
        r"
        import std::channel::channel;

        fn try_take() -> Option<int> {
            let (tx, rx) = channel.new(4);
            let v: Option<int> = rx.try_recv();
            tx.close();
            v
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol }
                if c_symbol == "hew_channel_try_recv_int"
        )),
        "expected hew_channel_try_recv_int rewrite after deferred resolution, got: {:?}",
        output.method_call_rewrites
    );
}

/// `send()` must defer when both the channel inner type and the sent value are
/// still `Ty::Var` at the call site, then pick the int-specific symbol once a
/// later `recv()` annotation constrains the shared channel type.
#[test]
fn deferred_channel_send_int_constrained_after_call() {
    let output = typecheck_inline(
        r"
        import std::channel::channel;

        fn relay() {
            let (tx, rx) = channel.new(4);
            if let Some(v) = rx.recv() {
                tx.send(v);
            }
            let _: Option<int> = rx.recv();
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol }
                if c_symbol == "hew_channel_send_int"
        )),
        "expected hew_channel_send_int rewrite after deferred resolution, got: {:?}",
        output.method_call_rewrites
    );
}

/// When neither send nor recv arguments or annotations constrain the inner type
/// before the checker boundary, `finalize_channel_rewrites` must emit an
/// `InferenceFailed` error rather than silently recording the wrong symbol.
#[test]
fn deferred_channel_unresolved_inner_fails_closed() {
    let output = typecheck_inline(
        r"
        import std::channel::channel;

        fn untyped() {
            let (tx, rx) = channel.new(4);
            let _ = rx.recv();
            tx.close();
        }
        ",
    );
    // Must produce InferenceFailed — T is genuinely unconstrained.
    assert!(
        output.errors.iter().any(|e| {
            e.kind == TypeErrorKind::InferenceFailed && e.message.contains("inner type")
        }),
        "expected InferenceFailed for unresolved channel inner type, got: {:#?}",
        output.errors
    );
    // The span must NOT have a rewrite recorded (codegen-fails-closed invariant).
    assert!(
        !output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol }
                if c_symbol.contains("hew_channel_recv")
        )),
        "no recv rewrite should be recorded when inner type is unresolved: {:?}",
        output.method_call_rewrites
    );
}

use std::fs;
use std::path::{Path, PathBuf};

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

// ── Generic passthrough: non-Copy type param returns ─────────────────────────

/// Returning an unbounded generic param is unsafe when instantiated with
/// a ref-counted type — the checker catches this at definition time.
#[test]
fn rc_generic_passthrough_unbounded_errors() {
    let output = typecheck_inline(
        r"
        fn id<T>(x: T) -> T {
            x
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
        "returning non-Copy generic param should emit BorrowedParamReturn, got errors: {:#?}",
        output.errors
    );
    // Verify the error message mentions the type parameter, not "Rc".
    assert!(
        rc_errors[0].message.contains("non-Copy parameter"),
        "message should describe non-Copy param, got: {}",
        rc_errors[0].message
    );
}

/// Generic param with explicit Copy bound is safe — no error expected.
#[test]
fn rc_generic_copy_bounded_no_error() {
    let output = typecheck_inline(
        r"
        fn id<T: Copy>(x: T) -> T {
            x
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
        "Copy-bounded generic param should not error, got: {rc_errors:#?}",
    );
}

/// Where-clause `T: Copy` bound also exempts the parameter from the check.
#[test]
fn rc_generic_where_clause_copy_no_error() {
    let output = typecheck_inline(
        r"
        fn id<T>(x: T) -> T where T: Copy {
            x
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
        "where-clause Copy-bounded generic param should not error, got: {rc_errors:#?}",
    );
}

/// Returning a non-Copy generic param via explicit `return` triggers the error.
#[test]
fn rc_generic_passthrough_explicit_return_errors() {
    let output = typecheck_inline(
        r"
        fn maybe<T>(x: T, flag: bool) -> T {
            if flag {
                return x;
            }
            x
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
        2,
        "both the explicit return and trailing expr should fire, got: {rc_errors:#?}",
    );
}

/// Wrapping a non-Copy generic param in `Some()` should also be caught.
#[test]
fn rc_generic_passthrough_some_wrap_errors() {
    let output = typecheck_inline(
        r"
        fn wrap<T>(x: T) -> Option<T> {
            Some(x)
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
        "wrapping non-Copy generic param in Some should error, got errors: {:#?}",
        output.errors
    );
}

/// Multiple type params — only non-Copy ones should be flagged.
#[test]
fn rc_generic_mixed_bounds_selective_error() {
    let output = typecheck_inline(
        r"
        fn pick<A: Copy, B>(a: A, b: B) -> B {
            b
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
        "only non-Copy param `b` should fire, got: {rc_errors:#?}",
    );
    assert!(
        rc_errors[0].message.contains("`b`"),
        "error should mention param `b`, got: {}",
        rc_errors[0].message
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

/// Generic param stored via method call: v.push(x) where x: T (non-Copy).
#[test]
fn rc_generic_method_call_store_errors() {
    let output = typecheck_inline(
        r"
        fn bad<T>(x: T) -> Vec<T> {
            var v = Vec::new();
            v.push(x);
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
        "v.push(x) for non-Copy T then return v must fire, got: {:#?}",
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

// ── Known limitations of BorrowedParamReturn ────────────────────────────────────
//
// The following patterns are NOT caught by the current syntactic scanner and
// are explicitly deferred to a future escape-analysis pass:
//
// 1. Inter-procedural storage: `let v = wrap(r); return v;` where `wrap`
//    stores `r` into a container.  Requires cross-function analysis.
//
// 2. Deeply nested non-constructor call chains are not caught.
//
// 3. Field-assignment escapes: `s.field = r; return s;` — not yet tracked.
//
// These are tracked as future escape-analysis work.

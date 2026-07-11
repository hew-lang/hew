mod common;

use std::fs;
use std::path::Path;
use std::time::{SystemTime, UNIX_EPOCH};

use common::{
    checker, isolated_checker, parse_and_typecheck_inline, parse_program, repo_root,
    typecheck as typecheck_inline, typecheck_wasm as typecheck_inline_wasm,
};
use hew_parser::ast::{Expr, Item, Stmt};
use hew_types::check::SpanKey;
use hew_types::error::TypeErrorKind;

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

fn assert_single_unknown_return_error(output: &hew_types::TypeCheckOutput, context: &str) {
    assert_eq!(
        output.errors.len(),
        1,
        "{context}: expected exactly one error, got: {:#?}",
        output.errors
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UndefinedType
                && e.message.contains("unknown type `UnknownType`")),
        "{context}: expected a single `unknown type `UnknownType`` error, got: {:#?}",
        output.errors
    );
}

#[test]
fn typecheck_all_examples() {
    let examples_dir = repo_root().join("examples");
    test_directory(&examples_dir, "examples");
}

#[test]
fn typecheck_top_level_networking_demos() {
    for relative in ["examples/http_server.hew", "examples/static_server.hew"] {
        assert_typechecks(&repo_root().join(relative), relative);
    }
}

fn assert_typechecks(path: &Path, label: &str) {
    let source = fs::read_to_string(path).unwrap();
    let program = parse_program(&source);
    let mut checker = checker();
    let output = checker.check_program(&program);
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
fn array_literal_typechecks_against_fixed_array_return() {
    assert_inline_typechecks_cleanly(
        r"
fn one(a: i64) -> [i64; 2] {
    [a, a]
}
",
        "array literal in fixed-array return position",
    );
}

#[test]
fn array_literal_typechecks_against_fixed_array_let_annotation() {
    assert_inline_typechecks_cleanly(
        r"
fn main() {
    let xs: [i64; 2] = [1, 2];
}
",
        "array literal in fixed-array let annotation",
    );
}

#[test]
fn unannotated_array_literal_still_infers_vec() {
    assert_inline_typechecks_cleanly(
        r"
fn take_vec(xs: Vec<i64>) {}

fn main() {
    let xs = [1, 2];
    take_vec(xs);
}
",
        "unannotated array literal should infer Vec<i64>",
    );
}

#[test]
fn fixed_array_literal_length_mismatch_is_targeted() {
    let output = typecheck_inline(
        r"
fn one(a: i64) -> [i64; 2] {
    [a, a, a]
}
",
    );

    assert_eq!(
        output.errors.len(),
        1,
        "length mismatch should emit one targeted diagnostic: {:#?}",
        output.errors
    );
    let error = &output.errors[0];
    assert_eq!(error.kind, TypeErrorKind::ArityMismatch);
    assert!(
        error.message.contains("expected 2") && error.message.contains("found 3"),
        "length mismatch diagnostic should name expected and actual arity: {error:#?}"
    );
}

#[test]
fn array_repeat_matching_length_typechecks_against_fixed_array() {
    assert_inline_typechecks_cleanly(
        r"
fn main() {
    let xs: [i64; 4] = [0; 4];
}
",
        "array repeat with matching literal count should pass the checker",
    );
}

#[test]
fn array_repeat_length_mismatch_is_targeted() {
    let output = typecheck_inline(
        r"
fn main() {
    let xs: [i64; 4] = [0; 2];
}
",
    );

    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::ArityMismatch
                && error.message.contains("repeat length mismatch")
                && error.message.contains("expected 4")
                && error.message.contains("found 2")),
        "repeat length mismatch should emit a targeted arity diagnostic naming both lengths: {:#?}",
        output.errors
    );
}

#[test]
fn array_repeat_named_const_count_matches_declared_length() {
    assert_inline_typechecks_cleanly(
        r"
const N: i64 = 4;

fn main() {
    let xs: [i64; 4] = [0; N];
}
",
        "array repeat with a named const count equal to N should pass the checker",
    );
}

#[test]
fn array_repeat_named_const_count_mismatch_is_rejected() {
    let output = typecheck_inline(
        r"
const N: i64 = 3;

fn main() {
    let xs: [i64; 4] = [0; N];
}
",
    );

    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::ArityMismatch
                && error.message.contains("repeat length mismatch")
                && error.message.contains("expected 4")
                && error.message.contains("found 3")),
        "a named const repeat count differing from N should be rejected: {:#?}",
        output.errors
    );
}

#[test]
fn array_repeat_non_constant_count_in_fixed_array_position_is_rejected() {
    let output = typecheck_inline(
        r"
fn main() {
    let n: i64 = 4;
    let xs: [i64; 4] = [0; n];
}
",
    );

    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::ArityMismatch
                && error.message.contains("compile-time integer")),
        "a runtime (non-constant) repeat count cannot satisfy a fixed-array length and must be \
         rejected: {:#?}",
        output.errors
    );
}

#[test]
fn array_repeat_unannotated_local_count_in_fixed_array_position_is_rejected() {
    // Regression: an unannotated immutable integer literal binding (`let n = 4`)
    // is inserted into the checker's `const_values` map purely to retain
    // literal-coercion semantics. It is NOT a declared compile-time constant,
    // so it must not satisfy a fixed-array length. Before the declared-const
    // provenance gate, `const_eval_env` trusted every `const_values` entry and
    // this program type-checked with zero errors, silently bypassing the
    // non-constant-count rejection policy.
    let output = typecheck_inline(
        r"
fn main() {
    let n = 4;
    let xs: [i64; 4] = [0; n];
}
",
    );

    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::ArityMismatch
                && error.message.contains("compile-time integer")),
        "an unannotated local (literal-coercion entry, not a declared const) cannot satisfy a \
         fixed-array length and must be rejected: {:#?}",
        output.errors
    );
}

#[test]
fn array_repeat_parameter_shadowing_const_is_rejected() {
    let output = typecheck_inline(
        r"
const N: i64 = 4;

fn repeat(N: i64) {
    let xs: [i64; 4] = [0; N];
}
",
    );

    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::ArityMismatch
                && error.message.contains("compile-time integer")),
        "a parameter shadowing a declared const cannot satisfy a fixed-array length: {:#?}",
        output.errors
    );
}

#[test]
fn array_repeat_typed_local_shadowing_const_is_rejected() {
    let output = typecheck_inline(
        r"
const N: i64 = 4;

fn repeat(n: i64) {
    let N: i64 = n;
    let xs: [i64; 4] = [0; N];
}
",
    );

    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::ArityMismatch
                && error.message.contains("compile-time integer")),
        "a typed nonliteral local shadowing a declared const cannot satisfy a fixed-array length: {:#?}",
        output.errors
    );
}

#[test]
fn array_repeat_outer_const_survives_prior_inner_shadow() {
    let output = typecheck_inline(
        r"
const N: i64 = 4;

fn shadows() {
    {
        let N = 2;
    }
}

fn main() {
    let xs: [i64; 4] = [0; N];
}
",
    );

    assert!(
        output
            .errors
            .iter()
            .all(|error| error.kind == TypeErrorKind::Shadowing),
        "the later fixed-array repeat must continue to accept the outer declared const after an \
         earlier inner shadow; only the independent shadowing diagnostic is expected: {:#?}",
        output.errors
    );
}

#[test]
fn unannotated_array_repeat_with_runtime_count_still_infers_vec() {
    assert_inline_typechecks_cleanly(
        r"
fn take_vec(xs: Vec<i64>) {}

fn main() {
    let n: i64 = 4;
    let xs = [0; n];
    take_vec(xs);
}
",
        "an unannotated array repeat with a runtime count should still infer Vec<i64> unaffected",
    );
}

#[test]
fn fixed_array_literal_element_mismatch_cites_element() {
    let output = typecheck_inline(
        r"
fn one(a: string) -> [i64; 2] {
    [a, a]
}
",
    );

    assert!(
        output.errors.iter().any(|error| matches!(
            &error.kind,
            TypeErrorKind::Mismatch { expected, actual }
                if expected == "i64" && actual == "string"
        )),
        "element mismatch should cite the element type mismatch: {:#?}",
        output.errors
    );
}

#[test]
fn hashmap_remove_typechecks_as_option() {
    assert_inline_typechecks_cleanly(
        r#"
fn main() {
    let m: HashMap<string, i64> = HashMap::new();
    m.insert("a", 1);
    let removed: Option<i64> = m.remove("a");
    let missing: Option<i64> = m.remove("a");
    match removed {
        Some(_) => match missing {
            Some(_) => {}
            None => println("ok"),
        },
        None => {}
    }
}
"#,
        "HashMap.remove should typecheck as Option<V>",
    );
}

#[test]
fn hashmap_remove_no_longer_typechecks_as_bool() {
    let output = typecheck_inline(
        r#"
fn main() {
    let m: HashMap<string, i64> = HashMap::new();
    m.insert("a", 1);
    let removed: bool = m.remove("a");
}
"#,
    );
    assert!(
        output.errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrorKind::Mismatch { expected, actual }
                if expected == "bool" && actual == "Option<i64>"
        )),
        "expected HashMap.remove bool/Option<i64> mismatch, got: {:#?}",
        output.errors
    );
}

#[test]
fn method_call_receiver_kinds_record_named_type_instance_dispatch() {
    let output = typecheck_inline(
        r"
type Widget {
    value: i64;
}

impl Widget {
    fn value_plus_one(w: Widget) -> i64 {
        w.value + 1
    }
}

fn use_widget(w: Widget) -> i64 {
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
    fn greet(g: Self) -> string;
}

type Bot {
    name: string;
}

impl Greeter for Bot {
    fn greet(bot: Bot) -> string {
        bot.name
    }
}

fn use_greeter(g: dyn Greeter) -> string {
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

fn close_conn(conn: net.Connection) {
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
fn consume(s: Stream<string>) {
    let _ = s.recv();
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
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol, .. }
                if c_symbol == "hew_stream_next_layout"
        )),
        "expected checker-owned builtin rewrite metadata, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn method_call_stream_take_fails_closed_with_honest_diagnostic() {
    // `take` shares the lazy-adapter capability boundary with `map`/`filter`:
    // it previously recorded a `DeferToLowering` rewrite that dead-ended in HIR
    // lowering with an internal-shaped `NotYetImplemented` note. Per issue #2530
    // it now fails closed at the checker with one honest diagnostic.
    let output = typecheck_inline(
        r"
fn consume(s: Stream<bytes>) {
    let _ = s.take(1);
}
",
    );
    let adapter_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| matches!(&e.kind, TypeErrorKind::StreamAdapterNotSupported { .. }))
        .collect();
    assert_eq!(
        adapter_errors.len(),
        1,
        "expected exactly one StreamAdapterNotSupported diagnostic, got: {:#?}",
        output.errors
    );
    assert!(
        matches!(
            &adapter_errors[0].kind,
            TypeErrorKind::StreamAdapterNotSupported { method, element_ty }
                if method == "take" && element_ty == "bytes"
        ),
        "expected `take`/`bytes` in the diagnostic, got: {:?}",
        adapter_errors[0].kind
    );
    assert!(
        !output
            .method_call_rewrites
            .values()
            .any(|rewrite| matches!(rewrite, hew_types::MethodCallRewrite::DeferToLowering)),
        "fail-closed adapters must record no DeferToLowering rewrite, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn method_call_stream_map_fails_closed_with_honest_diagnostic() {
    // The lazy stream adapters (`take`/`map`/`filter`) have no MIR lowering.
    // They previously type-checked clean and recorded `StreamInstance` receiver
    // metadata before dead-ending in HIR lowering with internal-shaped
    // `E_NOT_YET_IMPLEMENTED` noise. Per issue #2530 they now fail closed at the
    // checker with exactly one honest `StreamAdapterNotSupported` diagnostic and
    // record no receiver-kind metadata.
    let output = typecheck_inline(
        r"
fn consume(s: Stream<string>) {
    let _ = s.map(|item| item);
}
",
    );
    let adapter_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| matches!(&e.kind, TypeErrorKind::StreamAdapterNotSupported { .. }))
        .collect();
    assert_eq!(
        adapter_errors.len(),
        1,
        "expected exactly one StreamAdapterNotSupported diagnostic, got: {:#?}",
        output.errors
    );
    assert!(
        matches!(
            &adapter_errors[0].kind,
            TypeErrorKind::StreamAdapterNotSupported { method, element_ty }
                if method == "map" && element_ty == "string"
        ),
        "expected `map`/`string` in the diagnostic, got: {:?}",
        adapter_errors[0].kind
    );
    assert!(
        !output
            .method_call_receiver_kinds
            .values()
            .any(|kind| matches!(
                kind,
                hew_types::MethodCallReceiverKind::StreamInstance { .. }
            )),
        "fail-closed adapters must record no StreamInstance metadata, got: {:?}",
        output.method_call_receiver_kinds
    );
}

#[test]
fn method_call_stream_filter_fails_closed_with_honest_diagnostic() {
    let output = typecheck_inline(
        r"
fn consume(s: Stream<bytes>) {
    let _ = s.filter(|item| item.len() > 0);
}
",
    );
    let adapter_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| matches!(&e.kind, TypeErrorKind::StreamAdapterNotSupported { .. }))
        .collect();
    assert_eq!(
        adapter_errors.len(),
        1,
        "expected exactly one StreamAdapterNotSupported diagnostic, got: {:#?}",
        output.errors
    );
    assert!(
        matches!(
            &adapter_errors[0].kind,
            TypeErrorKind::StreamAdapterNotSupported { method, element_ty }
                if method == "filter" && element_ty == "bytes"
        ),
        "expected `filter`/`bytes` in the diagnostic, got: {:?}",
        adapter_errors[0].kind
    );
    assert!(
        !output
            .method_call_receiver_kinds
            .values()
            .any(|kind| matches!(
                kind,
                hew_types::MethodCallReceiverKind::StreamInstance { .. }
            )),
        "fail-closed adapters must record no StreamInstance metadata, got: {:?}",
        output.method_call_receiver_kinds
    );
}

#[test]
fn method_call_dispatches_resource_wrapper_through_impl() {
    let output = typecheck_inline(
        r#"
import std::net::http;

fn respond(req: http.Request) -> i64 {
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
        output.method_call_rewrites.is_empty(),
        "resource-wrapper methods must dispatch through their impl body, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn module_qualified_call_rewrites_record_registry_c_symbol_metadata() {
    let output = typecheck_inline(
        r#"
import std::fs;

fn main() {
    let _ = fs.exists("test.txt");
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
            hew_types::MethodCallRewrite::RewriteModuleQualifiedToFunction { c_symbol, .. }
                if c_symbol == "hew_file_exists"
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
    value: i64;
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
    var total: i64;

    receive fn set(v: i64) {
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

    var d: i64 = 0;
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
    // i32 and i64 should be marked signed.
    let signed_count = output
        .assign_target_shapes
        .values()
        .filter(|s| !s.is_unsigned)
        .count();
    assert_eq!(
        signed_count, 2,
        "expected 2 signed targets (i32, i64), got: {:?}",
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
    value: i64;
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
    var total: i64;

    receive fn set(v: i64) {
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
    let mut parse_errors = Vec::new();
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
        let program = match hew_parser::parse(&source) {
            parse_result if parse_result.errors.is_empty() => {
                parse_ok += 1;
                parse_result.program
            }
            parse_result => {
                parse_fail += 1;
                parse_errors.push(format!(
                    "  {} — parse errors: {:#?}",
                    path.file_name().unwrap().to_string_lossy(),
                    parse_result.errors
                ));
                continue;
            }
        };

        let mut checker = isolated_checker();
        let output = checker.check_program(&program);
        if output.errors.is_empty() {
            tc_ok += 1;
        } else {
            tc_fail += 1;
            let first_err = &output.errors[0];
            tc_errors.push(format!(
                "  {} — {:?}: {}",
                path.file_name().unwrap().to_string_lossy(),
                first_err.kind,
                first_err.message
            ));
        }
    }

    let total = parse_ok + parse_fail;
    println!(
        "\n{label}: {parse_ok}/{total} parsed, {tc_ok}/{parse_ok} type-checked ({tc_fail} failed)"
    );
    assert_eq!(
        parse_fail,
        0,
        "{label}: expected every fixture to parse; failures:\n{}",
        parse_errors.join("\n")
    );
    // Informational — don't fail on type-check errors yet.
    if !tc_errors.is_empty() {
        println!("Type-check failures:\n{}", tc_errors.join("\n"));
    }
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
    // A function whose parameter is explicitly spelled `stream.Sink<string>`.
    // Proves: the qualified spelling resolves to the canonical Sink<string>
    // type and its send/close methods are available.
    let output = typecheck_inline(
        r"
        import std::stream;

        fn flush_and_close(s: stream.Sink<string>, msg: string) {
            s.send(msg);
            s.close();
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "stream.Sink<string> annotation should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

#[test]
fn stream_dot_stream_int_element_now_admitted() {
    // The element-layout witness widened Stream<T> beyond string/bytes:
    // i64 elements ride the Plain envelope and must typecheck cleanly.
    let output = typecheck_inline(
        r"
        import std::stream;

        fn close_numbers(s: stream.Stream<i64>) {
            s.close();
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "Stream<i64> must be admitted by the layout witness, got: {:#?}",
        output.errors
    );
}

#[test]
fn stream_dot_stream_container_element_reports_user_facing_type() {
    // Container elements have no clone/drop thunk path — the fail-closed
    // diagnostic must name the user-facing element type.
    let output = typecheck_inline(
        r"
        import std::stream;

        fn close_rows(s: stream.Stream<Vec<i64>>) {
            s.close();
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("`Stream<Vec<i64>>` is not supported")),
        "expected Stream<Vec<i64>> fail-closed diagnostic, got: {:#?}",
        output.errors
    );
}

#[test]
fn stream_decode_fails_closed_before_codegen() {
    let output = typecheck_inline(
        r"
        import std::stream;

        #[wire]
        type Message {
            id: i64 @1;
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

        #[wire]
        type Message {
            id: i64 @1;
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
    // A function whose parameter is explicitly spelled `channel.Receiver<string>`.
    // Proves: the qualified spelling resolves to the canonical Receiver<string>
    // type and its recv/close methods are available.
    let output = typecheck_inline(
        r"
        import std::channel;

        fn take_one(rx: channel.Receiver<string>) -> Option<string> {
            let v = rx.recv();
            rx.close();
            v
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "channel.Receiver<string> annotation should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

// ===========================================================================
// for-await fail-closed tests
// These cover the typechecker's new is_await validation in Stmt::For.
// ===========================================================================

/// `for await item in rx` over `Receiver<string>` must typecheck cleanly.
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
        "for await over Receiver<string> should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

/// `for await val in rx` over `Receiver<i64>` must typecheck cleanly.
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
        "for await over Receiver<i64> should typecheck cleanly, got: {:#?}",
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

/// `for await item in rx` over `Receiver<Foo>` (a `BitCopy` record) rides the
/// element-layout witness and must typecheck cleanly.
#[test]
fn for_await_receiver_record_element_admitted() {
    let output = typecheck_inline(
        r"
        import std::channel::channel;

        type Foo { x: i64 }

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
        output.errors.is_empty(),
        "Receiver<Foo> for-await must be admitted by the layout witness, got: {:#?}",
        output.errors
    );
}

/// `for await item in rx` over a container element (`Receiver<Vec<i64>>`)
/// must fail closed — the witness cannot clone or drop a container element.
#[test]
fn for_await_receiver_container_element_errors() {
    let output = typecheck_inline(
        r"
        import std::channel::channel;

        fn main() {
            let (tx, rx): (channel.Sender<Vec<i64>>, channel.Receiver<Vec<i64>>) =
                channel.new(4);
            for await item in rx {
                println(item.len());
            }
        }
        ",
    );
    assert!(
        output.errors.iter().any(
            |e| e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("not supported")
        ),
        "expected InvalidOperation for Receiver<Vec<i64>> in for await, got: {:#?}",
        output.errors
    );
}

/// `for await item in input` over `Stream<Row>` (a `BitCopy` record) rides the
/// element-layout witness and must typecheck cleanly.
#[test]
fn for_await_stream_record_element_admitted() {
    let output = typecheck_inline(
        r#"
        import std::stream;

        type Row { value: i64 }

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
        output.errors.is_empty(),
        "Stream<Row> for-await must be admitted by the layout witness, got: {:#?}",
        output.errors
    );
}

/// `for await item in input` over a container element (`Stream<Vec<i64>>`)
/// must fail closed at the stream element validation boundary.
#[test]
fn for_await_stream_container_element_errors() {
    let output = typecheck_inline(
        r#"
        import std::stream;

        extern "C" {
            fn fake_stream() -> Stream<Vec<i64>>;
        }

        fn main() {
            let input = unsafe { fake_stream() };
            for await rows in input {
                println("seen");
            }
        }
        "#,
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("`Stream<Vec<i64>>` is not supported")
        }),
        "expected InvalidOperation for Stream<Vec<i64>> in for await, got: {:#?}",
        output.errors
    );
}

/// Unsupported first-class `Stream<T>` element types in `for await` must fail
/// closed without cascading into loop-body field/type errors.
#[test]
fn for_await_stream_unsupported_type_does_not_cascade() {
    let output = typecheck_inline(
        r#"
        extern "C" {
            fn fake_stream() -> Stream<Vec<i64>>;
        }

        fn main() {
            let input = unsafe { fake_stream() };
            for await rows in input {
                println(rows.missing);
            }
        }
        "#,
    );
    assert_eq!(
        output.errors.len(),
        1,
        "expected only the fail-closed Stream<Vec<i64>> error, got: {:#?}",
        output.errors
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message.contains("`Stream<Vec<i64>>` is not supported")
        }),
        "expected InvalidOperation for Stream<Vec<i64>> in for await, got: {:#?}",
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
            receive gen fn count_up() -> i64 {
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
        "for await over receive gen Stream<i64> should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

/// Actor method calls in `for await` must target `receive gen fn`, even if the
/// method's return type is `Stream<T>`.
#[test]
fn for_await_actor_method_stream_requires_receive_gen() {
    let output = typecheck_inline(
        r#"
        extern "C" { fn fake_stream() -> Stream<string>; }

        actor Reader {
            receive fn lines() -> Stream<string> {
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
            let v: Vec<i64> = Vec::new();
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

        fn consume(rx: channel.Receiver<string>) {
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
            let rc: Rc<i64> = Rc::new(42);
            println(rc.get());
            let rc2 = rc.clone();
            println(rc2.get());
            println(rc.strong_count());
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "Rc<i64> basic usage should type-check cleanly, got: {:#?}",
        output.errors
    );
}

/// `Rc<T>` must be rejected when sent across an actor boundary (non-Send).
#[test]
fn rc_rejected_at_actor_send_boundary() {
    let output = typecheck_inline(
        r"
        actor Sink {
            let _unused: i64;
            receive fn consume(val: Rc<i64>) {}
        }
        fn main() {
            let rc: Rc<i64> = Rc::new(1);
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
        "Rc<i64> must be rejected at actor send boundary with InvalidSend, got: {:#?}",
        output.errors
    );
}

#[test]
fn lambda_actor_capture_must_be_send() {
    // `actor move |params| { body }` captures are checked for Send; Rc<i64> is not Send.
    let output = typecheck_inline(
        r"
        fn main() {
            let rc: Rc<i64> = Rc::new(1);
            let worker = actor move |x: i64| {
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
        "actor lambda must reject non-Send captures, got: {:#?}",
        output.errors
    );
}

#[test]
fn lambda_actor_call_rejects_non_send_payload() {
    // Call-syntax dispatch on a lambda actor rejects non-Send arguments at the call site
    // (E_DUPLEX_NON_SEND): the message crosses an actor boundary.
    let output = typecheck_inline(
        r"
        fn main() {
            let worker = actor |msg: i64| {
                println(msg);
            };
            let rc: Rc<i64> = Rc::new(1);
            worker(rc.strong_count());
        }
        ",
    );
    // rc.strong_count() returns i64 which IS Send — this should be clean.
    // The earlier version of this test used a non-Send type as the declared param type,
    // which is caught at actor-definition time (E_DUPLEX_NON_SEND on the param).
    // That check is now in actor_lambda_non_send_param_rejected.
    assert!(
        output.errors.is_empty(),
        "call-syntax with Send i64 payload should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

#[test]
fn lambda_actor_non_send_param_rejected() {
    // A lambda actor whose parameter type is not Send is rejected at definition time
    // (E_DUPLEX_NON_SEND): the message type must be Send to cross the actor boundary.
    let output = typecheck_inline(
        r"
        fn main() {
            let worker = actor |msg: Rc<i64>| {
                println(msg.strong_count());
            };
            worker(42);
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| {
                e.kind == hew_types::error::TypeErrorKind::InvalidSend
                    && e.message.contains("E_DUPLEX_NON_SEND")
            }),
        "lambda actor with non-Send param type must reject with InvalidSend containing E_DUPLEX_NON_SEND, got: {:#?}",
        output.errors
    );
}

#[test]
fn actor_ref_send_method_requires_send_payload() {
    // Previously this test asserted `InvalidSend` for a non-Send `Rc<i64>`
    // payload.  After the anonymous-payload send surface was formally retired
    // (issue #2122), `sink.send(rc)` is rejected *earlier* — at the
    // `UndefinedMethod` gate that fires when the actor has no `receive fn send`
    // handler — so the `InvalidSend` check never runs.  The payload-Send bound
    // no longer matters for this shape; the whole call is rejected.  Confirm
    // that the call is indeed rejected with `UndefinedMethod` (actionable
    // diagnostic: "add `receive fn send(...)` or use a named handler").
    let output = typecheck_inline(
        r"
        actor Sink {
            let _unused: i64;
        }

        fn main() {
            let sink = spawn Sink(_unused: 0);
            let rc: Rc<i64> = Rc::new(1);
            sink.send(rc);
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::UndefinedMethod),
        "anonymous `.send()` on an actor without `receive fn send` must produce \
         UndefinedMethod (not InvalidSend) — the call is rejected at the handler \
         gate before Send-ness is checked. got: {:#?}",
        output.errors
    );
}

/// Regression test for F-02: `receive fn -> Option<i64>` must typecheck without
/// `E_DUPLEX_NON_SEND`. Before the fix, `Option` fell through the marker-derivation
/// arms to `type_fields.get("Option") → None → false`, so every ask-shaped receive fn
/// returning `Option<T>` was incorrectly rejected.
#[test]
fn actor_receive_fn_option_reply_accepted() {
    let output = typecheck_inline(
        r"
        actor Queue {
            let items: Vec<i64>;
            receive fn dequeue() -> Option<i64> {
                None
            }
        }

        fn main() {
            let q = spawn Queue(items: Vec::new());
            let _item = await q.dequeue();
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "actor receive fn returning Option<i64> must typecheck cleanly (F-02 regression), got: {:#?}",
        output.errors
    );
}

/// The `<-` send operator was removed in v0.5.  Both the lexer token and the
/// parser infix rule are gone; any source using `<-` now fails to parse.
/// This is the `E_OPERATOR_REMOVED` reject path (§3.3 of the migration guide).
#[test]
fn left_arrow_send_operator_is_rejected() {
    let result = hew_parser::parse(
        r"
        fn main() {
            let worker = actor |msg: i64| { println(msg); };
            worker <- 1;
        }
        ",
    );
    assert!(
        !result.errors.is_empty(),
        "`<-` must be rejected by the parser (E_OPERATOR_REMOVED), got clean parse"
    );
}

/// The legacy `spawn (...) => body` syntax was removed in v0.5.
/// The parser now emits `E_SPAWN_LAMBDA_SYNTAX_REMOVED`.  Accept path uses the
/// new `actor |...| { ... }` form.
#[test]
fn legacy_spawn_lambda_syntax_is_rejected() {
    let result = hew_parser::parse(
        r"
        fn main() {
            let worker = spawn (msg: i64) => { println(msg); };
        }
        ",
    );
    assert!(
        !result.errors.is_empty(),
        "`spawn (...) => ...` must be rejected by the parser (E_SPAWN_LAMBDA_SYNTAX_REMOVED)"
    );
    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("E_SPAWN_LAMBDA_SYNTAX_REMOVED")),
        "expected E_SPAWN_LAMBDA_SYNTAX_REMOVED in error messages, got: {:#?}",
        result.errors
    );
}

/// Accept path for the new `actor |...| { ... }` syntax with call-syntax dispatch.
/// Replaces the old `lambda_actor_send_operator_allows_send_payload` test.
#[test]
fn actor_lambda_new_syntax_typechecks() {
    let output = typecheck_inline(
        r"
        fn main() {
            let worker = actor |msg: i64| {
                println(msg);
            };
            worker(1);
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "`actor |...| {{ ... }}` with call-syntax dispatch should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

/// Ask-shaped lambda actor: accept path — body type matches declared return type.
#[test]
fn ask_shaped_actor_return_matches_typechecks() {
    let output = typecheck_inline(
        r"
        fn main() {
            let doubler = actor |n: i64| -> i64 {
                n * 2
            };
            doubler(5);
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "ask-shaped actor with matching return type should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

/// `E_LAMBDA_RETURN_TYPE_MISMATCH`: ask-shaped actor body return type ≠ declared reply type.
#[test]
fn ask_shaped_actor_return_mismatch_rejected() {
    let output = typecheck_inline(
        r#"
        fn main() {
            let bad = actor |n: i64| -> i64 {
                "not an i64"
            };
            bad(1);
        }
        "#,
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::ReturnTypeMismatch
                && e.message.contains("E_LAMBDA_RETURN_TYPE_MISMATCH")),
        "ask-shaped actor with mismatched body type must emit E_LAMBDA_RETURN_TYPE_MISMATCH, got: {:#?}",
        output.errors
    );
}

/// `E_LAMBDA_SELF_ESCAPE`: an actor lambda body that returns a Duplex handle is rejected.
/// A Duplex (lambda-actor handle) escaping via the body's return value violates the
/// handle's lifetime bound to the let-binding site.
#[test]
fn actor_lambda_self_escape_rejected() {
    // The inner actor is returned by the outer actor's body, leaking its handle.
    // The outer actor's body return type would be Duplex<...>, triggering E_LAMBDA_SELF_ESCAPE.
    let output = typecheck_inline(
        r"
        fn helper() {
            let inner = actor |x: i64| {
                println(x);
            };
            // This outer actor's body returns a Duplex handle — the self-escape pattern.
            let _outer = actor |_n: i64| -> Duplex<i64, ()> {
                inner
            };
        }
        ",
    );
    // The outer actor's body synthesises to Duplex<i64, ()> because `inner` is a
    // Duplex. That triggers E_LAMBDA_SELF_ESCAPE (InvalidOperation) before any
    // return-type-annotation check fires, so the fixture reliably exercises the path.
    let self_escape_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| {
            e.kind == TypeErrorKind::InvalidOperation && e.message.contains("E_LAMBDA_SELF_ESCAPE")
        })
        .collect();
    assert!(
        !self_escape_errors.is_empty(),
        "expected E_LAMBDA_SELF_ESCAPE diagnostic (InvalidOperation), got: {:?}",
        output.errors
    );
}

/// Lambda actor recursive self-call via let-binding name typechecks clean.
/// The let-binding name is the recursion handle (architecture §5.9 ratification 2).
#[test]
fn lambda_actor_recursive_self_call_typechecks() {
    let output = typecheck_inline(
        r"
        fn main() {
            let fib = actor |n: i64| {
                if n > 1 {
                    fib(n - 1);
                }
            };
            fib(10);
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "recursive self-call in actor body must typecheck clean, got: {:#?}",
        output.errors
    );
}

/// Lambda actor handles are `Duplex<Msg, Reply>` under the hood.  Now that
/// `Duplex::send()` is a wired method, calling `.send()` on a lambda actor
/// handle routes through the duplex-method dispatcher and typechecks cleanly
/// (the payload satisfies the Send bound; `i64` is Copy + Send).
///
/// Call-syntax (`worker(1)`) remains the idiomatic surface, but `.send()`
/// is no longer an error: both surfaces resolve to the same runtime symbol
/// and the type system cannot distinguish a lambda-actor Duplex from a
/// raw-duplex Duplex at the method-call site.
#[test]
fn lambda_actor_dot_send_now_accepted_via_duplex_method() {
    let output = typecheck_inline(
        r"
        fn main() {
            let worker = actor |msg: i64| {
                println(msg);
            };
            worker.send(1);
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "`.send()` on a lambda actor handle should typecheck via Duplex::send; got: {:#?}",
        output.errors
    );
}

/// Accept path for tell-shaped lambda actor: `actor |s: string| { ... }` + `log("x")`.
#[test]
fn tell_shaped_actor_typechecks() {
    let output = typecheck_inline(
        r#"
        fn main() {
            let log = actor |s: string| {
                println(s);
            };
            log("x");
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "tell-shaped actor with call-syntax dispatch should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

/// Accept path for ask-shaped lambda actor: `actor |n: i64| -> i64 { n*2 }`.
#[test]
fn ask_shaped_actor_typechecks() {
    let output = typecheck_inline(
        r"
        fn main() {
            let dbl = actor |n: i64| -> i64 { n * 2 };
            let _r = dbl(5);
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "ask-shaped actor with call-syntax dispatch should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

/// `Rc::new` must accept non-Copy `T`; the codegen passes the real drop function.
/// `Rc::get()` must be rejected when `T` is not `Copy` (`LoadOp` semantics).
#[test]
fn rc_non_copy_construction_ok() {
    // string is non-Copy; Rc::new should accept it (codegen will pass a real
    // drop function instead of null).
    let output = typecheck_inline(r#"fn main() { let _rc: Rc<string> = Rc::new("hello"); }"#);
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
            x: i64
            y: i64
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
        typecheck_inline(r#"fn main() { let _rc: Rc<Rc<string>> = Rc::new(Rc::new("hello")); }"#);
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
        "Rc::new with Option<string> should fail closed, got: {:#?}",
        output.errors
    );
}

#[test]
fn rc_owned_struct_payload_rejected() {
    let output = typecheck_inline(
        r#"
        type Labelled {
            name: string
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

/// User `impl Drop` is rejected fail-closed; the rejection fires before the
/// `Rc` payload check, so an `Rc<Token>` over a user-`Drop` payload still
/// fails closed at the unsupported destructor.
#[test]
fn rc_user_drop_payload_rejected() {
    let output = typecheck_inline(
        r"
        type Token {
            id: i64
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
                && e.message
                    .contains("`impl Drop` is not supported (its `drop` method would not run)")
        }),
        "user impl Drop payload should fail closed, got: {:#?}",
        output.errors
    );
}

#[test]
fn rc_owned_payload_annotation_rejected() {
    let output = typecheck_inline(r"fn borrow(_r: Rc<Option<string>>) {}");
    assert!(
        output.errors.iter().any(|e| {
            e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && e.message
                    .contains("does not recursively drop owned contents")
        }),
        "Rc<Option<string>> annotations should fail closed, got: {:#?}",
        output.errors
    );
}

#[test]
fn rc_generic_wrapper_payload_rejected() {
    let output = typecheck_inline(
        r#"
        type Labelled {
            name: string
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
    // Generic lambda `<T>(val: T) -> Rc<T> => ...` was removed in v0.5.
    // The Rc<non-Copy> restriction is still enforced: use a named generic
    // function, which is the correct post-v0.5 spelling.
    let output = typecheck_inline(
        r#"
        type Labelled {
            name: string
        }

        fn wrap<T>(val: T) -> Rc<T> { Rc::new(val) }

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
        "Rc<non-Copy> must be rejected via generic fn (fail-closed), got: {:#?}",
        output.errors
    );
}

#[test]
fn rc_get_non_copy_rejected() {
    // `rc.get()` performs a bitwise copy (LoadOp) which is only sound for
    // Copy types.  Calling it on Rc<string> must be rejected.
    let output = typecheck_inline(
        r#"fn main() { let rc: Rc<string> = Rc::new("hello"); let _ = rc.get(); }"#,
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

        fn inspect(server: http.Server, req: http.Request) -> string {
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
fn wasm_scope_block_rejected_before_codegen() {
    // Edition-2026 scope{} requires the OS-thread-per-task substrate in
    // hew-runtime/src/task_scope.rs, which is `#[cfg(not(target_arch =
    // "wasm32"))]`. The checker must reject a scope-block when the target
    // is wasm32 so the rejection is observable at compile time rather
    // than as a runtime trap or a silent no-op. Tracked at #1451.
    let output = typecheck_inline_wasm(
        r"
        fn main() {
            scope {
                let _ = 42;
            }
        }
        ",
    );
    let count = platform_limitation_error_count(&output, "Structured concurrency scopes");
    assert!(
        count >= 1,
        "expected scope block to be rejected on WASM, got: {:#?}",
        output.errors
    );
}

#[test]
fn wasm_scope_with_fork_child_rejected_before_codegen() {
    // The block-form rejection fires once on the outer scope{}; nested
    // children inside the block do not re-fire because Ty::Error short-
    // circuits the body synthesis. The user-facing outcome is what
    // matters: a scope{} with child tasks is observably rejected on
    // wasm32 with the StructuredConcurrency label naming the substrate
    // dependency.
    let output = typecheck_inline_wasm(
        r"
        fn compute() -> i64 { 7 }
        fn main() {
            scope {
                fork a = compute();
                await a;
            }
        }
        ",
    );
    let count = platform_limitation_error_count(&output, "Structured concurrency scopes");
    assert!(
        count >= 1,
        "expected scope{{}} with children to be rejected on WASM, got: {:#?}",
        output.errors
    );
}

#[test]
fn wasm_tcp_networking_surface_rejected_before_codegen() {
    let output = typecheck_inline_wasm(
        r#"
        import std::net;

        fn tune(listener: net.Listener, conn: net.Connection) -> i64 {
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
            TypeErrorKind::Mismatch { expected, .. } if expected == "string"
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

        fn close_listener(listener: net.Listener) {
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
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol, .. }
                if c_symbol == "hew_tcp_listener_close"
        )),
        "expected net.Listener::close fallback rewrite, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn http_request_close_dispatches_through_resource_impl() {
    let output = typecheck_inline(
        r"
        import std::net::http;

        fn release(req: http.Request) {
            req.close();
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected http.Request::close to resolve through the resource impl, got: {:#?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.is_empty(),
        "resource-wrapper close must not use the opaque-handle fallback rewrite, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn registry_loaded_resource_close_moves_receiver() {
    let output = typecheck_inline(
        r"
        import std::net::http;

        fn release(req: http.Request) {
            req.close();
            req.path();
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::UseAfterMove),
        "registry-loaded resource close must move the receiver, got: {:#?}",
        output.errors
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
    // `write` is now a Hew wrapper function (`write_result_from_status(unsafe {
    // hew_tcp_write(...) })`), not a direct single-call C shim. The stdlib
    // loader's handle-method extractor only recognises single-call shims, so
    // `write` no longer appears in `handle_methods` in the inline typecheck
    // context (no module graph, no `resolved_items` path). As a result, the
    // type checker correctly reports `UndefinedMethod` — the method is simply
    // not visible in this path. The full compilation path (with a module graph
    // that populates `resolved_items`) registers the Hew signature and enforces
    // the `bytes` arg-type constraint; the `tcp_write_backpressure` vertical
    // slice and the refactored examples validate the real call-site behaviour.
    let output = typecheck_inline(
        r#"
        import std::net;

        fn send(conn: net.Connection) {
            conn.write("wrong_type");
        }
        "#,
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::UndefinedMethod),
        "expected UndefinedMethod for conn.write in inline typecheck path \
         (write is a Hew wrapper not a direct C shim), got: {:#?}",
        output.errors
    );
}

#[test]
fn wasm_process_execution_surface_rejected_before_codegen() {
    let output = typecheck_inline_wasm(
        r#"
        import std::process;

        fn await_child(child: process.Child) -> i64 {
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
        import std::string;

        fn parse() -> i64 {
            let value: Option<i64> = string.to_int("9223372036854775807");
            match value {
                Some(n) => n,
                None => 0,
            }
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

        fn manage(child: process.Child) -> i64 {
            let waited: i64 = child.wait();
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
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol, .. }
                if c_symbol == "hew_process_wait"
        )),
        "expected process.Child.wait rewrite, got: {:?}",
        output.method_call_rewrites
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol, .. }
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
            let headers: Vec<(string, string)> = Vec::new();
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
            let headers: Vec<(string, string)> = Vec::new();
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
            resp.close();
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

/// Table-driven: every handle type covered by `reject_if_wasm_native_only_handle`
/// for TLS and QUIC must produce a `PlatformLimitation` error on the wasm32 target.
/// The handles are obtained via `extern "C"` so no module-level constructor call
/// fires the module-qualified-call gate first — the rejection must come solely from
/// the handle-method arm.
#[test]
fn wasm_rejects_all_native_only_handle_methods() {
    // Each row: (import_path, handle_type, extern_fn, method_call, expected_label)
    // method_call must be a valid method on the handle type so method resolution
    // succeeds before `reject_if_wasm_native_only_handle` inspects the receiver.
    let cases: &[(&str, &str, &str, &str, &str)] = &[
        (
            "std::net::tls",
            "tls.TlsStream",
            "fn fake_tls() -> tls.TlsStream",
            "let stream = unsafe { fake_tls() };\n            stream.close();",
            "std::net::tls",
        ),
        (
            "std::net::quic",
            "quic.QUICEndpoint",
            "fn fake_endpoint() -> quic.QUICEndpoint",
            "let ep = unsafe { fake_endpoint() };\n            ep.close();",
            "std::net::quic",
        ),
        (
            "std::net::quic",
            "quic.QUICConnection",
            "fn fake_conn() -> quic.QUICConnection",
            "let conn = unsafe { fake_conn() };\n            let _ = conn.observe();",
            "std::net::quic",
        ),
        (
            "std::net::quic",
            "quic.QUICStream",
            "fn fake_stream() -> quic.QUICStream",
            "let strm = unsafe { fake_stream() };\n            strm.close();",
            "std::net::quic",
        ),
        (
            "std::net::quic",
            "quic.QUICEvent",
            "fn fake_event() -> quic.QUICEvent",
            "let ev = unsafe { fake_event() };\n            ev.close();",
            "std::net::quic",
        ),
    ];

    for (import_path, handle_type, extern_fn, method_call, expected_label) in cases {
        let source = format!(
            r#"
        import {import_path};

        extern "C" {{
            {extern_fn};
        }}

        fn main() {{
            {method_call}
        }}
        "#,
        );
        let output = typecheck_inline_wasm(&source);
        assert!(
            output.errors.iter().any(|e| {
                e.kind == TypeErrorKind::PlatformLimitation && e.message.contains(expected_label)
            }),
            "expected wasm PlatformLimitation for {handle_type} handle-method, got: {:#?}",
            output.errors
        );
    }
}

/// Native-target parity: the same handle-method calls that are rejected on
/// wasm32 must NOT produce a `PlatformLimitation` error when type-checked
/// without the wasm target.  A regression that over-blocks on native would
/// pass the wasm tests above while silently breaking native builds.
#[test]
fn native_allows_all_native_only_handle_methods_no_platform_error() {
    // Mirrors the wasm table above — same programs, checked with `typecheck_inline`
    // (no `enable_wasm_target()` call).
    let cases: &[(&str, &str, &str, &str, &str)] = &[
        (
            "std::net::tls",
            "tls.TlsStream",
            "fn fake_tls() -> tls.TlsStream",
            "let stream = unsafe { fake_tls() };\n            stream.close();",
            "std::net::tls",
        ),
        (
            "std::net::quic",
            "quic.QUICEndpoint",
            "fn fake_endpoint() -> quic.QUICEndpoint",
            "let ep = unsafe { fake_endpoint() };\n            ep.close();",
            "std::net::quic",
        ),
        (
            "std::net::quic",
            "quic.QUICConnection",
            "fn fake_conn() -> quic.QUICConnection",
            "let conn = unsafe { fake_conn() };\n            let _ = conn.observe();",
            "std::net::quic",
        ),
        (
            "std::net::quic",
            "quic.QUICStream",
            "fn fake_stream() -> quic.QUICStream",
            "let strm = unsafe { fake_stream() };\n            strm.close();",
            "std::net::quic",
        ),
        (
            "std::net::quic",
            "quic.QUICEvent",
            "fn fake_event() -> quic.QUICEvent",
            "let ev = unsafe { fake_event() };\n            ev.close();",
            "std::net::quic",
        ),
    ];

    for (import_path, handle_type, extern_fn, method_call, expected_label) in cases {
        let source = format!(
            r#"
        import {import_path};

        extern "C" {{
            {extern_fn};
        }}

        fn main() {{
            {method_call}
        }}
        "#,
        );
        let output = typecheck_inline(&source);
        assert!(
            !output.errors.iter().any(|e| {
                e.kind == TypeErrorKind::PlatformLimitation && e.message.contains(expected_label)
            }),
            "unexpected PlatformLimitation on native target for {handle_type}: {:#?}",
            output.errors
        );
    }
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
    if pat.is_match("hello123") {
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
        fn identity(r: Rc<i64>) -> Rc<i64> {
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
        fn early(r: Rc<i64>, flag: bool) -> Rc<i64> {
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

/// F5: returning an immutable-borrow (`&T`) parameter must be rejected — the
/// borrow's owner lives in the caller's scope and the reference would dangle
/// after the caller drops it. This is the borrow-outlives-owner check in the
/// only form the v0.5 surface can express (return position; there is no
/// borrow-of-local expression).
#[test]
fn borrow_param_explicit_return_errors() {
    let output = typecheck_inline(
        r"
        fn f(x: &string) -> &string {
            return x;
        }
        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn),
        "returning a borrow param should emit BorrowedParamReturn, got errors: {:#?}, warnings: {:#?}",
        output.errors, output.warnings
    );
    // Behavioural-regression guard (LESSONS behavioral-regression-not-just-test-pass):
    // the diagnostic must be the borrow-specific message, not the Rc wording —
    // the borrow rejection is a distinct semantic class.
    assert!(
        output.errors.iter().any(|e| e.kind
            == hew_types::error::TypeErrorKind::BorrowedParamReturn
            && e.message.contains("borrow parameter")
            && e.message.contains("outlive its owner")),
        "borrow return must use the borrow-specific diagnostic, got: {:#?}",
        output.errors
    );
}

/// F5: a borrow param trailing-expr return is also rejected.
#[test]
fn borrow_param_trailing_return_errors() {
    let output = typecheck_inline(
        r"
        fn f(x: &string) -> &string {
            x
        }
        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn),
        "trailing-expr return of a borrow param should emit BorrowedParamReturn, got: {:#?}",
        output.errors
    );
}

/// F5 must not over-reject: a borrow param that is NOT returned is fine. A
/// function may accept `&T` and use it without the escape error firing.
#[test]
fn borrow_param_not_returned_ok() {
    let output = typecheck_inline(
        r"
        fn f(x: &string) -> i64 {
            return 0;
        }
        fn main() {}
        ",
    );
    let borrow_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
        .collect();
    assert!(
        borrow_errors.is_empty(),
        "a borrow param that does not escape must not emit BorrowedParamReturn, got: {borrow_errors:#?}",
    );
}

/// `break <rc_param>` inside a loop must trigger the error — the broken value
/// escapes to the enclosing scope with the same aliasing hazard as `return`.
#[test]
fn rc_param_break_value_errors_borrowed_rc() {
    let output = typecheck_inline(
        r"
        fn escape(r: Rc<i64>) -> Rc<i64> {
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
        fn wrapped(r: Rc<i64>) -> Rc<i64> {
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
        fn safe_identity(r: Rc<i64>) -> Rc<i64> {
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
        fn read_rc(r: Rc<i64>) -> i64 {
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
        fn wrap(r: Rc<i64>) -> Option<Rc<i64>> {
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
        fn wrap(r: Rc<i64>) -> (Rc<i64>, i64) {
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
            val: Rc<i64>,
        }
        fn wrap(r: Rc<i64>) -> Holder {
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
        fn wrap(r: Rc<i64>) -> Option<Rc<i64>> {
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
        fn fresh(_r: Rc<i64>) -> Rc<i64> {
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
        fn wrap(r: Rc<i64>) -> Option<Rc<i64>> {
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
        fn extract(r: Rc<i64>) -> i64 {
            r.get()
        }
        fn delegate(r: Rc<i64>) -> i64 {
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
        fn bad(r: Rc<i64>) -> Vec<Rc<i64>> {
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
        fn bad(r: Rc<i64>) -> Vec<Rc<i64>> {
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
        fn bad(r: Rc<i64>) -> Rc<i64> {
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
        fn bad(r: Rc<i64>) -> Option<Rc<i64>> {
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
        fn bad(r: Rc<i64>) -> Vec<Rc<i64>> {
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
        fn not_returned(r: Rc<i64>) -> i64 {
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
        fn safe(r: Rc<i64>) -> Vec<Rc<i64>> {
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
        fn bad(r: Rc<i64>, cond: bool) -> Vec<Rc<i64>> {
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
        fn bad(r: Rc<i64>) -> Rc<i64> {
            var v: Rc<i64> = Rc::new(0);
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
        fn ok(r: Rc<i64>) -> Vec<i64> {
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
            value: Rc<i64>,
        }
        fn bad(r: Rc<i64>) -> Wrapper {
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
            let v: Vec<[i64; 2]> = Vec::new();
            println(v.len());
        }",
        "`Vec<[i64; 2]>` is not supported",
        "annotated Vec<[i64; 2]>",
    );
}

// NOTE: the former `vec_from_array_elements_rejected` and
// `vec_tuple_with_array_elements_rejected` tests were removed: they constructed
// the array-element case via array LITERALS (`[1, 2]`), which now type as
// `Vec<i64>` (array literals desugar to Vec), so the `Vec<[i64; 2]> is not
// supported` rejection no longer applies to that syntax. The fixed-size-array
// (`[T; N]`) Vec-element rejection is still covered by the annotation-based
// tests below (`vec_nested_vec_array_annotation_rejected`,
// `vec_generic_wrapper_array_annotation_rejected`).

#[test]
fn vec_nested_vec_array_annotation_rejected() {
    assert_invalid_operation_contains(
        r"
        fn main() {
            let v: Vec<Vec<[i64; 2]>> = Vec::new();
            println(v.len());
        }",
        "`Vec<[i64; 2]>` is not supported",
        "annotated Vec<Vec<[i64; 2]>>",
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
            let v: Vec<Box<[i64; 2]>> = Vec::new();
            println(v.len());
        }",
        "`Vec<Box<[i64; 2]>>` is not supported",
        "annotated Vec<Box<[i64; 2]>>",
    );
}

#[test]
fn vec_option_wrapper_array_annotation_rejected() {
    assert_invalid_operation_contains(
        r"
        fn main() {
            let v: Vec<Option<[i64; 2]>> = Vec::new();
            println(v.len());
        }",
        "`Vec<Option<[i64; 2]>>` is not supported",
        "annotated Vec<Option<[i64; 2]>>",
    );
}

#[test]
fn vec_result_wrapper_array_annotation_rejected() {
    assert_invalid_operation_contains(
        r"
        fn main() {
            let v: Vec<Result<[i64; 2], string>> = Vec::new();
            println(v.len());
        }",
        "`Vec<Result<[i64; 2], string>>` is not supported",
        "annotated Vec<Result<[i64; 2], string>>",
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
        "Vec.push(Rc<i64>)",
    );
}

#[test]
fn rc_vec_set_rejected() {
    assert_unsafe_collection_element(
        r"
        fn main() {
            var v: Vec<Rc<i64>> = Vec::new();
            let r = Rc::new(99);
            v.set(0, r);
        }",
        "Vec.set(_, Rc<i64>)",
    );
}

#[test]
fn rc_vec_append_rejected() {
    assert_unsafe_collection_element(
        r"
        fn main() {
            var v: Vec<Rc<i64>> = Vec::new();
            let w: Vec<Rc<i64>> = Vec::new();
            v.append(w);
        }",
        "Vec.append(Vec<Rc<i64>>)",
    );
}

#[test]
fn rc_vec_pop_rejected() {
    assert_unsafe_collection_element(
        r"
        type Holder { v: Vec<Rc<i64>> }
        fn extract(h: Holder) -> Rc<i64> {
            h.v.pop()
        }",
        "Vec.pop() on Vec<Rc<i64>>",
    );
}

#[test]
fn rc_vec_get_rejected() {
    assert_unsafe_collection_element(
        r"
        type Holder { v: Vec<Rc<i64>> }
        fn extract(h: Holder) -> Rc<i64> {
            h.v.get(0)
        }",
        "Vec.get(_) on Vec<Rc<i64>>",
    );
}

#[test]
fn rc_vec_remove_rejected() {
    assert_unsafe_collection_element(
        r"
        type Holder { v: Vec<Rc<i64>> }
        fn extract(h: Holder) -> Rc<i64> {
            h.v.remove(0)
        }",
        "Vec.remove(_) on Vec<Rc<i64>>",
    );
}

#[test]
fn rc_vec_map_rejected() {
    assert_unsafe_collection_element(
        r"
        type Holder { v: Vec<Rc<i64>> }
        fn extract(h: Holder) -> Vec<Rc<i64>> {
            h.v.map(|x: Rc<i64>| x)
        }",
        "Vec.map(_) on Vec<Rc<i64>>",
    );
}

#[test]
fn rc_vec_filter_rejected() {
    assert_unsafe_collection_element(
        r"
        type Holder { v: Vec<Rc<i64>> }
        fn extract(h: Holder) -> Vec<Rc<i64>> {
            h.v.filter(|x: Rc<i64>| x.get() > 0)
        }",
        "Vec.filter(_) on Vec<Rc<i64>>",
    );
}

#[test]
fn rc_vec_fold_rejected() {
    assert_unsafe_collection_element(
        r"
        type Holder { v: Vec<Rc<i64>> }
        fn extract(h: Holder) -> Rc<i64> {
            h.v.fold(Rc::new(0), |acc: Rc<i64>, x: Rc<i64>| x)
        }",
        "Vec.fold(_, _) on Vec<Rc<i64>>",
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
        "HashMap.insert(_, Rc<i64>)",
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
        "HashMap.insert(Rc<i64>, _)",
    );
}

#[test]
fn rc_hashmap_get_value_rejected() {
    assert_unsafe_collection_element(
        r#"
        type Holder {
            items: HashMap<string, Rc<i64>>
        }
        fn leak(h: Holder) -> Option<Rc<i64>> {
            h.items.get("key")
        }"#,
        "HashMap.get() on HashMap<string, Rc<i64>>",
    );
}

#[test]
fn rc_hashmap_remove_value_rejected() {
    assert_unsafe_collection_element(
        r#"
        type Holder {
            items: HashMap<string, Rc<i64>>
        }
        fn remove_key(h: Holder) -> bool {
            h.items.remove("key")
        }"#,
        "HashMap.remove() on HashMap<string, Rc<i64>>",
    );
}

#[test]
fn rc_hashmap_keys_rejected_when_value_type_is_rc() {
    assert_unsafe_collection_element(
        r"
        type Holder {
            items: HashMap<string, Rc<i64>>
        }
        fn leak(h: Holder) -> Vec<string> {
            h.items.keys()
        }",
        "HashMap.keys() on HashMap<string, Rc<i64>>",
    );
}

#[test]
fn rc_hashmap_values_rejected() {
    assert_unsafe_collection_element(
        r"
        type Holder {
            items: HashMap<string, Rc<i64>>
        }
        fn leak(h: Holder) -> Vec<Rc<i64>> {
            h.items.values()
        }",
        "HashMap.values() on HashMap<string, Rc<i64>>",
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
        "HashSet.insert(Rc<i64>)",
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
        "Vec.push(Option<Rc<i64>>)",
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
        "Vec.push((Rc<i64>, i64))",
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
        "Vec<i64> push should be fine",
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
        "Vec<string> push should be fine",
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
        "HashMap<string, string> insert should be fine",
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
        "expected inferred HashSet<i64> insert to typecheck cleanly, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashmap_string_key_index_read_typechecks() {
    let output = typecheck_inline(
        r#"
        fn main() {
            var m: HashMap<string, string> = HashMap::new();
            m.insert("a", "alpha");
            let _x = m["a"];
        }"#,
    );
    assert!(
        output.errors.is_empty(),
        "expected `m[\"a\"]` over HashMap<string, string> to typecheck, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashmap_string_key_index_write_typechecks() {
    let output = typecheck_inline(
        r#"
        fn main() {
            var m: HashMap<string, string> = HashMap::new();
            m["a"] = "alpha";
        }"#,
    );
    assert!(
        output.errors.is_empty(),
        "expected `m[\"a\"] = \"alpha\"` over HashMap<string, string> to typecheck, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashmap_wrong_key_type_index_read_rejected() {
    let output = typecheck_inline(
        r"
        fn main() {
            var m: HashMap<string, string> = HashMap::new();
            let _x = m[5];
        }",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. })),
        "expected a key-type mismatch for `m[5]` on HashMap<string, _>, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashmap_wrong_value_type_index_write_rejected() {
    let output = typecheck_inline(
        r#"
        fn main() {
            var m: HashMap<string, string> = HashMap::new();
            m["a"] = 5;
        }"#,
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. })),
        "expected a value-type mismatch for `m[\"a\"] = 5` on HashMap<_, string>, got: {:#?}",
        output.errors
    );
}

#[test]
fn vec_string_index_still_rejected() {
    let output = typecheck_inline(
        r#"
        fn main() {
            var v: Vec<i64> = Vec::new();
            v.push(1);
            let _x = v["nope"];
        }"#,
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. })),
        "expected Vec index to keep rejecting a string subscript, got: {:#?}",
        output.errors
    );
}

#[test]
fn slice_param_annotation_accepted_as_vec_alias() {
    // `[T]` is now an alias for `Vec<T>`: a function with a `[i32]` param is
    // identical to one typed `Vec<i32>` and must typecheck cleanly.
    let output = typecheck_inline(
        r"
        fn take(xs: [i32]) {}

        fn main() {}",
    );
    assert!(
        output.errors.is_empty(),
        "slice parameter annotation [i32] should be accepted as Vec<i32> alias, got: {:#?}",
        output.errors
    );
}

#[test]
fn loader_registered_module_slice_signature_accepted_as_vec_alias() {
    // Regression test for issue #856. Originally a module exporting `[T]`-typed
    // signatures was rejected at import. Since `[T]` is now an alias for `Vec<T>`,
    // such a module must import cleanly — the slice annotation is lowered to Vec<i32>
    // in the registry loader before any error path is reached.
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
        !output
            .errors
            .iter()
            .any(|e| { e.kind == hew_types::error::TypeErrorKind::UnresolvedImport }),
        "registry-loaded module with [i32] signature should import cleanly as Vec<i32>, got: {:#?}",
        output.errors
    );
}

#[test]
fn hashmap_string_i64_annotation_typechecks_before_codegen() {
    assert_inline_typechecks_cleanly(
        r#"
        fn main() {
            let m: HashMap<string, i64> = HashMap::new();
            m.insert("answer", 42);
            println(m.len());
        }"#,
        "HashMap<string, i64> should stay within the current ABI",
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
        "inferred HashMap<string, string> map literal should stay within the current ABI",
    );
}

#[test]
fn vec_clone_method_typechecks_and_returns_vec() {
    assert_inline_typechecks_cleanly(
        r"
        fn main() {
            let v: Vec<i64> = Vec::new();
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
            let m: HashMap<string, i64> = HashMap::new();
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
            let s: HashSet<string> = HashSet::new();
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
            val: Rc<i64>
        }
        fn main() {
            var v = Vec::new();
            let h = Holder { val: Rc::new(1) };
            v.push(h);
        }",
        "Vec.push(Holder { val: Rc<i64> }) — named struct wrapping Rc should be rejected",
    );
}

#[test]
fn rc_in_named_struct_field_hashmap_value_rejected() {
    assert_unsafe_collection_element(
        r#"
        type Holder {
            val: Rc<i64>
        }
        fn main() {
            var m = HashMap::new();
            let h = Holder { val: Rc::new(1) };
            m.insert("k", h);
        }"#,
        "HashMap.insert(_, Holder { val: Rc<i64> }) — named struct wrapping Rc should be rejected",
    );
}

#[test]
fn rc_in_doubly_nested_named_struct_vec_push_rejected() {
    assert_unsafe_collection_element(
        r"
        type Inner {
            x: Rc<i64>
        }
        type Outer {
            inner: Inner
        }
        fn main() {
            var v = Vec::new();
            let o = Outer { inner: Inner { x: Rc::new(1) } };
            v.push(o);
        }",
        "Vec.push(Outer wrapping Inner wrapping Rc<i64>) — doubly-nested named struct should be rejected",
    );
}

#[test]
fn rc_in_named_enum_variant_vec_push_rejected() {
    assert_unsafe_collection_element(
        r"
        enum MaybeHolder {
            Some(Rc<i64>);
            None;
        }
        fn main() {
            var v = Vec::new();
            v.push(MaybeHolder::Some(Rc::new(1)));
        }",
        "Vec.push(MaybeHolder::Some(Rc<i64>)) — named enum wrapping Rc should be rejected",
    );
}

#[test]
fn plain_named_struct_no_rc_vec_push_ok() {
    assert_no_unsafe_collection_element(
        r"
        type Point {
            x: i64,
            y: i64
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
        fn escape(r: Rc<i64>, opt: Option<i64>) -> Rc<i64> {
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
        fn escape(r: Rc<i64>, opt: Option<i64>) -> Rc<i64> {
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
        fn escape(r: Rc<i64>, opt: Option<i64>) -> Rc<i64> {
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
        fn shadow(r: Rc<i64>) -> Rc<i64> {
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
        fn shadow_then_escape(r: Rc<i64>, opt: Option<i64>) -> Rc<i64> {
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
//    with value types (i64, string, string, structs) but unsound when `T = Rc<U>`.
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
// into codegen.

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
    // Key is constrained (string), value is not.
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
        "expected no errors for HashMap<string, i64>, got: {:#?}",
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
            let m: HashMap<_, string> = HashMap::new();
        }",
    );
    assert!(
        !output.errors.is_empty(),
        "expected error for HashMap<_, string> with unresolved key, got no errors"
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
        "expected no errors for HashSet<string>, got: {:#?}",
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
    // HashMap<_, string> with an unresolved key hole must produce at most one
    // InferenceFailed per span.  finalize_hashmap_admission must not add a
    // second error on top of the inference-holes diagnostic.
    let output = typecheck_inline(
        r"
        fn main() {
            let m: HashMap<_, string> = HashMap::new();
        }",
    );
    assert!(
        !output.errors.is_empty(),
        "expected error for HashMap<_, string> with unresolved key, got no errors"
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
            "duplicate InferenceFailed at span {key} for HashMap<_, string>: {errors:#?}",
            errors = output.errors
        );
    }
}

#[test]
fn hashmap_annotation_val_hole_no_duplicate_inference_failed() {
    // HashMap<string, _> with an unresolved value hole must produce at most one
    // InferenceFailed per span.
    let output = typecheck_inline(
        r"
        fn main() {
            let m: HashMap<string, _> = HashMap::new();
        }",
    );
    assert!(
        !output.errors.is_empty(),
        "expected error for HashMap<string, _> with unresolved value, got no errors"
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
            "duplicate InferenceFailed at span {key} for HashMap<string, _>: {errors:#?}",
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
    // `fn f(x: HashMap<_, string>) {}` must produce exactly one InferenceFailed.
    let output = typecheck_inline("fn f(x: HashMap<_, string>) {}");
    let inference_failed: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::InferenceFailed)
        .collect();
    assert_eq!(
        inference_failed.len(),
        1,
        "expected exactly one InferenceFailed for fn f(x: HashMap<_, string>), \
         got {}: {:#?}",
        inference_failed.len(),
        output.errors
    );
}

#[test]
fn registration_fn_param_hashmap_val_hole_single_error() {
    // `fn f(x: HashMap<string, _>) {}` must produce exactly one InferenceFailed.
    let output = typecheck_inline("fn f(x: HashMap<string, _>) {}");
    let inference_failed: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::InferenceFailed)
        .collect();
    assert_eq!(
        inference_failed.len(),
        1,
        "expected exactly one InferenceFailed for fn f(x: HashMap<string, _>), \
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
    // `[T]` is now a valid Vec<T> alias so it no longer produces Ty::Error.
    // Use `Task<i64>` instead: it is compiler-internal and resolves to Ty::Error
    // via the TaskNotNameable path, which is exactly what triggers pruning.
    let output = typecheck_inline(
        r"
        type Broken {
            value: Task<i64>;
            ok: i64;
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::TaskNotNameable),
        "expected Task<i64> to produce Ty::Error (TaskNotNameable), got: {:#?}",
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
    // `[i64]` is now a valid Vec<i64> alias so it no longer produces Ty::Error.
    // `Vec<[i64; 2]>` fires only a diagnostic — the field type remains the
    // concrete Named type, so pruning does not trigger.
    // Use `Task<i64>` instead: it is compiler-internal and resolve_type_expr
    // returns Ty::Error directly via the TaskNotNameable path, which is exactly
    // what triggers variant pruning.
    let output = typecheck_inline(
        r"
        enum Broken {
            Bad(Task<i64>);
            Good(i64);
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::TaskNotNameable),
        "expected Task<i64> to produce Ty::Error (TaskNotNameable), got: {:#?}",
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
    // `[i64]` is now a valid Vec<i64> alias so it no longer produces Ty::Error.
    // Use `Task<i64>` instead: it is compiler-internal and resolve_type_expr
    // returns Ty::Error directly via the TaskNotNameable path, which is what
    // triggers method pruning.
    let output = typecheck_inline(
        r"
        type Widget {
            value: i64;
        }

        impl Widget {
            fn good(w: Widget) -> i64 {
                w.value
            }

            fn broken(w: Widget, bad: Task<i64>) -> i64 {
                w.value
            }
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::TaskNotNameable),
        "expected Task<i64> to produce Ty::Error (TaskNotNameable), got: {:#?}",
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
    // `[i64]` is now a valid Vec<i64> alias so it no longer produces Ty::Error.
    // Use `Task<i64>` instead: it is compiler-internal and resolve_type_expr
    // returns Ty::Error directly via the TaskNotNameable path, which is what
    // triggers method pruning.
    let output = typecheck_inline(
        r"
        type Widget {
            value: i64;
        }

        impl Widget {
            fn good(w: Widget) -> i64 {
                w.value
            }

            fn broken(w: Widget) -> Task<i64> {
                w.value
            }
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::TaskNotNameable),
        "expected Task<i64> to produce Ty::Error (TaskNotNameable), got: {:#?}",
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
    assert_single_unknown_return_error(&output, "unknown fn return annotation");
}

#[test]
fn lambda_unknown_return_annotation_single_error() {
    let output = typecheck_inline(
        r"
        fn main() {
            let _f = || -> UnknownType { 1 };
        }
        ",
    );
    assert_single_unknown_return_error(&output, "unknown lambda return annotation");
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
    assert_single_unknown_return_error(&output, "unknown receive-fn return annotation");
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

/// `recv()` on a `Receiver<i64>` channel where the element type is inferred
/// from a downstream `let v: i64 = rx.recv()` annotation must still record
/// the layout-witness rewrite (`hew_channel_recv_layout`) once deferred
/// resolution completes — the element kind rides the witness, not the symbol.
#[test]
fn deferred_channel_recv_int_constrained_after_call() {
    let output = typecheck_inline(
        r"
        import std::channel::channel;

        fn take_one() -> Option<i64> {
            let (tx, rx) = channel.new(4);
            let v: Option<i64> = rx.recv();
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
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol, .. }
                if c_symbol == "hew_channel_recv_layout"
        )),
        "expected hew_channel_recv_layout rewrite after deferred resolution, got: {:?}",
        output.method_call_rewrites
    );
    // The retired per-type symbols must never be recorded.
    assert!(
        !output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol, .. }
                if c_symbol == "hew_channel_recv" || c_symbol == "hew_channel_recv_int"
        )),
        "retired per-type recv symbols must not be recorded: {:?}",
        output.method_call_rewrites
    );
}

/// `recv()` on a `Receiver<string>` channel where the element type is inferred
/// from the usage of the received value must emit `hew_channel_recv_layout`.
#[test]
fn deferred_channel_recv_string_constrained_after_call() {
    let output = typecheck_inline(
        r"
        import std::channel::channel;

        fn take_one() -> Option<string> {
            let (tx, rx) = channel.new(4);
            let v: Option<string> = rx.recv();
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
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol, .. }
                if c_symbol == "hew_channel_recv_layout"
        )),
        "expected hew_channel_recv_layout rewrite after deferred resolution, got: {:?}",
        output.method_call_rewrites
    );
}

/// `try_recv()` on a `Receiver<i64>` where the element type is constrained
/// after the call site must resolve to the layout-witness `try_recv` entry.
#[test]
fn deferred_channel_try_recv_int_constrained_after_call() {
    let output = typecheck_inline(
        r"
        import std::channel::channel;

        fn try_take() -> Option<i64> {
            let (tx, rx) = channel.new(4);
            let v: Option<i64> = rx.try_recv();
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
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol, .. }
                if c_symbol == "hew_channel_try_recv_layout"
        )),
        "expected hew_channel_try_recv_layout rewrite after deferred resolution, got: {:?}",
        output.method_call_rewrites
    );
}

/// `send()` must defer when both the channel inner type and the sent value are
/// still `Ty::Var` at the call site, then record the layout-witness send
/// rewrite once a later `recv()` annotation constrains the shared channel type.
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
            let _: Option<i64> = rx.recv();
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
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol, .. }
                if c_symbol == "hew_channel_send_layout"
        )),
        "expected hew_channel_send_layout rewrite after deferred resolution, got: {:?}",
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
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol, .. }
                if c_symbol.contains("hew_channel_recv")
        )),
        "no recv rewrite should be recorded when inner type is unresolved: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn let_propagate_sugar_valid_in_result_fn() {
    // `let r? = expr;` desugars to `let r = expr?;`.  When the RHS is
    // Result<T,E> and the enclosing function also returns Result<_,E>,
    // the type-checker must accept it without errors.  The bound name `r`
    // must have type T (the Ok-payload), not Result<T,E>.
    let output = typecheck_inline(
        r"
        fn make_result(x: i64) -> Result<i64, string> {
            Ok(x * 2)
        }
        fn use_sugar(x: i64) -> Result<i64, string> {
            let r? = make_result(x);
            Ok(r + 1)
        }
        fn main() { use_sugar(5); }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "Expected no errors for valid `let r? = Result<_,_>` in Result-returning fn, got: {:?}",
        output.errors
    );
}

#[test]
fn let_propagate_sugar_typed_annotation_accepted() {
    // `let r?: T = expr;` — the type annotation applies to the unwrapped
    // Ok-payload (T), not to the Result.  The checker must accept this and
    // bind `r` as type T.
    let output = typecheck_inline(
        r"
        fn make_result(x: i64) -> Result<i64, string> {
            Ok(x)
        }
        fn use_typed_sugar(x: i64) -> Result<i64, string> {
            let r?: i64 = make_result(x);
            Ok(r)
        }
        fn main() { use_typed_sugar(3); }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "Expected no errors for `let r?: i64 = Result<i64,_>`, got: {:?}",
        output.errors
    );
}

/// NEW-7: `await stream.recv()` over a `Stream<bytes>` typechecks cleanly — the
/// canonical suspending consumer surface.
#[test]
fn await_stream_recv_bytes_typechecks() {
    let output = typecheck_inline(
        "import std::stream;\n\
         #[opaque]\n\
         type Pair {}\n\
         extern \"C\" {\n\
         \x20   fn hew_stream_channel(capacity: i64) -> Pair;\n\
         \x20   fn hew_stream_pair_stream_bytes(pair: Pair) -> Stream<bytes>;\n\
         \x20   fn hew_stream_pair_free(pair: Pair);\n\
         }\n\
         actor Runner {\n\
         \x20   receive fn go(unused: i64) {\n\
         \x20       let pair = unsafe { hew_stream_channel(4) };\n\
         \x20       let input = unsafe { hew_stream_pair_stream_bytes(pair) };\n\
         \x20       unsafe { hew_stream_pair_free(pair); }\n\
         \x20       let item = await input.recv();\n\
         \x20       match item { Some(v) => {}, None => {}, }\n\
         \x20   }\n\
         }\n\
         fn main() { let r = spawn Runner(); r.go(0); }\n",
    );
    assert!(
        output.errors.is_empty(),
        "await stream.recv() over Stream<bytes> should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

/// NEW-7 widened: an `i64` element rides the element-layout witness — the
/// suspend lowering is no longer bound to string/bytes, so
/// `await stream.recv()` over `Stream<i64>` typechecks cleanly.
#[test]
fn await_stream_recv_int_element_admitted() {
    let output = typecheck_inline(
        "import std::stream;\n\
         #[opaque]\n\
         type Pair {}\n\
         extern \"C\" {\n\
         \x20   fn hew_stream_channel(capacity: i64) -> Pair;\n\
         \x20   fn hew_stream_pair_stream_i64(pair: Pair) -> Stream<i64>;\n\
         \x20   fn hew_stream_pair_free(pair: Pair);\n\
         }\n\
         actor Runner {\n\
         \x20   receive fn go(unused: i64) {\n\
         \x20       let pair = unsafe { hew_stream_channel(4) };\n\
         \x20       let input = unsafe { hew_stream_pair_stream_i64(pair) };\n\
         \x20       unsafe { hew_stream_pair_free(pair); }\n\
         \x20       let item = await input.recv();\n\
         \x20       match item { Some(v) => {}, None => {}, }\n\
         \x20   }\n\
         }\n\
         fn main() { let r = spawn Runner(); r.go(0); }\n",
    );
    assert!(
        output.errors.is_empty(),
        "await stream.recv() over Stream<i64> must be admitted by the \
         element-layout witness, got: {:#?}",
        output.errors
    );
}

// ── WASM gate: std::crypto native-only surfaces ─────────────────────────────

/// `std::crypto::encrypt` module calls are rejected on wasm32 with a structured
/// `PlatformLimitation` diagnostic.  The encrypt and sign modules are backed by
/// native-only staticlib companion crates absent from the wasm32 link set.
#[test]
fn wasm_rejects_crypto_encrypt_module_calls() {
    // The gate fires at the method call level; argument types need not be valid.
    let output = typecheck_inline_wasm(
        "import std::crypto::encrypt;\n\
         fn main() { let _ = encrypt.seal(bytes [0x61], bytes [0x62]); }",
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == hew_types::error::TypeErrorKind::PlatformLimitation
                && e.message.contains("std::crypto::encrypt")
        }),
        "expected PlatformLimitation for encrypt module call on wasm32, got: {:#?}",
        output.errors
    );
}

/// `std::crypto::sign` module calls are rejected on wasm32 with a structured
/// `PlatformLimitation` diagnostic.
#[test]
fn wasm_rejects_crypto_sign_module_calls() {
    let output = typecheck_inline_wasm(
        r"
        import std::crypto::sign;

        fn main() {
            let kp = sign.keypair();
            let msg = bytes [0x01, 0x02, 0x03];
            let _ = sign.sign(msg, kp.private_key);
        }
        ",
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == hew_types::error::TypeErrorKind::PlatformLimitation
                && e.message.contains("std::crypto::sign")
        }),
        "expected PlatformLimitation for sign module call on wasm32, got: {:#?}",
        output.errors
    );
}

/// `std::crypto::crypto.random_bytes` is rejected on wasm32 with a structured
/// `PlatformLimitation` diagnostic. The secure entropy source is native-only
/// and absent from the wasm32 link set.
#[test]
fn wasm_rejects_crypto_random_bytes_module_call() {
    let output = typecheck_inline_wasm(
        "import std::crypto::crypto;\n\
         fn main() { let _ = crypto.random_bytes(32); }",
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == hew_types::error::TypeErrorKind::PlatformLimitation
                && e.message.contains("random_bytes")
        }),
        "expected PlatformLimitation for crypto.random_bytes on wasm32, got: {:#?}",
        output.errors
    );
}

/// Native target parity: the same encrypt/sign module calls that are rejected
/// on wasm32 must NOT produce a `PlatformLimitation` error on the native target.
#[test]
fn native_allows_crypto_encrypt_and_sign_module_calls() {
    let encrypt_output = typecheck_inline(
        "import std::crypto::encrypt;\n\
         fn main() { let _ = encrypt.seal(bytes [0x61], bytes [0x62]); }",
    );
    assert!(
        !encrypt_output
            .errors
            .iter()
            .any(|e| { e.kind == hew_types::error::TypeErrorKind::PlatformLimitation }),
        "encrypt module must not be rejected on native target, got: {:#?}",
        encrypt_output.errors
    );

    let sign_output = typecheck_inline(
        r"
        import std::crypto::sign;

        fn main() {
            let kp = sign.keypair();
            let msg = bytes [0x01, 0x02, 0x03];
            let _ = sign.sign(msg, kp.private_key);
        }
        ",
    );
    assert!(
        !sign_output
            .errors
            .iter()
            .any(|e| { e.kind == hew_types::error::TypeErrorKind::PlatformLimitation }),
        "sign module must not be rejected on native target, got: {:#?}",
        sign_output.errors
    );
}

// ===========================================================================
// Machine channel elements (transition-watch lane, stage-0 pin)
// ===========================================================================

/// A monomorphic machine value as a channel element typechecks clean —
/// machines are enums at the value-classification layer and ride the
/// owned-element queue witness. Was the stage-0 fail-closed pin
/// (`machine_channel_element_currently_fails_closed`) before the machine
/// admission landed.
#[test]
fn monomorphic_machine_channel_element_admitted() {
    let output = typecheck_inline(
        r"
        import std::channel::channel;

        machine Light {
            events {
                Flip;
            }
            state Off;
            state On;
            on Flip: Off => On { On }
            on Flip: On => Off { Off }
        }

        fn main() {
            let (tx, rx): (channel.Sender<Light>, channel.Receiver<Light>) = channel.new(2);
            tx.send(Light::Off);
            tx.close();
            let _ = rx.recv();
            rx.close();
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "monomorphic machine channel element must be admitted; got: {:#?}",
        output.errors
    );
}

/// A GENERIC machine instantiation as a channel element stays refused:
/// the machine substrate canonicalizes instantiations to one bare-named
/// decl layout, so the recv binding's `Option<T>` has no
/// per-instantiation layout to land in. The imported spelling resolves
/// through the generic no-thunk-path clause (the tailored bare-canon
/// clause fires for locally declared generic machines, whose type defs
/// resolve by bare name).
#[test]
fn generic_machine_instantiation_channel_element_refused() {
    let output = typecheck_inline(
        r"
        import std::concurrency::lifecycle;
        import std::channel::channel;

        fn main() {
            let (tx, rx): (channel.Sender<lifecycle.Lifecycle<i64>>, channel.Receiver<lifecycle.Lifecycle<i64>>) = channel.new(2);
            tx.close();
            let _ = rx.recv();
            rx.close();
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("Lifecycle<i64>") && e.message.contains("is not supported")),
        "generic machine instantiation element must be refused; got: {:#?}",
        output.errors
    );
}

/// A machine whose state payload carries a container (`Vec<i64>`) stays
/// refused exactly as a container-bearing enum would — the machine
/// admission rides the same transitive container walk.
#[test]
fn container_bearing_machine_channel_element_refused() {
    let output = typecheck_inline(
        r"
        import std::channel::channel;

        machine Buffered {
            events {
                Load { items: Vec<i64>; }
            }
            state Empty;
            state Loaded { items: Vec<i64>; }
            on Load: Empty => Loaded { Buffered::Loaded { items: event.items } }
            on Load: _ => _ { state }
        }

        fn main() {
            let (tx, rx): (channel.Sender<Buffered>, channel.Receiver<Buffered>) = channel.new(2);
            tx.send(Buffered::Empty);
            tx.close();
            let _ = rx.recv();
            rx.close();
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("is not supported")),
        "container-bearing machine element must stay refused; got: {:#?}",
        output.errors
    );
}

/// Cross-node guard for the local channel-handle transfer surface: a
/// `RemotePid<T>` exposes no receive-fn dispatch, so a channel handle can
/// never ride an actor message across a node boundary through this
/// surface. If `RemotePid` ever grows handler dispatch, its payloads must
/// route through the Serializable enforcement (channel handles are not
/// Serializable) — this pin fails first.
#[test]
fn remote_receive_fn_dispatch_with_channel_handle_refused() {
    let output = typecheck_inline(
        r"
        import std::channel::channel;

        actor Observer {
            receive fn watch(rx: channel.Receiver<string>) {
                rx.close();
            }
        }

        fn forward(o: RemotePid<Observer>, rx: channel.Receiver<string>) {
            o.watch(rx);
        }
        ",
    );
    assert!(
        output.errors.iter().any(|e| e
            .message
            .contains("no method `watch` on `RemotePid<Observer>`")),
        "remote receive-fn dispatch must stay refused; got: {:#?}",
        output.errors
    );
}

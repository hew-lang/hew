mod common;

use hew_parser::ast::Ident;
use std::fs;
use std::time::{SystemTime, UNIX_EPOCH};

use common::{
    parse_and_typecheck_inline, repo_root, typecheck as typecheck_inline,
    typecheck_wasm as typecheck_inline_wasm,
};
use hew_parser::ast::{Expr, Item, Stmt};
use hew_types::check::SpanKey;
use hew_types::error::TypeErrorKind;
use hew_types::{MethodCallRewrite, RcIntrinsicOp};

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
            Item::Function(fd) if fd.name == Ident::new("main") => Some(fd),
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

// A module's `impl Trait for T` blocks are registered when the module is
// loaded, not when the importer names `T` in an explicit `{...}` selection.
// A bare `import std.net;` plus a qualified `net.NetError` therefore
// satisfies the `Display` bound that `to_string` and f-string interpolation
// require, with no `import std.net.{NetError};` line to prop it up.
#[test]
fn bare_module_import_satisfies_the_display_bound_from_the_modules_impl() {
    assert_inline_typechecks_cleanly(
        r#"
import std.net;

fn describe(err: net.NetError) -> string {
    to_string(err)
}

fn interpolate(err: net.NetError) -> string {
    f"{err}"
}
"#,
        "bare module import with a qualified error type",
    );
}

// Counterfactual for the test above: the same import spelling, the same
// module, and the same qualified-type shape, on a type the module declares no
// `impl Display` for, is still refused. Registration follows the module's
// impls, not the import spelling.
#[test]
fn bare_module_import_without_a_display_impl_still_refuses_interpolation() {
    let output = typecheck_inline(
        r#"
import std.net;

fn interpolate(endpoint: net.Endpoint) -> string {
    f"{endpoint}"
}
"#,
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == TypeErrorKind::BoundsNotSatisfied
                && e.message.contains("std.net.Endpoint")
                && e.message.contains("Display")
        }),
        "expected a Display bound refusal for `std.net.Endpoint`, got: {:#?}",
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
        r#"fn main() {
    var m: HashMap<string, i64> = HashMap.new();
    m.insert("a", 1);
    let removed: Option<i64> = m.remove("a");
    let missing: Option<i64> = m.remove("a");
    match removed {
        .Some(_) => match missing {
            .Some(_) => {}
            .None => println("ok"),
        }
        .None => {}
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
    let m: HashMap<string, i64> = HashMap.new();
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
        r"type Widget {
    value: i64;
}

impl Widget {
    fn value_plus_one(self) -> i64 {
        self.value + 1
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
                hew_types::MethodCallReceiverKind::NamedTypeInstance { head }
                    if Some(*head) == output.root_type_head("Widget")
            )),
        "expected named-type method call receiver metadata, got: {:?}",
        output.method_call_receiver_kinds
    );
}

#[test]
fn method_call_receiver_kinds_record_trait_object_dispatch() {
    let output = typecheck_inline(
        r"trait Greeter {
    fn greet(self) -> string;
}

type Bot {
    name: string;
}

impl Greeter for Bot {
    fn greet(self) -> string {
        self.name
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
import std.net;

fn close_conn(consume conn: net.Connection) {
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
                    if type_name == "std.net.Connection"
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
fn method_call_stream_take_reaches_its_runtime_row() {
    // `take` is a lowered adaptor: it consumes its source stream and returns a
    // fresh one through `hew_stream_take`, so it type-checks clean and records
    // a runtime rewrite instead of the adapter capability boundary.
    let output = typecheck_inline(
        r"
fn consume(s: Stream<bytes>) {
    let _ = s.take(1);
}
",
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| matches!(&e.kind, TypeErrorKind::StreamAdapterNotSupported { .. })),
        "`take` must not report the adapter capability boundary, got: {:#?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol, .. }
                if c_symbol == "hew_stream_take"
        )),
        "`take` must record its runtime rewrite, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn method_call_stream_map_fails_closed_with_honest_diagnostic() {
    // The callback adapters (`map`/`filter`) are not part of the pipe surface
    // (D506): there is no type-erased runtime entry for a user callback over
    // a `Stream<T>`. They fail closed at the checker with exactly one
    // ordinary `UndefinedMethod` diagnostic and record no receiver-kind
    // metadata — no special adapter carve-out.
    let output = typecheck_inline(
        r"
fn consume(s: Stream<string>) {
    let _ = s.map(|item| item);
}
",
    );
    let undefined_method_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::UndefinedMethod)
        .collect();
    assert_eq!(
        undefined_method_errors.len(),
        1,
        "expected exactly one UndefinedMethod diagnostic, got: {:#?}",
        output.errors
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
    let _ = s.filter(|item| true);
}
",
    );
    let undefined_method_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::UndefinedMethod)
        .collect();
    assert_eq!(
        undefined_method_errors.len(),
        1,
        "expected exactly one UndefinedMethod diagnostic, got: {:#?}",
        output.errors
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
import std.net.http.server as http;

fn respond(consume req: http.Request) -> i64 {
    let _sent = req.respond_text(200, "ok").is_ok();
    0
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
            MethodCallRewrite::RewriteToFunction { target: hew_types::check::CallTarget::ImplMethod(def), .. }
                if output.defs.path(*def).ends_with("::respond_text")
        )),
        "resource-wrapper methods must dispatch through their impl body, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn assign_target_kinds_record_assignment_target_authority() {
    let output = typecheck_inline(
        r"type Boxed {
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
        output
            .assign_target_kinds
            .keys()
            .filter(|key| key.module_idx == 0)
            .count(),
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
    for key in output.assign_target_kinds.keys() {
        assert!(
            output.assign_target_shapes.contains_key(key),
            "assign_target_kinds span {key:?} is missing from assign_target_shapes",
        );
    }
    for key in output.assign_target_shapes.keys() {
        assert!(
            output.assign_target_kinds.contains_key(key),
            "assign_target_shapes span {key:?} is missing from assign_target_kinds",
        );
    }
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
        output
            .assign_target_shapes
            .keys()
            .filter(|key| key.module_idx == 0)
            .count(),
        4,
        "expected four shape entries, got: {:?}",
        output.assign_target_shapes
    );
    // u8 and u64 should be marked unsigned.
    let unsigned_count = output
        .assign_target_shapes
        .iter()
        .filter(|(key, s)| key.module_idx == 0 && s.is_unsigned)
        .count();
    assert_eq!(
        unsigned_count, 2,
        "expected 2 unsigned targets (u8, u64), got: {:?}",
        output.assign_target_shapes
    );
    // i32 and i64 should be marked signed.
    let signed_count = output
        .assign_target_shapes
        .iter()
        .filter(|(key, s)| key.module_idx == 0 && !s.is_unsigned)
        .count();
    assert_eq!(
        signed_count, 2,
        "expected 2 signed targets (i32, i64), got: {:?}",
        output.assign_target_shapes
    );
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
        import std.stream;

        fn flush_and_close(s: stream.Sink<string>, msg: string) {
            let _ = s.send(msg);
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
        import std.stream;

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

// The old bare `stream.decode()` / `sink.encode()` fail-closed carve-outs are
// gone along with `stream.bytes_pipe`: decoding/encoding now goes through the
// `Codec<T>` trait (`Lines`, `LengthPrefixed`), never a method directly on
// `Stream`/`Sink`. `method_resolution::unlowerable_stream_codec_boundaries_are_not_builtin_methods`
// pins that `decode`/`encode` are simply absent from the builtin method table.

#[test]
fn stream_dot_stream_annotation_typechecks() {
    // A function whose parameter is explicitly spelled `stream.Stream<string>`.
    // Proves: the qualified spelling resolves to the canonical Stream<string>
    // type and its recv/close methods are available.
    let output = typecheck_inline(
        r"
        import std.stream;

        fn take_one(rx: stream.Stream<string>) -> Option<string> {
            let v = rx.recv();
            rx.close();
            v
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "stream.Stream<string> annotation should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

// ===========================================================================
// Stream `for` loop fail-closed tests
// ===========================================================================

/// `for item in rx` over `Stream<string>` must typecheck cleanly.
#[test]
fn for_stream_string_ok() {
    let output = typecheck_inline(
        r#"
        import std.stream;

        fn main() {
            let (tx, rx) = match stream.pipe(4) { .Ok(pair) => pair, .Err(error) => panic(error), };
            let _ = tx.send("hello");
            tx.finish();
            for msg in rx {
                println(msg);
            }
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "for over Stream<string> should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

/// `for val in rx` over `Stream<i64>` must typecheck cleanly.
#[test]
fn for_stream_int_ok() {
    let output = typecheck_inline(
        r"
        import std.stream;

        fn main() {
            let (tx, rx) = match stream.pipe(4) { .Ok(pair) => pair, .Err(error) => panic(error), };
            let _ = tx.send(42);
            tx.finish();
            for val in rx {
                println(val);
            }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "for over Stream<i64> should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

/// `for item in input` over `Stream<Row>` (a `BitCopy` record) rides the
/// element-layout witness and must typecheck cleanly.
#[test]
fn for_stream_record_element_admitted() {
    let output = typecheck_inline(
        r#"import std.stream;

type Row {
    value: i64;
}

extern "C" {
    fn fake_stream() -> Stream<Row>;
}

fn main() {
    let input = unsafe {
        fake_stream()
    };
    for row in input {
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

/// `for item in input` over a bare `Stream` annotation must fail closed
/// instead of bypassing stream element validation and lowering as text.
#[test]
fn for_stream_missing_element_type_errors() {
    let output = typecheck_inline(
        r#"
        extern "C" { fn make_stream() -> Stream; }

        fn main() {
            let s = unsafe { make_stream() };
            for x in s {
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
        "expected InvalidOperation for bare Stream in for, got: {:#?}",
        output.errors
    );
}

/// `for item in actor.receive_gen()` must keep the actor mailbox path and
/// not reuse first-class `Stream<T>` element restrictions.
#[test]
fn for_receive_generator_int_stream_typechecks() {
    let output = typecheck_inline(
        r"
        actor Counter {
            receive gen fn count_up() -> i64 {
                yield 1;
            }
        }

        fn main() {
            let c = spawn Counter;
            for val in c.count_up() {
                println(val);
            }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "for over receive gen Stream<i64> should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

/// A `gen fn` return type that spells the generator handle
/// `Generator<Y, R>` instead of the yield type `Y` is `E_GEN_RETURN_SPELLING`
/// (HEW-SPEC-2026 §4.12), not a fallthrough type mismatch on the first
/// `yield`. #3265.
#[test]
fn gen_fn_return_type_spelling_generator_handle_rejected() {
    let output = typecheck_inline(
        r"
        gen fn counter() -> Generator<i64, ()> {
            yield 1;
        }

        fn main() {
            let _g = counter();
        }
        ",
    );
    // Pinned at exactly 1: the checker recovers using the intended yield
    // type, so no downstream `yield` mismatch cascades alongside the new
    // diagnostic.
    assert_eq!(
        output.errors.len(),
        1,
        "expected exactly one error (no mismatch cascade), got: {:#?}",
        output.errors
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == hew_types::error::TypeErrorKind::GenReturnSpelling
                && e.message.contains("E_GEN_RETURN_SPELLING")
        }),
        "expected E_GEN_RETURN_SPELLING for a gen fn spelling its own Generator<Y, R> handle, got: {:#?}",
        output.errors
    );
}

/// Negative control: naming the yield type directly (the accepted spelling)
/// must not trip `E_GEN_RETURN_SPELLING`.
#[test]
fn gen_fn_return_type_spelling_yield_type_accepted() {
    let output = typecheck_inline(
        r"
        gen fn counter() -> i64 {
            yield 1;
        }

        fn main() {
            let _g = counter();
        }
        ",
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::GenReturnSpelling),
        "expected no E_GEN_RETURN_SPELLING for the accepted `-> Y` spelling, got: {:#?}",
        output.errors
    );
}

/// Only a `receive gen fn` produces a stream a `for` loop can drain. A plain
/// `receive fn` is an ask whose call value is `Result<Stream<string>, ActorError>`
/// (U383), so the loop is refused rather than silently draining the reply.
#[test]
fn for_over_a_plain_receive_fn_is_refused() {
    let output = typecheck_inline(
        r#"
        extern "C" { fn fake_stream() -> Stream<string>; }

        actor Reader {
            receive fn lines() -> Stream<string> {
                unsafe { fake_stream() }
            }
        }

        fn main() {
            let r = spawn Reader;
            for line in r.lines() {
                println(line);
            }
        }
        "#,
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == hew_types::error::TypeErrorKind::InvalidOperation
                && (e.message.contains("requires a `receive gen fn`")
                    || e.message.contains("type is not iterable"))
        }),
        "expected the loop over a plain `receive fn` to be refused, got: {:#?}",
        output.errors
    );
}

/// A `Receiver<string>` parameter drained by `for` carries an admissible
/// element type, so the queue-element guard must stay silent.
#[test]
fn for_over_a_receiver_parameter_is_admitted() {
    let output = typecheck_inline(
        r"
        import std.channel;

        fn consume(rx: channel.Receiver<string>) {
            for msg in rx {
                println(msg);
            }
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| !e.message.contains("is not supported in a `for` loop")),
        "the queue-element guard must not fire on an admissible element type, got errors: {:#?}",
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
            let rc: Rc<i64> = Rc.new(42);
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

#[test]
fn rc_weak_surface_records_typed_intrinsics() {
    let output = typecheck_inline(
        r"
        fn main() {
            let rc = Rc.new(42);
            let alias = rc.clone();
            let _value = alias.get();
            rc.set(9);
            let weak = rc.downgrade();
            let weak2 = weak.clone();
            let _upgraded = weak2.upgrade();
            let _strong = rc.strong_count();
            let _weak = rc.weak_count();
            let _unique = rc.is_unique();
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "complete Rc/Weak surface should type-check cleanly: {:#?}",
        output.errors
    );
    let ops = output
        .method_call_rewrites
        .values()
        .filter_map(|rewrite| match rewrite {
            MethodCallRewrite::RcIntrinsic { op, payload_ty } => {
                assert_eq!(payload_ty, &hew_types::ResolvedTy::I64);
                Some(*op)
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    for expected in [
        RcIntrinsicOp::New,
        RcIntrinsicOp::Clone,
        RcIntrinsicOp::GetCopy,
        RcIntrinsicOp::Set,
        RcIntrinsicOp::Downgrade,
        RcIntrinsicOp::WeakClone,
        RcIntrinsicOp::WeakUpgrade,
        RcIntrinsicOp::StrongCount,
        RcIntrinsicOp::WeakCount,
        RcIntrinsicOp::IsUnique,
    ] {
        assert!(
            ops.contains(&expected),
            "missing typed intrinsic {expected:?}: {ops:?}"
        );
    }
}

#[test]
fn weak_breaks_recursive_value_size_cycle() {
    let output = typecheck_inline(
        r"type Node {
    parent: Option<Weak<Node>>;
    value: i64;
}

fn main() {}
",
    );
    assert!(
        output.errors.is_empty(),
        "Weak<Node> must be an indirection for recursive-size analysis: {:#?}",
        output.errors
    );
}

#[test]
fn weak_rejected_at_actor_send_boundary() {
    let output = typecheck_inline(
        r"actor BoundarySink {
    let _unused: i64;
    receive fn consume(value: Weak<i64>) {}
}

fn main() {
    let rc = Rc.new(1);
    let weak = rc.downgrade();
    let sink = spawn BoundarySink { _unused: 0 };
    let _ = sink.consume(weak);
}
",
    );
    assert_eq!(output.errors.len(), 1, "{:#?}", output.errors);
    assert_eq!(output.errors[0].kind, TypeErrorKind::InvalidSend);
    assert!(
        output.errors[0].message.contains("Weak<i64>"),
        "expected `Weak<i64>` in InvalidSend diagnostic: {:?}",
        output.errors[0].message
    );
}

#[test]
fn rc_new_rejects_borrowed_non_copy_parameter_without_clone() {
    let rejected = typecheck_inline(
        r"
        fn wrap(value: string) -> Rc<string> {
            Rc.new(value)
        }
        fn main() {}
        ",
    );
    let errors = rejected
        .errors
        .iter()
        .filter(|error| error.kind == TypeErrorKind::BorrowedParamReturn)
        .count();
    assert_eq!(
        errors, 1,
        "borrowed Rc::new source must fail once: {:#?}",
        rejected.errors
    );

    let accepted = typecheck_inline(
        r"
        fn wrap(value: string) -> Rc<string> {
            Rc.new(value.clone())
        }
        fn main() {}
        ",
    );
    assert!(
        accepted.errors.is_empty(),
        "an explicit clone must materialize an owned Rc::new source: {:#?}",
        accepted.errors
    );
}

#[test]
fn rc_set_rejects_borrowed_non_copy_parameter_without_clone() {
    let rejected = typecheck_inline(
        r"
        fn replace(rc: Rc<string>, value: string) {
            rc.set(value);
        }
        fn main() {}
        ",
    );
    let errors = rejected
        .errors
        .iter()
        .filter(|error| error.kind == TypeErrorKind::BorrowedParamReturn)
        .count();
    assert_eq!(
        errors, 1,
        "borrowed Rc.set source must fail once: {:#?}",
        rejected.errors
    );

    let accepted = typecheck_inline(
        r"
        fn replace(rc: Rc<string>, value: string) {
            rc.set(value.clone());
        }
        fn main() {}
        ",
    );
    assert!(
        accepted.errors.is_empty(),
        "an explicit clone must materialize an owned Rc.set source: {:#?}",
        accepted.errors
    );
}

#[test]
fn weak_param_return_requires_clone() {
    let rejected = typecheck_inline(
        r"
        fn identity(value: Weak<i64>) -> Weak<i64> { value }
        fn main() {}
        ",
    );
    assert_eq!(
        rejected
            .errors
            .iter()
            .filter(|error| error.kind == TypeErrorKind::BorrowedParamReturn)
            .count(),
        1,
        "returning borrowed Weak without clone must fail once: {:#?}",
        rejected.errors
    );

    let accepted = typecheck_inline(
        r"
        fn identity(value: Weak<i64>) -> Weak<i64> { value.clone() }
        fn main() {}
        ",
    );
    assert!(
        accepted.errors.is_empty(),
        "cloned Weak return must pass: {:#?}",
        accepted.errors
    );
}

/// `Rc<T>` must be rejected when sent across an actor boundary (non-Send).
#[test]
fn rc_rejected_at_actor_send_boundary() {
    let output = typecheck_inline(
        r"actor BoundarySink {
    let _unused: i64;
    receive fn consume(val: Rc<i64>) {}
}

fn main() {
    let rc: Rc<i64> = Rc.new(1);
    let a = spawn BoundarySink { _unused: 0 };
    let _ = a.consume(rc);
}
",
    );
    assert_eq!(output.errors.len(), 1, "{:#?}", output.errors);
    assert_eq!(output.errors[0].kind, TypeErrorKind::InvalidSend);
    assert!(
        output.errors[0].message.contains("Rc<i64>"),
        "expected `Rc<i64>` in InvalidSend diagnostic: {:?}",
        output.errors[0].message
    );
}

#[test]
fn ordinary_actor_send_keeps_sender_binding_readable() {
    let output = typecheck_inline(
        r"type Boxed {
    payload: Vec<i64>;
}

actor BoundarySink {
    let _unused: i64;
    receive fn take(value: Boxed) {}
}

fn main() {
    let sink = spawn BoundarySink { _unused: 0 };
    var value = Boxed { payload: [1, 2] };
    let _ = sink.take(value);
    let _ = sink.take(value);
    value.payload.push(3);
    println(value.payload[2]);
}
",
    );
    assert!(
        output.errors.is_empty(),
        "ordinary actor sends are snapshots and must not move the sender binding: {:#?}",
        output.errors
    );
}

#[test]
fn actor_spawn_still_moves_affine_handle_arguments() {
    let output = typecheck_inline(
        r#"import std.stream;

actor Writer {
    let sink: stream.Sink<string>;
}

fn main() {
    let (sink, _input) = match stream.pipe(4) {
        .Ok(pair) => pair,
        .Err(error) => panic(error),
    };
    let _writer = spawn Writer { sink: sink };
    sink.send("after-move");
}
"#,
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::UseAfterMove),
        "snapshot sends preserve ordinary values, but affine spawn arguments must \
         remain consumed: {:#?}",
        output.errors
    );
}

#[test]
fn nested_rc_and_weak_send_rejections_do_not_cascade() {
    let output = typecheck_inline(
        r"type RcBox {
    value: Rc<i64>;
}

actor BoundarySink {
    let _unused: i64;
    receive fn take_box(value: RcBox) {}
    receive fn take_tuple(value: (i64, Weak<i64>)) {}
}

fn main() {
    let sink = spawn BoundarySink { _unused: 0 };
    let rc = Rc.new(1);
    let weak = rc.downgrade();
    let boxed = RcBox { value: rc.clone() };
    let pair = (2, weak);
    let _ = sink.take_box(boxed);
    let _ = sink.take_tuple(pair);
    println(boxed.value.strong_count());
    println(pair.0);
}
",
    );
    assert_eq!(output.errors.len(), 2, "{:#?}", output.errors);
    assert!(
        output
            .errors
            .iter()
            .all(|error| error.kind == TypeErrorKind::InvalidSend),
        "{:#?}",
        output.errors
    );
}

#[test]
fn lambda_actor_capture_must_be_send() {
    // `actor move |params| { body }` captures are checked for Send; Rc<i64> is not Send.
    let output = typecheck_inline(
        r"
        fn main() {
            let rc: Rc<i64> = Rc.new(1);
            let worker = actor move |x: i64| {
                println(rc.strong_count());
                println(x);
            };
            _ = worker.send(1);
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
            let rc: Rc<i64> = Rc.new(1);
            _ = worker(rc.strong_count());
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
        r"actor Sink {
    let _unused: i64;
}

fn main() {
    let sink = spawn Sink { _unused: 0 };
    let rc: Rc<i64> = Rc.new(1);
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
        r"actor Queue {
    let items: Vec<i64>;
    receive fn dequeue() -> Option<i64> {
        .None
    }
}

fn main() {
    let q = spawn Queue { items: Vec.new() };
    let _item = q.dequeue();
}
",
    );
    assert!(
        output.errors.is_empty(),
        "actor receive fn returning Option<i64> must typecheck cleanly (F-02 regression), got: {:#?}",
        output.errors
    );
}

/// Accept path for the `actor |...| { ... }` syntax with call-syntax dispatch.
#[test]
fn actor_lambda_new_syntax_typechecks() {
    let output = typecheck_inline(
        r"
        fn main() {
            let worker = actor |msg: i64| {
                println(msg);
            };
            _ = worker(1);
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
            _ = doubler(5);
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
                    _ = fib(n - 1);
                }
            };
            _ = fib(10);
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "recursive self-call in actor body must typecheck clean, got: {:#?}",
        output.errors
    );
}

#[test]
fn ordinary_closure_cannot_capture_an_outer_actor_receiver() {
    let output = typecheck_inline(
        r"fn main() {
            let worker = actor |n: i64| { println(n); };
            let forward = |n: i64| { let _ = worker(n); };
            forward(1);
        }",
    );
    assert!(
        output.errors.iter().any(|error| matches!(
            error.kind,
            hew_types::error::TypeErrorKind::ClosureCapturesDuplexHandle { .. }
        )),
        "outer actor receiver must retain its capture restriction: {:#?}",
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
            _ = worker.send(1);
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
            _ = log("x");
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "tell-shaped actor with call-syntax dispatch should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

/// `Rc::new` must accept non-Copy `T`; the codegen passes the real drop function.
/// `Rc::get()` must be rejected when `T` is not `Copy` (`LoadOp` semantics).
#[test]
fn rc_non_copy_construction_ok() {
    // string is non-Copy; Rc::new should accept it (codegen will pass a real
    // drop function instead of null).
    let output = typecheck_inline(r#"fn main() { let _rc: Rc<string> = Rc.new("hello"); }"#);
    assert!(
        output.errors.is_empty(),
        "Rc::new with a non-Copy inner type should succeed; got errors: {:#?}",
        output.errors
    );
}

#[test]
fn rc_copy_struct_construction_ok() {
    let output = typecheck_inline(
        r"type Point {
    x: i64;
    y: i64;
}

fn main() {
    let _rc = Rc.new(Point { x: 1, y: 2 });
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
        typecheck_inline(r#"fn main() { let _rc: Rc<Rc<string>> = Rc.new(Rc.new("hello")); }"#);
    assert!(
        output.errors.is_empty(),
        "Rc::new with nested supported Rc payloads should succeed; got errors: {:#?}",
        output.errors
    );
}

#[test]
fn rc_owned_option_payload_supported() {
    assert_inline_typechecks_cleanly(
        r#"fn main() { let _rc = Rc.new(Option.Some("hello")); }"#,
        "Rc::new with Option<string> should use aggregate clone/drop support",
    );
}

#[test]
fn rc_owned_struct_payload_supported() {
    assert_inline_typechecks_cleanly(
        r#"type Labelled {
    name: string;
}

fn main() {
    let _rc = Rc.new(Labelled { name: "hello" });
}
"#,
        "Rc::new with an owned struct should use aggregate clone/drop support",
    );
}

/// User `impl Drop` is rejected fail-closed; the rejection fires before the
/// `Rc` payload check, so an `Rc<Token>` over a user-`Drop` payload still
/// fails closed at the unsupported destructor.
#[test]
fn rc_user_drop_payload_rejected() {
    let output = typecheck_inline(
        r"type Token {
    id: i64;
}

impl Drop for Token {
    fn drop(self) {
        print(self.id);
    }
}

fn main() {
    let _rc = Rc.new(Token { id: 1 });
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
fn rc_owned_payload_annotation_supported() {
    assert_inline_typechecks_cleanly(
        r"fn borrow(_r: Rc<Option<string>>) {}",
        "Rc<Option<string>> annotations should use aggregate clone/drop support",
    );
}

#[test]
fn rc_generic_wrapper_payload_rejected() {
    let output = typecheck_inline(
        r#"type Labelled {
    name: string;
}

fn wrap<T>(val: T) -> Rc<T> {
    Rc.new(val)
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
fn rc_get_non_copy_rejected() {
    // `rc.get()` performs a bitwise copy (LoadOp) which is only sound for
    // Copy types.  Calling it on Rc<string> must be rejected.
    let output = typecheck_inline(
        r#"fn main() { let rc: Rc<string> = Rc.new("hello"); let _ = rc.get(); }"#,
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
        r#"import std.net.http.server as http;

fn main() {
    match http.listen(":8080") {
        .Ok(server) => {
            let req = match server.accept() { .Ok(request) => request, .Err(_) => return, };
            let _sent = req.respond(200, "text/plain", "Hello, Hew!").is_ok();
            server.close();
        }
        .Err(_err) => {}
    }
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
        r#"import std.net.http.server as http;

fn main() {
    match http.listen(":8080") {
        .Ok(server) => {
            let req = server.accept();
            req.respond(200, "text/plain", 11, "Hello, Hew!");
            req.close();
            server.close();
        }
        .Err(_err) => {}
    }
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
        import std.net.http.server as http;

        fn inspect(server: http.Server, req: http.Request) -> string {
            let _next = server.accept();
            let _sent = req.respond_text(200, "ok").is_ok();
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
        import std.net;

        fn tune(listener: net.Listener, conn: net.Connection) -> i64 {
            let _accepted = listener.accept();
            conn.set_read_timeout(10ms);
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
        import std.net.http.server as http;

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
fn net_resource_close_uses_source_impl_not_raw_fallback() {
    let output = typecheck_inline(
        r"
        import std.net;

        fn close_listener(consume listener: net.Listener) {
            listener.close()
        }

        fn close_connection(consume connection: net.Connection) {
            connection.close()
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected net resource closes to resolve through their authored impls, got: {:#?}",
        output.errors
    );
    assert!(
        !output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol, .. }
                if matches!(c_symbol.as_str(), "hew_tcp_listener_close" | "hew_tcp_close")
        )),
        "resource close must not bypass source dispatch with a raw FFI rewrite, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn http_request_close_dispatches_through_resource_impl() {
    let output = typecheck_inline(
        r"
        import std.net.http.server as http;

        fn release(consume req: http.Request) {
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
        output.method_call_rewrites.iter().any(|(key, rewrite)| key.module_idx == 0 && matches!(
            rewrite,
            MethodCallRewrite::RewriteToFunction { target: hew_types::check::CallTarget::ImplMethod(def), .. }
                if output.defs.path(*def).starts_with("std.net.http.server.Request::")
        )),
        "resource-wrapper close must dispatch through the authored impl, got: {:?}",
        output.method_call_rewrites
    );
    assert!(
        !output
            .method_call_rewrites
            .iter()
            .any(|(key, rewrite)| key.module_idx == 0
                && matches!(
                    rewrite,
                    MethodCallRewrite::RewriteToFunction { c_symbol, .. }
                        if c_symbol.starts_with("hew_")
                )),
        "resource-wrapper close must not fall back to a raw FFI rewrite, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn registry_loaded_resource_close_moves_receiver() {
    let output = typecheck_inline(
        r"
        import std.net.http.server as http;

        fn release(consume req: http.Request) {
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
        import std.net.http.server as http;

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
fn net_connection_send_arg_type_checked() {
    // `send` is a Hew wrapper function (`send_result_from_status(unsafe {
    // hew_tcp_write(...) })`), not a direct single-call C shim, so the
    // registry's handle-method extractor cannot see it. The module's own
    // source declarations supply the signature on every import spelling, so
    // the registry-only checker path enforces the same `bytes` argument type
    // the full module-graph path does.
    let output = typecheck_inline(
        r#"
        import std.net;

        fn relay(conn: net.Connection) {
            let _ = conn.send("wrong_type");
        }
        "#,
    );
    assert!(
        output.errors.iter().any(|error| matches!(
            &error.kind,
            TypeErrorKind::Mismatch { expected, actual }
                if expected == "bytes" && actual == "string"
        )),
        "expected conn.send to reject a string where `bytes` is required, got: {:#?}",
        output.errors
    );
}

#[test]
fn wasm_process_execution_surface_rejected_before_codegen() {
    let output = typecheck_inline_wasm(
        r#"
        import std.process;

        fn await_child(child: process.Child) -> Result<process.ExitStatus, process.ProcessError> {
            child.wait()
        }

        fn main() {
            let _ = process.run("echo hi");
            let _ = process.start(process.command("sleep", ["1"]));
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
        import std.string;

        fn parse() -> i64 {
            let value: Result<i64, string.NumberError> = ("9223372036854775807").to_int();
            match value {
                .Ok(n) => n,
                .Err(_) => 0,
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
fn process_child_methods_resolve_as_fielded_resource_methods() {
    let output = typecheck_inline(
        r"
        import std.process;

        fn manage(child: process.Child) -> process.ExitStatus fails process.ProcessError {
            child.terminate()?;
            child.kill()?;
            child.wait()?
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
    assert!(
        !output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol, .. }
                if c_symbol == "hew_process_exit_stream" || c_symbol == "hew_process_signal"
        )),
        "fielded process.Child methods must execute their source-level \
         bodies (which unwrap ChildHandle) rather than bypass them through \
         opaque-handle C-symbol rewrites: {:?}",
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
        import std.net.smtp;

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
            ).expect("send succeeds");
            smtp.send_html(
                "smtp.example.com",
                587,
                "user",
                "pass",
                "from@example.com",
                "to@example.com",
                "Subject",
                "<h1>Hello</h1>",
            ).expect("send_html succeeds");
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
        import std.net.http.client;

        fn main() {
            let headers: Vec<(string, string)> = Vec.new();
            let _ = client.set_timeout(250ms);
            let _body = client.request_string("GET", "https://example.com", "", headers);
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
    for (import, module) in [
        ("std.net.http.client", "client"),
        ("std.net.http.client as requests", "requests"),
    ] {
        let source = format!(
            r#"
            import {import};
            fn main() {{
                let headers: Vec<(string, string)> = Vec.new();
                let _ = {module}.set_timeout(250ms);
                let _body = {module}.request_string("GET", "https://example.com", "", headers);
                let _request = {module}.request;
            }}
            "#,
        );
        assert_http_client_wasm_rejections(&source, 3);
    }

    assert_http_client_wasm_rejections(
        r#"
        import std.net.http.client.{request_string, set_timeout};
        fn main() {
            let headers: Vec<(string, string)> = Vec.new();
            let _ = set_timeout(250ms);
            let _body = request_string("GET", "https://example.com", "", headers);
            let _request = request_string;
        }
        "#,
        3,
    );
}

fn assert_http_client_wasm_rejections(source: &str, expected: usize) {
    let native = typecheck_inline(source);
    assert!(
        native.errors.is_empty(),
        "HTTP client source must remain valid natively: {:#?}",
        native.errors
    );
    let output = typecheck_inline_wasm(source);
    assert_eq!(
        output.errors.len(),
        expected,
        "each HTTP client operation needs one precise capability rejection: {:#?}",
        output.errors
    );
    assert!(
        output.errors.iter().all(|error| {
            error.kind == TypeErrorKind::PlatformLimitation
                && error
                    .message
                    .contains("std.net.http.client operations are not supported on WASM32")
        }),
        "HTTP client operations must not inherit the HTTP server capability: {:#?}",
        output.errors
    );
}

#[test]
fn http_client_response_methods_rejected_on_wasm() {
    for (import, module) in [
        ("std.net.http.client", "client"),
        ("std.net.http.client as requests", "requests"),
    ] {
        let source = format!(
            r#"
            import {import};
            extern "C" {{
                fn fake_response() -> {module}.Response;
            }}
            fn main() {{
                let resp = unsafe {{ fake_response() }};
                let _status = resp.status();
                resp.close();
            }}
            "#,
        );
        assert_http_client_wasm_rejections(&source, 2);
    }
}

#[test]
fn smtp_module_helpers_rejected_on_wasm() {
    let output = typecheck_inline_wasm(
        r#"
        import std.net.smtp;

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
            ).expect("send succeeds");
        }
        "#,
    );
    assert!(
        output.errors.iter().any(|e| {
            e.kind == TypeErrorKind::PlatformLimitation
                && e.message
                    .contains("std.net.smtp operations are not supported on WASM32")
        }),
        "expected smtp wasm rejection, got: {:#?}",
        output.errors
    );
}

#[test]
fn smtp_conn_methods_rejected_on_wasm() {
    let output = typecheck_inline_wasm(
        r#"
        import std.net.smtp;

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
                    .contains("std.net.smtp operations are not supported on WASM32")
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
fn native_only_handle_methods_rejected_on_wasm_and_allowed_natively() {
    let cases: &[(&str, &str, &str, &str, &str)] = &[
        (
            "std.net.tls",
            "tls.TlsStream",
            "fn fake_tls() -> tls.TlsStream",
            "let stream = unsafe { fake_tls() };\n            stream.close();",
            "std.net.tls",
        ),
        (
            "std.net.quic",
            "quic.QUICEndpoint",
            "fn fake_endpoint() -> quic.QUICEndpoint",
            "let ep = unsafe { fake_endpoint() };\n            ep.close();",
            "std.net.quic",
        ),
        (
            "std.net.quic",
            "quic.QUICConnection",
            "fn fake_conn() -> quic.QUICConnection",
            "let conn = unsafe { fake_conn() };\n            let _ = conn.observe();",
            "std.net.quic",
        ),
        (
            "std.net.quic",
            "quic.QUICStream",
            "fn fake_stream() -> quic.QUICStream",
            "let strm = unsafe { fake_stream() };\n            strm.close();",
            "std.net.quic",
        ),
        (
            "std.net.quic",
            "quic.QUICEvent",
            "fn fake_event() -> quic.QUICEvent",
            "let ev = unsafe { fake_event() };\n            ev.close();",
            "std.net.quic",
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
        let wasm_output = typecheck_inline_wasm(&source);
        assert!(
            wasm_output.errors.iter().any(|e| {
                e.kind == TypeErrorKind::PlatformLimitation && e.message.contains(expected_label)
            }),
            "expected wasm PlatformLimitation for {handle_type} handle-method, got: {:#?}",
            wasm_output.errors
        );

        let native_output = typecheck_inline(&source);
        assert!(
            !native_output.errors.iter().any(|e| {
                e.kind == TypeErrorKind::PlatformLimitation && e.message.contains(expected_label)
            }),
            "unexpected PlatformLimitation on native target for {handle_type}: {:#?}",
            native_output.errors
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
        r#"import std.text.regex;
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
        .iter()
        .filter(|(key, ty)| key.module_idx == 0 && **ty == hew_types::Ty::I32)
        .count();
    let i64_bound_count = output
        .expr_types
        .iter()
        .filter(|(key, ty)| key.module_idx == 0 && **ty == hew_types::Ty::I64)
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

/// Negative control for the field-access arm above: `Range<T>` exposes only
/// `start` and `end`, so any other field name still refuses.
#[test]
fn range_literal_field_access_rejects_unknown_field() {
    let output = typecheck_inline(
        r"fn test() {
    let r: Range<i32> = 0..10;
    let _: i32 = r.middle;
}
",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UndefinedField),
        "unknown field on Range<i32> should be refused: {:#?}",
        output.errors
    );
}

#[test]
fn range_bounds_are_read_only_through_direct_and_indexed_places() {
    for source in [
        "fn main() { var span = 0..5; span.start = 2; }",
        "fn main() { var span = 0..5; span.end += 2; }",
        "fn main() { var spans = [0..5]; spans[0].start = 2; }",
    ] {
        let output = typecheck_inline(source);
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.message.contains("range bounds are read-only")),
            "{source}: {:?}",
            output.errors
        );
    }
    let output = typecheck_inline(
        "fn main() { var span = 0..5; span = 2..5; for value in span { println(value); } }",
    );
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn range_sequence_metadata_is_not_source_accessible_or_constructible() {
    for field in ["first", "last", "stride", "descending", "nonempty"] {
        let source = format!("fn main() {{ let span = 0..5; let _ = span.{field}; }}");
        let output = typecheck_inline(&source);
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.kind == TypeErrorKind::UndefinedField),
            "{source}: {:?}",
            output.errors
        );
    }
    let output = typecheck_inline("fn main() { let span = Range<i64> { start: 0, end: 5, first: 0, last: 4, stride: 1, descending: false, nonempty: true }; }");
    assert!(
        !output.errors.is_empty(),
        "range storage is not a source record constructor"
    );
}

/// Cases where a borrowed `Rc` parameter escapes directly, via aggregates,
/// or through tainted locals/branches/fields.
const BORROWED_PARAM_ESCAPE_CASES: &[(&str, &str, usize, bool)] = &[
    (
        "trailing bare ident",
        "fn identity(r: Rc<i64>) -> Rc<i64> { r } fn main() {}",
        1,
        false,
    ),
    (
        "explicit early return",
        "fn early(r: Rc<i64>, flag: bool) -> Rc<i64> {\n    if flag { return r; }\n    Rc.new(0)\n}\nfn main() {}",
        1,
        false,
    ),
    (
        "block-wrapped return",
        "fn wrapped(r: Rc<i64>) -> Rc<i64> { { r } } fn main() {}",
        1,
        false,
    ),
    (
        "Option.Some wrap",
        "fn wrap(r: Rc<i64>) -> Option<Rc<i64>> { .Some(r) } fn main() {}",
        1,
        false,
    ),
    (
        "tuple wrap",
        "fn wrap(r: Rc<i64>) -> (Rc<i64>, i64) { (r, 0) } fn main() {}",
        1,
        false,
    ),
    (
        "struct init field",
        "type Holder { val: Rc<i64>; }\nfn wrap(r: Rc<i64>) -> Holder { Holder { val: r } }\nfn main() {}",
        1,
        false,
    ),
    (
        "explicit return Some",
        "fn wrap(r: Rc<i64>) -> Option<Rc<i64>> { return .Some(r); } fn main() {}",
        1,
        false,
    ),
    (
        "vec push store and return",
        "fn bad(r: Rc<i64>) -> Vec<Rc<i64>> { var v = Vec.new(); v.push(r); v }\nfn main() {}",
        1,
        true,
    ),
    (
        "vec push store and explicit return",
        "fn bad(r: Rc<i64>) -> Vec<Rc<i64>> { var v = Vec.new(); v.push(r); return v; }\nfn main() {}",
        1,
        false,
    ),
    (
        "direct local alias",
        "fn bad(r: Rc<i64>) -> Rc<i64> { let v = r; v }\nfn main() {}",
        1,
        false,
    ),
    (
        "aggregate local alias",
        "fn bad(r: Rc<i64>) -> Option<Rc<i64>> { let v = Option.Some(r); v }\nfn main() {}",
        1,
        false,
    ),
    (
        "transitive alias taint",
        "fn bad(r: Rc<i64>) -> Vec<Rc<i64>> { let a = r; var v = Vec.new(); v.push(a); v }\nfn main() {}",
        1,
        false,
    ),
    (
        "branch-conditional vec store",
        "fn bad(r: Rc<i64>, cond: bool) -> Vec<Rc<i64>> { var v = Vec.new(); if cond { v.push(r); } v }\nfn main() {}",
        1,
        false,
    ),
    (
        "assignment taint",
        "fn bad(r: Rc<i64>) -> Rc<i64> { var v: Rc<i64> = Rc.new(0); v = r; v }\nfn main() {}",
        1,
        false,
    ),
    (
        "field assignment escape",
        "type Wrapper { value: Rc<i64>; }\nfn bad(r: Rc<i64>) -> Wrapper { var s = Wrapper { value: Rc.new(0) }; s.value = r; s }\nfn main() {}",
        1,
        false,
    ),
    (
        "if-let body trailing expr",
        "fn escape(r: Rc<i64>, opt: Option<i64>) -> Rc<i64> { if let .Some(_v) = opt { r } else { Rc.new(0) } }\nfn main() {}",
        1,
        false,
    ),
    (
        "if-let else trailing expr",
        "fn escape(r: Rc<i64>, opt: Option<i64>) -> Rc<i64> { if let .Some(_v) = opt { Rc.new(0) } else { r } }\nfn main() {}",
        1,
        false,
    ),
    (
        "if-let both branches",
        "fn escape(r: Rc<i64>, opt: Option<i64>) -> Rc<i64> { if let .Some(_v) = opt { r } else { r } }\nfn main() {}",
        2,
        false,
    ),
    (
        "if-let shadow suppresses inner branch only",
        "fn shadow_then_escape(r: Rc<i64>, opt: Option<i64>) -> Rc<i64> { if let .Some(v) = opt { let r = Rc.new(v); r } else { r } }\nfn main() {}",
        1,
        false,
    ),
];

/// Returning a borrowed-param binding or propagating its taint through
/// tainted locals/branches/fields must fail closed with `BorrowedParamReturn`.
#[test]
fn borrowed_param_return_rejects_escapes() {
    for (label, source, expected_count, check_names) in BORROWED_PARAM_ESCAPE_CASES {
        let output = typecheck_inline(source);
        let rc_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
            .collect();
        assert_eq!(
            rc_errors.len(),
            *expected_count,
            "{label}: expected {expected_count} BorrowedParamReturn errors, got: {:#?}",
            output.errors
        );
        if *check_names {
            assert!(
                rc_errors[0].message.contains("`v`") && rc_errors[0].message.contains("`r`"),
                "{label}: error should mention both `v` and `r`, got: {}",
                rc_errors[0].message
            );
        }
    }
}

#[test]
fn borrowed_param_return_allows_clones_and_non_escapes() {
    for (label, source) in [
        (
            "clone return",
            "fn safe_identity(r: Rc<i64>) -> Rc<i64> { r.clone() }\nfn main() {}",
        ),
        (
            "read-only borrow callee",
            "fn read_rc(r: Rc<i64>) -> i64 { r.get() }\nfn main() { let rc = Rc.new(42); println(read_rc(rc)); }",
        ),
        (
            "Option.Some(clone)",
            "fn wrap(r: Rc<i64>) -> Option<Rc<i64>> { .Some(r.clone()) }\nfn main() {}",
        ),
        (
            "fresh Rc.new return",
            "fn fresh(_r: Rc<i64>) -> Rc<i64> { Rc.new(0) }\nfn main() {}",
        ),
        (
            "delegated borrow call",
            "fn extract(r: Rc<i64>) -> i64 { r.get() }\nfn delegate(r: Rc<i64>) -> i64 { extract(r) }\nfn main() {}",
        ),
        (
            "local store not returned",
            "fn not_returned(r: Rc<i64>) -> i64 { var v = Vec.new(); v.push(r); 42 }\nfn main() {}",
        ),
        (
            "push clone then return vec",
            "fn safe(r: Rc<i64>) -> Vec<Rc<i64>> { var v = Vec.new(); v.push(r.clone()); v }\nfn main() {}",
        ),
        (
            "read-only contains does not taint",
            "fn ok(r: Rc<i64>) -> Vec<i64> { let v = Vec.new(); v.contains(r.get()); v }\nfn main() {}",
        ),
        (
            "local shadow binding",
            "fn shadow(r: Rc<i64>) -> Rc<i64> { let r = Rc.new(99); r }\nfn main() {}",
        ),
    ] {
        let output = typecheck_inline(source);
        let rc_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| e.kind == hew_types::error::TypeErrorKind::BorrowedParamReturn)
            .collect();
        assert!(
            rc_errors.is_empty(),
            "{label}: should not emit BorrowedParamReturn, got: {rc_errors:#?}"
        );
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
//  Fixed arrays in collection values
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
fn fixed_arrays_compose_with_collection_element_types() {
    for element in [
        "[i64; 2]",
        "Vec<[i64; 2]>",
        "Box<[i64; 2]>",
        "Option<[i64; 2]>",
        "Result<[i64; 2], string>",
    ] {
        assert_inline_typechecks_cleanly(&format!(
            "type Box<T> {{ value: T; }} fn main() {{ let values: Vec<{element}> = Vec.new(); println(values.len()); }}"
        ), element);
    }
}

#[test]
fn fixed_array_repeat_refuses_implicit_affine_duplication() {
    assert_invalid_operation_contains(
        r"#[resource]
type Token {
    id: i64;
}

impl Token {
    fn close(consume self) {}
}

fn main() {
    let values: [Token; 2] = [Token { id: 1 }; 2];
}
",
        "requires a Clone element",
        "fixed array repeat cannot duplicate a resource",
    );
}

// ═══════════════════════════════════════════════════════════════════════════════
//  Rc<T> in collections
// ═══════════════════════════════════════════════════════════════════════════════

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
fn rc_in_collections_supported_operations() {
    for (label, source) in [
        (
            "Vec.push(Rc<i64>)",
            "fn main() { let r = Rc.new(42); var v = Vec.new(); v.push(r); }",
        ),
        (
            "Vec.set(_, Rc<i64>)",
            "fn main() { var v: Vec<Rc<i64>> = Vec.new(); let r = Rc.new(99); v.set(0, r); }",
        ),
        (
            "Vec.append(Vec<Rc<i64>>)",
            "fn main() { var v: Vec<Rc<i64>> = Vec.new(); let w: Vec<Rc<i64>> = Vec.new(); v.append(w); }",
        ),
        (
            "Vec.pop() on Vec<Rc<i64>>",
            "type Holder { v: Vec<Rc<i64>>; }\nfn extract(h: Holder) -> Rc<i64> { var local = h.v; local.pop() }",
        ),
        (
            "Vec.get(_) on Vec<Rc<i64>>",
            "type Holder { v: Vec<Rc<i64>>; }\nfn extract(h: Holder) -> Option<Rc<i64>> { h.v.get(0) }",
        ),
        (
            "Vec.remove(_) on Vec<Rc<i64>>",
            "type Holder { v: Vec<Rc<i64>>; }\nfn extract(h: Holder) -> Rc<i64> { var items = h.v; items.remove(0) }",
        ),
        (
            "Vec.map(_) on Vec<Rc<i64>>",
            "type Holder { v: Vec<Rc<i64>>; }\nfn extract(h: Holder) -> Vec<Rc<i64>> { h.v.map(|x: Rc<i64>| x) }",
        ),
        (
            "Vec.filter(_) on Vec<Rc<i64>>",
            "type Holder { v: Vec<Rc<i64>>; }\nfn extract(h: Holder) -> Vec<Rc<i64>> { h.v.filter(|x: Rc<i64>| x.get() > 0) }",
        ),
        (
            "Vec.fold(_, _) on Vec<Rc<i64>>",
            "type Holder { v: Vec<Rc<i64>>; }\nfn extract(h: Holder) -> Rc<i64> { h.v.fold(Rc.new(0), |acc: Rc<i64>, x: Rc<i64>| x) }",
        ),
        (
            "HashMap.insert(_, Rc<i64>)",
            "fn main() { var m = HashMap.new(); let r = Rc.new(42); m.insert(\"key\", r); }",
        ),
        (
            "HashMap.get() on HashMap<string, Rc<i64>>",
            "type Holder { items: HashMap<string, Rc<i64>>; }\nfn leak(h: Holder) -> Option<Rc<i64>> { h.items.get(\"key\") }",
        ),
        (
            "HashMap.remove() on HashMap<string, Rc<i64>>",
            "type Holder { items: HashMap<string, Rc<i64>>; }\nfn remove_key(h: Holder) -> Option<Rc<i64>> { var items = h.items; items.remove(\"key\") }",
        ),
        (
            "HashMap.keys() on HashMap<string, Rc<i64>>",
            "type Holder { items: HashMap<string, Rc<i64>>; }\nfn leak(h: Holder) -> Vec<string> { h.items.keys() }",
        ),
        (
            "HashMap.values() on HashMap<string, Rc<i64>>",
            "type Holder { items: HashMap<string, Rc<i64>>; }\nfn leak(h: Holder) -> Vec<Rc<i64>> { h.items.values() }",
        ),
        (
            "Vec.push(Option.Some(Rc<i64>))",
            "fn main() { var v = Vec.new(); let r = Rc.new(42); v.push(Option.Some(r)); }",
        ),
        (
            "Vec.push((Rc<i64>, i64))",
            "fn main() { var v = Vec.new(); let r = Rc.new(42); v.push((r, 0)); }",
        ),
    ] {
        assert_inline_typechecks_cleanly(source, label);
    }
}

#[test]
fn rc_collection_keys_without_hash_eq_rejected() {
    for (label, source) in [
        (
            "HashMap.insert(Rc<i64>, _)",
            "fn main() { var m = HashMap.new(); let r = Rc.new(42); m.insert(r, \"val\"); }",
        ),
        (
            "HashSet.insert(Rc<i64>)",
            "fn main() { var s = HashSet.new(); let r = Rc.new(42); s.insert(r); }",
        ),
    ] {
        let output = typecheck_inline(source);
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::BoundsNotSatisfied),
            "{label} should require Hash + Eq, got: {:#?}",
            output.errors
        );
    }
}

// ── Safe patterns: collections with Copy / primitive types ──────────────

#[test]
fn vec_int_push_ok() {
    assert_no_unsafe_collection_element(
        r"
        fn main() {
            var v = Vec.new();
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
            var v = Vec.new();
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
            var m = HashMap.new();
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
            var s = HashSet.new();
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
            var m: HashMap<string, string> = HashMap.new();
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
            var m: HashMap<string, string> = HashMap.new();
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
            var m: HashMap<string, string> = HashMap.new();
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
            var m: HashMap<string, string> = HashMap.new();
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
            var v: Vec<i64> = Vec.new();
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

    let parse_result = hew_parser::parse("import std.issue856mod;\n\nfn main() {}\n");
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
            var m: HashMap<string, i64> = HashMap.new();
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
            let v: Vec<i64> = Vec.new();
            var c = v.clone();
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
            let m: HashMap<string, i64> = HashMap.new();
            var c = m.clone();
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
            let s: HashSet<string> = HashSet.new();
            var c = s.clone();
            c.insert("value");
            println(c.contains("value"));
        }"#,
        "HashSet.clone() should resolve and return a usable HashSet",
    );
}

// ── Named-wrapper transitive Rc support ─────────────────────────────────────────
//
// Aggregate collection lowering must carry Rc<T> fields through named
// structs/enums, including nested wrappers.

#[test]
fn rc_in_named_wrappers_collection_admission_supported() {
    for (label, source) in [
        (
            "Vec.push(Holder { val: Rc<i64> })",
            "type Holder { val: Rc<i64>; }\nfn main() { var v = Vec.new(); let h = Holder { val: Rc.new(1) }; v.push(h); }",
        ),
        (
            "HashMap.insert(_, Holder { val: Rc<i64> })",
            "type Holder { val: Rc<i64>; }\nfn main() { var m = HashMap.new(); let h = Holder { val: Rc.new(1) }; m.insert(\"k\", h); }",
        ),
        (
            "Vec.push(Outer wrapping Inner wrapping Rc<i64>)",
            "type Inner { x: Rc<i64>; }\ntype Outer { inner: Inner; }\nfn main() { var v = Vec.new(); let o = Outer { inner: Inner { x: Rc.new(1) } }; v.push(o); }",
        ),
        (
            "Vec.push(MaybeHolder::Some(Rc<i64>))",
            "enum MaybeHolder { Some(Rc<i64>); None; }\nfn main() { var v = Vec.new(); v.push(MaybeHolder.Some(Rc.new(1))); }",
        ),
    ] {
        assert_inline_typechecks_cleanly(source, label);
    }

    assert_no_unsafe_collection_element(
        "type Point { x: i64; y: i64; }\nfn main() { var v = Vec.new(); let p = Point { x: 1, y: 2 }; v.push(p); }",
        "Vec<Point> with no Rc should be fine",
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

// ── HashMap/HashSet admission fail-closed ────────────────────────────────────

#[test]
fn hashmap_error_key_type_fails_closed_silently() {
    // An already-errored key type (from an undefined name) should not produce
    // a *second* diagnostic about the HashMap admission — only one error.
    let output = typecheck_inline(
        r#"
        fn main() {
            let k = undefined_fn();
            var m = HashMap.new();
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
            var m = HashMap.new();
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
            var s = HashSet.new();
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
            var s = HashSet.new();
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
            var s = HashSet.new();
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
            var s = HashSet.new();
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
fn collection_annotation_infer_holes_single_error_per_span() {
    for (label, source, expect_exact_count) in [
        (
            "let HashSet<_>",
            "fn main() { let s: HashSet<_> = HashSet.new(); }",
            false,
        ),
        (
            "var HashSet<_> annotation only",
            "fn main() { var s: HashSet<_> = HashSet.new(); }",
            true,
        ),
        (
            "let HashMap<_, string>",
            "fn main() { let m: HashMap<_, string> = HashMap.new(); }",
            false,
        ),
        (
            "let HashMap<string, _>",
            "fn main() { let m: HashMap<string, _> = HashMap.new(); }",
            false,
        ),
        ("fn param HashSet<_>", "fn f(x: HashSet<_>) {}", true),
        (
            "fn param HashMap<_, string>",
            "fn f(x: HashMap<_, string>) {}",
            true,
        ),
        (
            "fn param HashMap<string, _>",
            "fn f(x: HashMap<string, _>) {}",
            true,
        ),
    ] {
        let output = typecheck_inline(source);
        let inference_failed: Vec<_> = output
            .errors
            .iter()
            .filter(|e| e.kind == TypeErrorKind::InferenceFailed)
            .collect();
        assert!(
            !inference_failed.is_empty(),
            "{label}: expected InferenceFailed, got: {:#?}",
            output.errors
        );
        if expect_exact_count {
            assert_eq!(
                inference_failed.len(),
                1,
                "{label}: expected 1 InferenceFailed, got: {:#?}",
                output.errors
            );
            if label == "var HashSet<_> annotation only" {
                assert!(
                    !inference_failed[0].message.contains("HashSet"),
                    "{label}: expected binding-level InferenceFailed, got: {}",
                    inference_failed[0].message
                );
            }
        }
        let mut seen_spans: std::collections::HashSet<String> = std::collections::HashSet::new();
        for e in inference_failed {
            let key = format!("{:?}", e.span);
            assert!(
                seen_spans.insert(key.clone()),
                "{label}: duplicate InferenceFailed at span {key}: {:#?}",
                output.errors
            );
        }
    }
}

#[test]
fn unresolved_collection_multiple_method_calls_single_lowering_error() {
    for (needle, source) in [
        (
            "HashSet",
            "fn main() { var s = HashSet.new(); let _ = s.len(); let _ = s.is_empty(); }",
        ),
        (
            "HashMap",
            "fn main() { var m = HashMap.new(); let _ = m.len(); let _ = m.is_empty(); }",
        ),
    ] {
        let output = typecheck_inline(source);
        let lowering_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| e.kind == TypeErrorKind::InferenceFailed && e.message.contains(needle))
            .collect();
        assert_eq!(
            lowering_errors.len(),
            1,
            "expected exactly one {needle} InferenceFailed, got: {:#?}",
            output.errors
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
                "duplicate InferenceFailed at span {key} for {needle}: {:#?}",
                output.errors
            );
        }
    }
}

// ── type_defs output-contract regressions ────────────────────────────────────

#[test]
fn type_defs_with_error_shapes_are_pruned_from_output() {
    for (label, source) in [
        (
            "struct error field",
            "type Broken {\n    value: Task<i64>;\n    ok: i64;\n}\n",
        ),
        (
            "enum error variant payload",
            "enum Broken {\n    Bad(Task<i64>);\n    Good(i64);\n}\n",
        ),
    ] {
        let output = typecheck_inline(source);
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::TaskNotNameable),
            "{label}: expected TaskNotNameable, got: {:#?}",
            output.errors
        );
        assert!(
            output.type_def_at_path("Broken").is_none(),
            "{label}: type_defs must prune errored shapes: {:#?}",
            output.type_defs
        );
    }

    for (label, source) in [
        (
            "method error param",
            "type Widget { value: i64; }\nimpl Widget {\n    fn good(self) -> i64 { self.value }\n    fn broken(self, bad: Task<i64>) -> i64 { self.value }\n}\n",
        ),
        (
            "method error return",
            "type Widget { value: i64; }\nimpl Widget {\n    fn good(self) -> i64 { self.value }\n    fn broken(self) -> Task<i64> { self.value }\n}\n",
        ),
    ] {
        let output = typecheck_inline(source);
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::TaskNotNameable),
            "{label}: expected TaskNotNameable, got: {:#?}",
            output.errors
        );
        let widget = output
            .type_def_at_path("Widget")
            .expect("type_defs should retain Widget when only a method signature is errored");
        assert!(
            widget.methods.contains_key("good"),
            "{label}: clean methods must survive: {:#?}",
            widget.methods
        );
        assert!(
            !widget.methods.contains_key("broken"),
            "{label}: errored method must be pruned: {:#?}",
            widget.methods
        );
    }
}

#[test]
fn unknown_return_annotation_single_error() {
    for (label, source) in [
        ("unknown fn return annotation", "fn f() -> UnknownType {}"),
        (
            "unknown lambda return annotation",
            "fn main() { let _f = || -> UnknownType { 1 }; }",
        ),
        (
            "unknown receive-fn return annotation",
            "actor Worker { receive fn run() -> UnknownType {} }\nfn main() {}",
        ),
    ] {
        let output = typecheck_inline(source);
        assert_single_unknown_return_error(&output, label);
    }
}

#[test]
fn call_type_args_failed_generic_call_pruned_at_boundary() {
    let source = r"
        fn id<T>(x: T) -> T { x }

        fn main() {
            let _ = id(Option.None);
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
// Deferred pipe method rewrite tests
//
// These tests cover the post-inference symbol-selection fix for
// Sink<T>::send, Stream<T>::recv, and Stream<T>::try_recv when the inner type
// T is only constrained *after* the call site.  The correct type-specific C
// symbol must be selected even when the call is visited before the
// surrounding context has narrowed T.
// ===========================================================================

/// `recv()` on a `Stream<i64>` pipe where the element type is inferred
/// from a downstream `let v: i64 = rx.recv()` annotation must still record
/// the layout-witness rewrite (`hew_stream_next_layout`) once deferred
/// resolution completes — the element kind rides the witness, not the symbol.
#[test]
fn deferred_pipe_recv_int_constrained_after_call() {
    let output = typecheck_inline(
        r"
        import std.stream;

        fn take_one() -> Option<i64> {
            let (tx, rx) = match stream.pipe(4) { .Ok(pair) => pair, .Err(error) => panic(error), };
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
                if c_symbol == "hew_stream_next_layout"
        )),
        "expected hew_stream_next_layout rewrite after deferred resolution, got: {:?}",
        output.method_call_rewrites
    );
}

/// `recv()` on a `Stream<string>` pipe where the element type is inferred
/// from the usage of the received value must emit `hew_stream_next_layout`.
#[test]
fn deferred_pipe_recv_string_constrained_after_call() {
    let output = typecheck_inline(
        r"
        import std.stream;

        fn take_one() -> Option<string> {
            let (tx, rx) = match stream.pipe(4) { .Ok(pair) => pair, .Err(error) => panic(error), };
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
                if c_symbol == "hew_stream_next_layout"
        )),
        "expected hew_stream_next_layout rewrite after deferred resolution, got: {:?}",
        output.method_call_rewrites
    );
}

/// `try_recv()` on a `Stream<i64>` where the element type is constrained
/// after the call site must resolve to the layout-witness `try_recv` entry.
#[test]
fn deferred_pipe_try_recv_int_constrained_after_call() {
    let output = typecheck_inline(
        r"
        import std.stream;

        fn try_take() -> Option<i64> {
            let (tx, rx) = match stream.pipe(4) { .Ok(pair) => pair, .Err(error) => panic(error), };
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
                if c_symbol == "hew_stream_try_next_layout"
        )),
        "expected hew_stream_try_next_layout rewrite after deferred resolution, got: {:?}",
        output.method_call_rewrites
    );
}

/// `send()` must defer when both the pipe's inner type and the sent value are
/// still `Ty::Var` at the call site, then record the layout-witness send
/// rewrite once a later `recv()` annotation constrains the shared pipe type.
#[test]
fn deferred_pipe_send_int_constrained_after_call() {
    let output = typecheck_inline(
        r"
        import std.stream;

        fn relay() {
            let (tx, rx) = match stream.pipe(4) { .Ok(pair) => pair, .Err(error) => panic(error), };
            if let .Some(v) = rx.recv() {
                let _ = tx.send(v);
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
                if c_symbol == "hew_stream_send_layout"
        )),
        "expected hew_stream_send_layout rewrite after deferred resolution, got: {:?}",
        output.method_call_rewrites
    );
}

/// When neither send nor recv arguments or annotations constrain the inner type
/// before the checker boundary, deferred pipe rewrite finalization must emit an
/// `InferenceFailed` error rather than silently recording the wrong symbol.
#[test]
fn deferred_pipe_unresolved_inner_fails_closed() {
    let output = typecheck_inline(
        r"
        import std.stream;

        fn untyped() {
            let (tx, rx) = match stream.pipe(4) { .Ok(pair) => pair, .Err(error) => panic(error), };
            let _ = rx.recv();
            tx.close();
        }
        ",
    );
    // Must produce InferenceFailed — T is genuinely unconstrained.
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InferenceFailed),
        "expected InferenceFailed for unresolved pipe inner type, got: {:#?}",
        output.errors
    );
    // The span must NOT have a rewrite recorded (codegen-fails-closed invariant).
    assert!(
        !output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            hew_types::MethodCallRewrite::RewriteToFunction { c_symbol, .. }
                if c_symbol.contains("hew_stream_next")
        )),
        "no recv rewrite should be recorded when inner type is unresolved: {:?}",
        output.method_call_rewrites
    );
}

/// NEW-7: `stream.recv()` over a `Stream<bytes>` typechecks cleanly — the
/// canonical suspending consumer surface.
#[test]
fn await_stream_recv_bytes_typechecks() {
    let output = typecheck_inline(
        "import std.stream;\n\n#[opaque]\ntype Pair {\n}\n\nextern \"C\" {\n    fn hew_stream_channel(capacity: i64) -> Pair;\n    fn hew_stream_pair_stream_bytes(pair: Pair) -> Stream<bytes>;\n    fn hew_stream_pair_free(pair: Pair);\n}\n\nactor Runner {\n    receive fn go(unused: i64) {\n        let pair = unsafe {\n            hew_stream_channel(4)\n        };\n        let input = unsafe {\n            hew_stream_pair_stream_bytes(pair)\n        };\n        unsafe {\n            hew_stream_pair_free(pair);\n        };\n        let item = input.recv();\n        match item {\n            .Some(v) => {}\n            .None => {}\n        }\n    }\n}\n\nfn main() {\n    let r = spawn Runner;\n    let _ = r.go(0);\n}\n",
    );
    assert!(
        output.errors.is_empty(),
        "stream.recv() over Stream<bytes> should typecheck cleanly, got: {:#?}",
        output.errors
    );
}

/// NEW-7 widened: an `i64` element rides the element-layout witness — the
/// suspend lowering is no longer bound to string/bytes, so
/// `stream.recv()` over `Stream<i64>` typechecks cleanly.
#[test]
fn await_stream_recv_int_element_admitted() {
    let output = typecheck_inline(
        "import std.stream;\n\n#[opaque]\ntype Pair {\n}\n\nextern \"C\" {\n    fn hew_stream_channel(capacity: i64) -> Pair;\n    fn hew_stream_pair_stream_i64(pair: Pair) -> Stream<i64>;\n    fn hew_stream_pair_free(pair: Pair);\n}\n\nactor Runner {\n    receive fn go(unused: i64) {\n        let pair = unsafe {\n            hew_stream_channel(4)\n        };\n        let input = unsafe {\n            hew_stream_pair_stream_i64(pair)\n        };\n        unsafe {\n            hew_stream_pair_free(pair);\n        };\n        let item = input.recv();\n        match item {\n            .Some(v) => {}\n            .None => {}\n        }\n    }\n}\n\nfn main() {\n    let r = spawn Runner;\n    let _ = r.go(0);\n}\n",
    );
    assert!(
        output.errors.is_empty(),
        "stream.recv() over Stream<i64> must be admitted by the \
         element-layout witness, got: {:#?}",
        output.errors
    );
}

// ── WASM gate: std::crypto native-only surfaces ─────────────────────────────

/// `std::crypto::encrypt` module calls are admitted on wasm32 now that
/// AES-256-GCM runs on pure-Rust crates across native and `wasm32-wasip1`.
#[test]
fn wasm_admits_crypto_encrypt_module_calls() {
    let output = typecheck_inline_wasm(
        "import std.crypto.encrypt;\n\
         fn main() {\n\
             let _ = encrypt.seal(bytes [0x61], \"hello\");\n\
             let _ = encrypt.open(bytes [0x61], bytes [0x62]);\n\
         }",
    );
    assert!(
        output.errors.is_empty(),
        "expected encrypt module calls to be admitted on wasm32, got: {:#?}",
        output.errors
    );
}

/// `std::crypto::sign` module calls are admitted on wasm32 now that Ed25519
/// runs on pure-Rust crates across native and `wasm32-wasip1`.
#[test]
fn wasm_admits_crypto_sign_module_calls() {
    let output = typecheck_inline_wasm(
        r#"
        import std.crypto.sign;

        fn main() {
            let key = sign.from_seed(sign.generate_seed()).expect("seed");
            let msg = bytes [0x01, 0x02, 0x03];
            let sig = key.sign(msg);
            let _ = sign.verify(msg, sig, key.public_key());
            key.close();
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "expected sign module calls to be admitted on wasm32, got: {:#?}",
        output.errors
    );
}

/// Native target parity: the same encrypt/sign module calls that are rejected
/// on wasm32 must NOT produce a `PlatformLimitation` error on the native target.
#[test]
fn native_allows_crypto_encrypt_and_sign_module_calls() {
    let encrypt_output = typecheck_inline(
        "import std.crypto.encrypt;\n\
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
        import std.crypto.sign;

        fn main() {
            let key = sign.from_seed(sign.generate_seed());
            let msg = bytes [0x01, 0x02, 0x03];
            let _ = sign.verify(msg, msg, msg);
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
// Machine pipe elements (transition-watch lane, stage-0 pin)
// ===========================================================================

/// A monomorphic machine value as a pipe element typechecks clean —
/// machines are enums at the value-classification layer and ride the
/// owned-element queue witness. Was the stage-0 fail-closed pin
/// (`machine_channel_element_currently_fails_closed`) before the machine
/// admission landed.
#[test]
fn monomorphic_machine_pipe_element_admitted() {
    let output = typecheck_inline(
        r"import std.stream;

machine Light {
    events {
        Flip;
    }
    state Off;
    state On;
    on Flip: Off => .On;
    on Flip: On => .Off;
}

fn main() {
    let (tx, rx): (stream.Sink<Light>, stream.Stream<Light>) = match stream.pipe(2) {
        .Ok(pair) => pair,
        .Err(error) => panic(error),
    };
    let _ = tx.send(Light.Off);
    tx.close();
    let _ = rx.recv();
    rx.close();
}
",
    );
    assert!(
        output.errors.is_empty(),
        "monomorphic machine pipe element must be admitted; got: {:#?}",
        output.errors
    );
}

/// Discovery metadata grants no actor dispatch capability.
#[test]
fn location_receive_fn_dispatch_with_pipe_handle_refused() {
    let output = typecheck_inline(
        r"
        import std.stream;

        actor Observer {
            receive fn watch(rx: stream.Stream<string>) {
                rx.close();
            }
        }

        fn forward(o: Location, rx: stream.Stream<string>) {
            o.watch(rx);
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("no method `watch` on `Location`")),
        "discovery metadata receive-fn dispatch must stay refused; got: {:#?}",
        output.errors
    );
}

fn wire_actor_call_source(ping: &str) -> String {
    format!(
        r"
        {ping}
        actor Echo {{
            #[wire]
            receive fn handle(msg: Ping) -> i64 {{ msg.seq }}
        }}
        fn call(echo: Echo) -> i64 {{
            match echo.handle(Ping {{ seq: 1 }}) {{
                .Ok(value) => value,
                .Err(_) => 0,
            }}
        }}
        "
    )
}

/// Wire handlers require tagged payloads throughout the complete value shape.
#[test]
fn wire_actor_handler_requires_explicit_wire_schema() {
    let plain = typecheck_inline(&wire_actor_call_source("type Ping {\n    seq: i64;\n}\n"));
    plain
        .errors
        .iter()
        .find(|e| {
            e.message
                .contains("wire handler `Echo.handle` reaches `Ping`")
                && e.message.contains("without `#[wire]` tags")
        })
        .unwrap_or_else(|| panic!("plain record must be refused; got: {:#?}", plain.errors));
    let wire = typecheck_inline(&wire_actor_call_source(
        "#[wire]\ntype Ping {\n    seq: i64 @1;\n}\n",
    ));
    assert!(
        wire.errors.is_empty(),
        "a tagged #[wire] message crosses the node: {:#?}",
        wire.errors
    );

    // Every record a remote payload reaches is tagged; the refusal names the field.
    let nested = typecheck_inline(&wire_actor_call_source(
        "type Inner {\n    v: i64;\n}\n#[wire]\ntype Ping {\n    seq: i64 @1;\n    inner: Inner @2;\n}\n",
    ));
    assert!(
        nested
            .errors
            .iter()
            .any(|e| e.message.contains("reaches `Inner` at `.inner`")),
        "a #[wire] message reaching a plain record must be refused at the field: {:#?}",
        nested.errors
    );
}

#[test]
fn signed_module_const_arithmetic_is_checked_in_its_declared_type() {
    let output = typecheck_inline(
        r"
        const NIL: i64 = 0 - 1;
        const PAREN: i16 = -(1 + 2);
        const MIN: i8 = -128;
        fn main() { println(NIL); }
        ",
    );
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}

#[test]
fn invalid_module_const_integer_arithmetic_has_specific_checker_diagnostics() {
    for (source, expected) in [
        (
            "const BAD: u8 = 0 - 1;",
            "arithmetic overflows declared type `u8`",
        ),
        (
            "const BAD: i8 = 127 + 1;",
            "arithmetic overflows declared type `i8`",
        ),
        (
            "const BAD: i8 = -(-(-128));",
            "arithmetic overflows declared type `i8`",
        ),
        ("const BAD: i64 = 1 / 0;", "divides by zero"),
        ("const BAD: u8 = 256;", "does not fit in declared type `u8`"),
    ] {
        let output = typecheck_inline(source);
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.message.contains(expected)),
            "expected `{expected}` for `{source}`, got {:#?}",
            output.errors
        );
        assert!(
            !output
                .errors
                .iter()
                .any(|error| error.kind.as_kind_str() == "NotYetImplemented"),
            "integer const rejection must not be NYI: {:#?}",
            output.errors
        );
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.kind.as_kind_str() == "E_CONST_INITIALIZER"),
            "integer const rejection must use E_CONST_INITIALIZER: {:#?}",
            output.errors
        );
    }
}

#[test]
fn misspelled_field_and_variant_types_report_undefined_type() {
    for (source, name) in [
        ("type Foo {\n    x: Misspelled;\n}\nfn main() {}\n", "Foo"),
        ("enum E {\n    A(Misspelled);\n}\nfn main() {}\n", "E"),
    ] {
        let output = typecheck_inline(source);
        assert!(
            output.errors.iter().any(|error| {
                matches!(error.kind, hew_types::error::TypeErrorKind::UndefinedType)
                    && error.message.contains("Misspelled")
            }),
            "expected UndefinedType for `{name}` with misspelled member type, got {:#?}",
            output.errors
        );
        assert!(
            output.type_def_at_path(name).is_none(),
            "declaration `{name}` with error-typed member must be pruned from output type_defs"
        );
    }
}

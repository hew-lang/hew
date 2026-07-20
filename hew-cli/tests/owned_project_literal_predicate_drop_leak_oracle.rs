//! Leak, poisoned-allocator, and exact-close oracle for owned literal-predicate
//! project matches. Predicate-only record and tuple chains borrow their source;
//! retained string predicate loads drop before branching, while the caller's
//! composite owner remains responsible for one resource close and one heap
//! teardown on both match and mismatch paths.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

fn predicate_borrow_source(frames: usize) -> String {
    format!(
        "#[resource] type Token {{ id: i64; }}\n\
         impl Token {{\n\
         \x20   fn close(self) {{\n\
         \x20       if self.id == 1 {{ println(\"record-match-close\"); }}\n\
         \x20       else if self.id == 2 {{ println(\"record-mismatch-close\"); }}\n\
         \x20       else if self.id == 3 {{ println(\"tuple-match-close\"); }}\n\
         \x20       else {{ println(\"tuple-mismatch-close\"); }}\n\
         \x20   }}\n\
         }}\n\
         type Packet {{ tag: string, token: Token }}\n\
         fn classify_record(p: Packet) -> i64 {{\n\
         \x20   match p {{\n\
         \x20       Packet {{ tag: \"hit\", token: _ }} => 1,\n\
         \x20       Packet {{ tag: _, token: _ }} => 2,\n\
         \x20   }}\n\
         }}\n\
         fn classify_tuple(p: (string, Token)) -> i64 {{\n\
         \x20   match p {{\n\
         \x20       (\"hit\", _) => 3,\n\
         \x20       (_, _) => 4,\n\
         \x20   }}\n\
         }}\n\
         fn record_match() -> i64 {{\n\
         \x20   let p = Packet {{ tag: \"h\" + \"it\", token: Token {{ id: 1 }} }};\n\
         \x20   classify_record(p)\n\
         }}\n\
         fn record_mismatch() -> i64 {{\n\
         \x20   let p = Packet {{ tag: \"m\" + \"iss\", token: Token {{ id: 2 }} }};\n\
         \x20   classify_record(p)\n\
         }}\n\
         fn tuple_match() -> i64 {{\n\
         \x20   let p = (\"h\" + \"it\", Token {{ id: 3 }});\n\
         \x20   classify_tuple(p)\n\
         }}\n\
         fn tuple_mismatch() -> i64 {{\n\
         \x20   let p = (\"m\" + \"iss\", Token {{ id: 4 }});\n\
         \x20   classify_tuple(p)\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for _ in 0..{frames} {{\n\
         \x20       total = total + record_match();\n\
         \x20       total = total + record_mismatch();\n\
         \x20       total = total + tuple_match();\n\
         \x20       total = total + tuple_mismatch();\n\
         \x20   }}\n\
         \x20   if total != {frames} * 10 {{ return 79; }}\n\
         \x20   println(\"predicate-borrow-ok\");\n\
         \x20   0\n\
         }}\n"
    )
}

#[test]
fn owned_project_predicate_borrow_leak_slope_is_flat() {
    assert_frame_slope_below_tolerance("owned_project_predicate_borrow", predicate_borrow_source);
}

#[test]
fn owned_project_predicate_borrow_releases_each_owner_once() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("owned-project-predicate-borrow-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &predicate_borrow_source(1),
        dir.path(),
        "owned_project_predicate_borrow_scribble",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "owned predicate-only paths must run clean under the poisoned allocator;\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "record-match-close\nrecord-mismatch-close\ntuple-match-close\n\
         tuple-mismatch-close\npredicate-borrow-ok\n",
        "each match/mismatch owner must close exactly once;\n{}",
        describe_output(&output)
    );
}

fn predicate_consume_source(frames: usize) -> String {
    format!(
        "#[resource] type Token {{ id: i64; }}\n\
         impl Token {{ fn close(self) {{ println(self.id); }} }}\n\
         type Packet {{ tag: i64, token: Token, text: string }}\n\
         fn record_join(p: Packet) -> i64 {{\n\
         \x20   match p {{\n\
         \x20       Packet {{ tag: 0, token, text: _ }} => token.id,\n\
         \x20       Packet {{ tag: _, token, text: _ }} => token.id,\n\
         \x20   }}\n\
         }}\n\
         fn tuple_join(p: (i64, Token, string)) -> i64 {{\n\
         \x20   match p {{\n\
         \x20       (0, token, _) => token.id,\n\
         \x20       (_, token, _) => token.id,\n\
         \x20   }}\n\
         }}\n\
         fn direct_return(p: Packet) -> i64 {{\n\
         \x20   match p {{\n\
         \x20       Packet {{ tag: 0, token, text: _ }} => return token.id,\n\
         \x20       Packet {{ tag: _, token, text: _ }} => token.id,\n\
         \x20   }}\n\
         }}\n\
         fn whole_fallback(p: Packet) -> i64 {{\n\
         \x20   match p {{\n\
         \x20       Packet {{ tag: 0, token, text: _ }} => token.id,\n\
         \x20       whole => whole.token.id,\n\
         \x20   }}\n\
         }}\n\
         fn packet(tag: i64, id: i64) -> Packet {{\n\
         \x20   Packet {{ tag: tag, token: Token {{ id: id }}, text: \"pay\" + \"load\" }}\n\
         }}\n\
         fn tuple_value(tag: i64, id: i64) -> (i64, Token, string) {{\n\
         \x20   (tag, Token {{ id: id }}, \"pay\" + \"load\")\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for _ in 0..{frames} {{\n\
         \x20       total = total + record_join(packet(0, 1));\n\
         \x20       total = total + record_join(packet(9, 2));\n\
         \x20       total = total + tuple_join(tuple_value(0, 3));\n\
         \x20       total = total + tuple_join(tuple_value(9, 4));\n\
         \x20       total = total + direct_return(packet(0, 5));\n\
         \x20       total = total + direct_return(packet(9, 6));\n\
         \x20       total = total + whole_fallback(packet(0, 7));\n\
         \x20       total = total + whole_fallback(packet(9, 8));\n\
         \x20   }}\n\
         \x20   if total != {frames} * 36 {{ return 80; }}\n\
         \x20   println(\"predicate-consume-ok\");\n\
         \x20   0\n\
         }}\n"
    )
}

#[test]
fn owned_project_predicate_consume_leak_slope_is_flat() {
    assert_frame_slope_below_tolerance("owned_project_predicate_consume", predicate_consume_source);
}

#[test]
fn owned_project_predicate_consume_releases_each_owner_once() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("owned-project-predicate-consume-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &predicate_consume_source(1),
        dir.path(),
        "owned_project_predicate_consume_scribble",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "owned consuming predicate paths must run clean under the poisoned allocator;\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "1\n2\n3\n4\n5\n6\n7\n8\npredicate-consume-ok\n",
        "each selected-arm resource owner must close exactly once;\n{}",
        describe_output(&output)
    );
}

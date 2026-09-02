//! Actor-handler owned-parameter disposition oracle.
//!
//! A mailbox delivery is an ownership transfer. The handler frame must release
//! a delivered heap value when the body only ignores or borrows it, including
//! every branch and early-return edge. Conversely, a value moved into actor
//! state or forwarded to another mailbox must have exactly one downstream
//! owner and no handler-exit release.
//!
//! The slope cases are the missing-drop counterfactual: deleting the handler
//! parameter mint leaks at least one allocation per message. The poisoned
//! state/forwarding control is the extra-drop counterfactual: retaining the
//! handler release after transfer double-frees or scribbles the exact sentinel.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

fn ignored_string_source(frames: usize) -> String {
    format!(
        "actor ProbeSink {{\n\
         \x20   var seen: i64;\n\
         \x20   receive fn take(label: string) {{ seen = seen + 1; }}\n\
         \x20   receive fn count() -> i64 {{ seen }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let sink = spawn ProbeSink(seen: 0);\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       sink.take(\"unused\".to_upper());\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   match await sink.count() {{ Ok(n) => if n == {frames} {{ 0 }} else {{ 71 }}, Err(_) => 72 }}\n\
         }}\n"
    )
}

fn ignored_recursive_record_source(frames: usize) -> String {
    format!(
        "type Inner {{ label: string; values: Vec<i64> }}\n\
         type Envelope {{ payload: Inner }}\n\
         actor ProbeSink {{\n\
         \x20   var seen: i64;\n\
         \x20   receive fn take(message: Envelope) {{ seen = seen + 1; }}\n\
         \x20   receive fn count() -> i64 {{ seen }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let sink = spawn ProbeSink(seen: 0);\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let message = Envelope {{ payload: Inner {{ label: \"record\".to_upper(), values: [i, i + 1] }} }};\n\
         \x20       sink.take(message);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   match await sink.count() {{ Ok(n) => if n == {frames} {{ 0 }} else {{ 73 }}, Err(_) => 74 }}\n\
         }}\n"
    )
}

fn ignored_container_source(frames: usize) -> String {
    format!(
        "actor ProbeSink {{\n\
         \x20   var seen: i64;\n\
         \x20   receive fn take(values: Vec<string>) {{ seen = seen + 1; }}\n\
         \x20   receive fn count() -> i64 {{ seen }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let sink = spawn ProbeSink(seen: 0);\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let values: Vec<string> = [\"left\".to_upper(), \"right\".to_upper()];\n\
         \x20       sink.take(values);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   match await sink.count() {{ Ok(n) => if n == {frames} {{ 0 }} else {{ 75 }}, Err(_) => 76 }}\n\
         }}\n"
    )
}

fn branch_and_early_exit_source(frames: usize) -> String {
    format!(
        "actor ProbeSink {{\n\
         \x20   var seen: i64;\n\
         \x20   receive fn take(label: string, early: bool) {{\n\
         \x20       if early {{ seen = seen + 1; return; }}\n\
         \x20       seen = seen + 1;\n\
         \x20   }}\n\
         \x20   receive fn count() -> i64 {{ seen }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let sink = spawn ProbeSink(seen: 0);\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       sink.take(\"branch\".to_upper(), i % 2 == 0);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   match await sink.count() {{ Ok(n) => if n == {frames} {{ 0 }} else {{ 77 }}, Err(_) => 78 }}\n\
         }}\n"
    )
}

fn state_or_drop_source(frames: usize) -> String {
    format!(
        "actor Keeper {{\n\
         \x20   var seen: i64;\n\
         \x20   var last: string;\n\
         \x20   receive fn take(label: string, keep: bool) {{\n\
         \x20       if keep {{ last = label; }} else {{ seen = seen + label.len(); }}\n\
         \x20   }}\n\
         \x20   receive fn total() -> i64 {{ seen + last.len() }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let sink = spawn Keeper(seen: 0, last: \"seed\");\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       sink.take(\"state\".to_upper(), i % 2 == 0);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   match await sink.total() {{ Ok(n) => if n > 0 {{ 0 }} else {{ 79 }}, Err(_) => 80 }}\n\
         }}\n"
    )
}

fn conditional_local_record_handoff_source(frames: usize) -> String {
    let expected = frames.saturating_mul(7).saturating_add(4);
    format!(
        "type Wrap {{ name: string }}\n\
         actor Fan {{\n\
         \x20   var seen: i64;\n\
         \x20   var held: Wrap;\n\
         \x20   receive fn route(label: string, store: bool) {{\n\
         \x20       let next = Wrap {{ name: label }};\n\
         \x20       if store {{ held = next; }}\n\
         \x20       else {{ seen = seen + next.name.len(); }}\n\
         \x20   }}\n\
         \x20   receive fn total() -> i64 {{ seen + held.name.len() }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let fan = spawn Fan(seen: 0, held: Wrap {{ name: \"seed\" }});\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       fan.route(\"discard\".to_upper(), false);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   match await fan.total() {{ Ok(n) => if n == {expected} {{ 0 }} else {{ 99 }}, Err(_) => 100 }}\n\
         }}\n"
    )
}

fn loop_carried_record_ingress_source(frames: usize) -> String {
    format!(
        "type Wrap {{ name: string }}\n\
         actor Fan {{\n\
         \x20   var seen: i64;\n\
         \x20   var held: Wrap;\n\
         \x20   receive fn route(label: string, count: i64) {{\n\
         \x20       var j: i64 = 0;\n\
         \x20       while j < count {{\n\
         \x20           let next = Wrap {{ name: label }};\n\
         \x20           if j > 0 {{ seen = seen + 1; }}\n\
         \x20           held = next;\n\
         \x20           j = j + 1;\n\
         \x20       }}\n\
         \x20       seen = seen + 1;\n\
         \x20   }}\n\
         \x20   receive fn total() -> i64 {{ seen + held.name.len() }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let fan = spawn Fan(seen: 0, held: Wrap {{ name: \"seed\" }});\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       fan.route(\"loop\".to_upper(), i % 5);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   match await fan.total() {{ Ok(n) => if n > {frames} {{ 0 }} else {{ 85 }}, Err(_) => 86 }}\n\
         }}\n"
    )
}

fn nested_loop_carried_record_ingress_source(frames: usize) -> String {
    format!(
        "type Inner {{ name: string }}\n\
         type Outer {{ inner: Inner }}\n\
         actor Fan {{\n\
         \x20   var seen: i64;\n\
         \x20   var held: Outer;\n\
         \x20   receive fn route(label: string, count: i64) {{\n\
         \x20       var j: i64 = 0;\n\
         \x20       while j < count {{\n\
         \x20           held = Outer {{ inner: Inner {{ name: label }} }};\n\
         \x20           j = j + 1;\n\
         \x20       }}\n\
         \x20       seen = seen + 1;\n\
         \x20   }}\n\
         \x20   receive fn total() -> i64 {{ seen + held.inner.name.len() }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let fan = spawn Fan(seen: 0, held: Outer {{ inner: Inner {{ name: \"seed\" }} }});\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       fan.route(\"nested\".to_upper(), i % 5);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   match await fan.total() {{ Ok(n) => if n > {frames} {{ 0 }} else {{ 87 }}, Err(_) => 88 }}\n\
         }}\n"
    )
}

fn branch_selected_record_ingress_source(frames: usize) -> String {
    format!(
        "type Wrap {{ name: string }}\n\
         actor Fan {{\n\
         \x20   var seen: i64;\n\
         \x20   var held: Wrap;\n\
         \x20   receive fn route(label: string, count: i64) {{\n\
         \x20       var j: i64 = 0;\n\
         \x20       while j < count {{\n\
         \x20           let next = if j > 0 {{\n\
         \x20               Wrap {{ name: label }}\n\
         \x20           }} else {{\n\
         \x20               Wrap {{ name: label }}\n\
         \x20           }};\n\
         \x20           held = next;\n\
         \x20           j = j + 1;\n\
         \x20       }}\n\
         \x20       seen = seen + 1;\n\
         \x20   }}\n\
         \x20   receive fn total() -> i64 {{ seen + held.name.len() }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let fan = spawn Fan(seen: 0, held: Wrap {{ name: \"seed\" }});\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       fan.route(\"branch\".to_upper(), i % 5);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   match await fan.total() {{ Ok(n) => if n > {frames} {{ 0 }} else {{ 89 }}, Err(_) => 90 }}\n\
         }}\n"
    )
}

fn branch_selected_nested_record_ingress_source(frames: usize) -> String {
    format!(
        "type Inner {{ name: string }}\n\
         type Outer {{ inner: Inner }}\n\
         actor Fan {{\n\
         \x20   var seen: i64;\n\
         \x20   var held: Outer;\n\
         \x20   receive fn route(label: string, count: i64) {{\n\
         \x20       var j: i64 = 0;\n\
         \x20       while j < count {{\n\
         \x20           let inner = if j > 0 {{\n\
         \x20               Inner {{ name: label }}\n\
         \x20           }} else {{\n\
         \x20               Inner {{ name: label }}\n\
         \x20           }};\n\
         \x20           let next = Outer {{ inner: inner }};\n\
         \x20           held = next;\n\
         \x20           j = j + 1;\n\
         \x20       }}\n\
         \x20       seen = seen + 1;\n\
         \x20   }}\n\
         \x20   receive fn total() -> i64 {{ seen + held.inner.name.len() }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let fan = spawn Fan(seen: 0, held: Outer {{ inner: Inner {{ name: \"seed\" }} }});\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       fan.route(\"nested-branch\".to_upper(), i % 5);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   match await fan.total() {{ Ok(n) => if n > {frames} {{ 0 }} else {{ 93 }}, Err(_) => 94 }}\n\
         }}\n"
    )
}

fn cross_field_swapped_record_ingress_source(frames: usize) -> String {
    format!(
        "type Inner {{ name: string }}\n\
         type Pair {{ left: Inner; right: Inner }}\n\
         actor Fan {{\n\
         \x20   var seen: i64;\n\
         \x20   var held: Pair;\n\
         \x20   receive fn route(label: string, other: string, count: i64) {{\n\
         \x20       var j: i64 = 0;\n\
         \x20       while j < count {{\n\
         \x20           let named = Inner {{ name: label }};\n\
         \x20           let alternate = Inner {{ name: other }};\n\
         \x20           let next = if j % 2 == 0 {{\n\
         \x20               Pair {{ left: named, right: alternate }}\n\
         \x20           }} else {{\n\
         \x20               Pair {{ left: alternate, right: named }}\n\
         \x20           }};\n\
         \x20           held = next;\n\
         \x20           j = j + 1;\n\
         \x20       }}\n\
         \x20       seen = seen + 1;\n\
         \x20   }}\n\
         \x20   receive fn total() -> i64 {{\n\
         \x20       seen + held.left.name.len() + held.right.name.len()\n\
         \x20   }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let fan = spawn Fan(\n\
         \x20       seen: 0,\n\
         \x20       held: Pair {{ left: Inner {{ name: \"left\" }}, right: Inner {{ name: \"right\" }} }}\n\
         \x20   );\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       fan.route(\"named\".to_upper(), \"alternate\".to_upper(), i % 5);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   match await fan.total() {{ Ok(n) => if n > {frames} {{ 0 }} else {{ 91 }}, Err(_) => 92 }}\n\
         }}\n"
    )
}

fn shifted_repeated_record_ingress_source(frames: usize) -> String {
    format!(
        "type Inner {{ name: string }}\n\
         type Triple {{ a: Inner; b: Inner; c: Inner }}\n\
         actor Fan {{\n\
         \x20   var seen: i64;\n\
         \x20   var held: Triple;\n\
         \x20   receive fn route(label: string, other: string, count: i64) {{\n\
         \x20       var j: i64 = 0;\n\
         \x20       while j < count {{\n\
         \x20           let next = if j % 2 == 0 {{\n\
         \x20               Triple {{\n\
         \x20                   a: Inner {{ name: other }},\n\
         \x20                   b: Inner {{ name: label }},\n\
         \x20                   c: Inner {{ name: label }}\n\
         \x20               }}\n\
         \x20           }} else {{\n\
         \x20               Triple {{\n\
         \x20                   a: Inner {{ name: label }},\n\
         \x20                   b: Inner {{ name: label }},\n\
         \x20                   c: Inner {{ name: other }}\n\
         \x20               }}\n\
         \x20           }};\n\
         \x20           held = next;\n\
         \x20           j = j + 1;\n\
         \x20       }}\n\
         \x20       seen = seen + 1;\n\
         \x20   }}\n\
         \x20   receive fn total() -> i64 {{\n\
         \x20       seen + held.a.name.len() + held.b.name.len() + held.c.name.len()\n\
         \x20   }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let fan = spawn Fan(\n\
         \x20       seen: 0,\n\
         \x20       held: Triple {{\n\
         \x20           a: Inner {{ name: \"a\" }},\n\
         \x20           b: Inner {{ name: \"b\" }},\n\
         \x20           c: Inner {{ name: \"c\" }}\n\
         \x20       }}\n\
         \x20   );\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       fan.route(\"repeat\".to_upper(), \"other\".to_upper(), i % 5);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   match await fan.total() {{ Ok(n) => if n > {frames} {{ 0 }} else {{ 93 }}, Err(_) => 94 }}\n\
         }}\n"
    )
}

fn branch_to_two_state_fields_source(frames: usize) -> String {
    format!(
        "type Wrap {{ name: string }}\n\
         actor Fan {{\n\
         \x20   var seen: i64;\n\
         \x20   var one: Wrap;\n\
         \x20   var two: Wrap;\n\
         \x20   receive fn route(label: string, choose_one: bool) {{\n\
         \x20       let next = Wrap {{ name: label }};\n\
         \x20       if choose_one {{ one = next; }} else {{ two = next; }}\n\
         \x20       seen = seen + 1;\n\
         \x20   }}\n\
         \x20   receive fn total() -> i64 {{ seen + one.name.len() + two.name.len() }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let fan = spawn Fan(\n\
         \x20       seen: 0,\n\
         \x20       one: Wrap {{ name: \"one\" }},\n\
         \x20       two: Wrap {{ name: \"two\" }}\n\
         \x20   );\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       fan.route(\"either\".to_upper(), i % 2 == 0);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   match await fan.total() {{ Ok(n) => if n > {frames} {{ 0 }} else {{ 95 }}, Err(_) => 96 }}\n\
         }}\n"
    )
}

const FORWARDED_POISON_SOURCE: &str = "\
actor Consumer {\n\
\x20   var seen: i64;\n\
\x20   var last: string;\n\
\x20   receive fn take(value: string) -> i64 { last = value; seen = seen + 1; seen }\n\
\x20   receive fn total() -> i64 { if last.contains(\"FORWARD\") { seen } else { -1 } }\n\
}\n\
actor Relay {\n\
\x20   let consumer: LocalPid<Consumer>;\n\
\x20   var seen: i64;\n\
\x20   receive fn forward(value: string) -> i64 {\n\
\x20       let delivered = match await consumer.take(value) { .Ok(n) => n, Err(_) => -1 };\n\
\x20       seen = seen + 1;\n\
\x20       delivered\n\
\x20   }\n\
\x20   receive fn count() -> i64 { seen }\n\
}\n\
fn main() -> i64 {\n\
\x20   let consumer = spawn Consumer(seen: 0, last: \"seed\");\n\
\x20   let relay = spawn Relay(consumer: consumer, seen: 0);\n\
\x20   var i: i64 = 0;\n\
\x20   while i < 40 {\n\
\x20       let delivered = match await relay.forward(\"forward\".to_upper()) { .Ok(n) => n, Err(_) => -1 };\n\
\x20       if delivered < 0 { return 82; }\n\
\x20       i = i + 1;\n\
\x20   }\n\
\x20   let relayed = match await relay.count() { .Ok(n) => n, Err(_) => -1 };\n\
\x20   let consumed = match await consumer.total() { .Ok(n) => n, Err(_) => -1 };\n\
\x20   if relayed == 40 && consumed == 40 { 0 } else { 81 }\n\
}\n";

const CONDITIONAL_RECORD_INGRESS_POISON_SOURCE: &str = "\
type Wrap { name: string }\n\
actor Fan {\n\
\x20   var seen: i64;\n\
\x20   var held: Wrap;\n\
\x20   var last: string;\n\
\x20   receive fn route(label: string, mode: i64) {\n\
\x20       if mode == 0 { held = Wrap { name: label }; }\n\
\x20       else { last = label; }\n\
\x20       seen = seen + 1;\n\
\x20   }\n\
\x20   receive fn total() -> i64 { seen + held.name.len() + last.len() }\n\
}\n\
fn main() -> i64 {\n\
\x20   let fan = spawn Fan(seen: 0, held: Wrap { name: \"held\" }, last: \"last\");\n\
\x20   var i: i64 = 0;\n\
\x20   while i < 40 {\n\
\x20       fan.route(\"payload\".to_upper(), i % 2);\n\
\x20       i = i + 1;\n\
\x20   }\n\
\x20   match await fan.total() { Ok(n) => if n > 40 { 0 } else { 83 }, Err(_) => 84 }\n\
}\n";

const BRANCH_LOCAL_FRESH_NESTED_POISON_SOURCE: &str = "\
type Inner { name: string }\n\
type Outer { inner: Inner }\n\
actor Fan {\n\
\x20   var seen: i64;\n\
\x20   var held: Outer;\n\
\x20   receive fn route(flag: bool, other: bool) {\n\
\x20       let selected = if flag {\n\
\x20           let fresh = \"hot\".to_upper();\n\
\x20           Inner { name: fresh }\n\
\x20       } else {\n\
\x20           Inner { name: \"cold\" }\n\
\x20       };\n\
\x20       if other { held = Outer { inner: selected }; }\n\
\x20       else { held = Outer { inner: selected }; }\n\
\x20       seen = seen + 1;\n\
\x20   }\n\
\x20   receive fn total() -> i64 { seen + held.inner.name.len() }\n\
}\n\
fn main() -> i64 {\n\
\x20   let fan = spawn Fan(seen: 0, held: Outer { inner: Inner { name: \"seed\" } });\n\
\x20   var i: i64 = 0;\n\
\x20   while i < 12 {\n\
\x20       fan.route(i % 2 == 0, i % 3 == 0);\n\
\x20       i = i + 1;\n\
\x20   }\n\
\x20   match await fan.total() { Ok(n) => if n > 12 { 0 } else { 97 }, Err(_) => 98 }\n\
}\n";

const CONDITIONAL_LOCAL_RECORD_HANDOFF_POISON_SOURCE: &str = "\
type Wrap { name: string }\n\
actor Fan {\n\
\x20   var seen: i64;\n\
\x20   var held: Wrap;\n\
\x20   receive fn route(label: string, store: bool) {\n\
\x20       let next = Wrap { name: label };\n\
\x20       if store { held = next; }\n\
\x20       else { seen = seen + next.name.len(); }\n\
\x20   }\n\
\x20   receive fn total() -> i64 { seen + held.name.len() }\n\
}\n\
fn main() -> i64 {\n\
\x20   let fan = spawn Fan(seen: 0, held: Wrap { name: \"seed\" });\n\
\x20   var i: i64 = 0;\n\
\x20   while i < 40 {\n\
\x20       fan.route(\"handoff\".to_upper(), i % 2 == 0);\n\
\x20       i = i + 1;\n\
\x20   }\n\
\x20   match await fan.total() { Ok(n) => if n > 40 { 0 } else { 101 }, Err(_) => 102 }\n\
}\n";

/// Counterfactual for the resource/record classification overlap. The local is
/// a direct String-record construction (so it matches W60.115's ingress shape)
/// and a `#[resource]` (so only the affine close flag may govern it). The close
/// reads the payload under the poisoned allocator, making a second close of the
/// neutralized source observable.
const DIRECT_STRING_RESOURCE_TRANSFER_POISON_SOURCE: &str = "\
#[resource]\n\
type Token { text: string }\n\
impl Token {\n\
\x20   fn close(self) {\n\
\x20       let _ = self.text.len();\n\
\x20   }\n\
}\n\
fn take(consume token: Token) -> i64 {\n\
\x20   token.text.len()\n\
}\n\
fn main() -> i64 {\n\
\x20   let token = Token { text: \"payload\".to_upper() };\n\
\x20   let transfer = \"go\".len() == 2;\n\
\x20   var seen: i64 = 0;\n\
\x20   if transfer { seen = take(token); }\n\
\x20   if seen == 7 { 0 } else { 103 }\n\
}\n";

/// Counterfactual for the two-flag collision between W60.115 and #2301. The
/// mutable direct record is moved on alternating edges, then overwritten on
/// every iteration. #2301 must suppress the old-value release only on moved
/// edges and re-arm it after each assignment; no conditional `RecordInPlace`
/// flag may participate.
const MUTABLE_DIRECT_RECORD_TRANSFER_OVERWRITE_POISON_SOURCE: &str = "\
type Wrap { name: string }\n\
fn main() -> i64 {\n\
\x20   var current = Wrap { name: \"old\".to_upper() };\n\
\x20   var seen: i64 = 0;\n\
\x20   var i: i64 = 0;\n\
\x20   while i < 40 {\n\
\x20       if i % 2 == 0 {\n\
\x20           let moved: Wrap = current;\n\
\x20           seen = seen + moved.name.len();\n\
\x20       }\n\
\x20       current = Wrap { name: \"fresh\".to_upper() };\n\
\x20       seen = seen + current.name.len();\n\
\x20       i = i + 1;\n\
\x20   }\n\
\x20   if seen == 298 { 0 } else { 104 }\n\
}\n";

macro_rules! macos_slope_test {
    ($name:ident, $label:literal, $source:ident) => {
        #[cfg_attr(
            not(target_os = "macos"),
            ignore = "leak oracle needs macOS `leaks(1)`; a host that cannot run it records a SKIP"
        )]
        #[test]
        fn $name() {
            assert_frame_slope_below_tolerance($label, $source);
        }
    };
}

macos_slope_test!(
    ignored_string_handler_param_has_flat_leak_slope,
    "actor_handler_unused_string",
    ignored_string_source
);
macos_slope_test!(
    ignored_recursive_record_param_has_flat_leak_slope,
    "actor_handler_unused_recursive_record",
    ignored_recursive_record_source
);
macos_slope_test!(
    ignored_container_param_has_flat_leak_slope,
    "actor_handler_unused_container",
    ignored_container_source
);
macos_slope_test!(
    branch_and_early_exit_release_param_on_every_edge,
    "actor_handler_unused_branch_early",
    branch_and_early_exit_source
);
macos_slope_test!(
    state_transfer_and_borrow_branch_are_exactly_once,
    "actor_handler_state_or_drop",
    state_or_drop_source
);
macos_slope_test!(
    conditional_local_record_nonconsuming_edge_has_flat_leak_slope,
    "conditional_local_record_handoff",
    conditional_local_record_handoff_source
);
macos_slope_test!(
    loop_carried_record_ingress_retains_once_per_distinct_state_owner,
    "actor_handler_loop_carried_record_ingress",
    loop_carried_record_ingress_source
);
macos_slope_test!(
    nested_loop_carried_record_ingress_retains_once_per_distinct_state_owner,
    "actor_handler_nested_loop_carried_record_ingress",
    nested_loop_carried_record_ingress_source
);
macos_slope_test!(
    branch_selected_record_ingress_retains_only_the_selected_paths,
    "actor_handler_branch_selected_record_ingress",
    branch_selected_record_ingress_source
);
macos_slope_test!(
    branch_selected_nested_record_ingress_retains_before_the_cfg_join,
    "actor_handler_branch_selected_nested_record_ingress",
    branch_selected_nested_record_ingress_source
);
macos_slope_test!(
    cross_field_swapped_record_ingress_balances_preserved_owners,
    "actor_handler_cross_field_swapped_record_ingress",
    cross_field_swapped_record_ingress_source
);
macos_slope_test!(
    shifted_repeated_record_ingress_preserves_exact_owner_multiplicity,
    "actor_handler_shifted_repeated_record_ingress",
    shifted_repeated_record_ingress_source
);
macos_slope_test!(
    branch_to_two_state_fields_mints_one_source_owner,
    "actor_handler_branch_to_two_state_fields",
    branch_to_two_state_fields_source
);

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the poisoned allocator control uses the Darwin malloc diagnostics"
)]
#[test]
fn forwarded_param_survives_without_handler_double_drop() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("actor-handler-forwarded-param-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(FORWARDED_POISON_SOURCE, dir.path(), "forwarded_param");
    let output = run_under_malloc_scribble(&bin);
    assert_eq!(
        output.status.code(),
        Some(0),
        "forwarded mailbox ownership must transfer exactly once; an extra \
         handler drop corrupts the consumer's retained string:\n{}",
        describe_output(&output)
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the poisoned allocator control uses the Darwin malloc diagnostics"
)]
#[test]
fn conditional_record_ingress_retains_before_handler_drop() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("actor-handler-conditional-record-ingress-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        CONDITIONAL_RECORD_INGRESS_POISON_SOURCE,
        dir.path(),
        "conditional_record_ingress",
    );
    let output = run_under_malloc_scribble(&bin);
    assert_eq!(
        output.status.code(),
        Some(0),
        "record ingress on the unconsumed branch must retain before the \
         handler's guarded source drop; otherwise actor state observes a \
         double-free or corrupted string:\n{}",
        describe_output(&output)
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the poisoned allocator control uses the Darwin malloc diagnostics"
)]
#[test]
fn branch_local_fresh_ingress_is_never_hoisted_past_join() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("actor-handler-branch-local-fresh-ingress-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        BRANCH_LOCAL_FRESH_NESTED_POISON_SOURCE,
        dir.path(),
        "branch_local_fresh_ingress",
    );
    let output = run_under_malloc_scribble(&bin);
    assert_eq!(
        output.status.code(),
        Some(0),
        "a branch-local fresh leaf must be retained while path-defined and live; \
         hoisting after the join reads an uninitialised or already-dropped slot:\n{}",
        describe_output(&output)
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the poisoned allocator control uses the Darwin malloc diagnostics"
)]
#[test]
fn conditional_local_record_handoff_releases_exactly_once_on_both_edges() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("conditional-local-record-handoff-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        CONDITIONAL_LOCAL_RECORD_HANDOFF_POISON_SOURCE,
        dir.path(),
        "conditional_local_record_handoff",
    );
    let output = run_under_malloc_scribble(&bin);
    assert_eq!(
        output.status.code(),
        Some(0),
        "the local record must drop on the non-consuming edge and transfer on \
         the state-store edge without double release:\n{}",
        describe_output(&output)
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the poisoned allocator control uses the Darwin malloc diagnostics"
)]
#[test]
fn direct_string_resource_transfer_has_only_affine_close_authority() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("direct-string-resource-transfer-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        DIRECT_STRING_RESOURCE_TRANSFER_POISON_SOURCE,
        dir.path(),
        "direct_string_resource_transfer",
    );
    let output = run_under_malloc_scribble(&bin);
    assert_eq!(
        output.status.code(),
        Some(0),
        "a direct String-record resource transfer must close exactly once \
         through the affine protocol:\n{}",
        describe_output(&output)
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the poisoned allocator control uses the Darwin malloc diagnostics"
)]
#[test]
fn mutable_direct_record_transfer_then_overwrite_uses_only_overwrite_flag() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("mutable-direct-record-transfer-overwrite-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        MUTABLE_DIRECT_RECORD_TRANSFER_OVERWRITE_POISON_SOURCE,
        dir.path(),
        "mutable_direct_record_transfer_overwrite",
    );
    let output = run_under_malloc_scribble(&bin);
    assert_eq!(
        output.status.code(),
        Some(0),
        "alternating direct record transfers and overwrites must release each \
         generation exactly once through #2301:\n{}",
        describe_output(&output)
    );
}

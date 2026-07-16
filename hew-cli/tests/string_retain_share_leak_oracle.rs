//! Leak-slope coverage for string retain-on-share mint points.

#![cfg(unix)]

mod support;

use support::leak_slope::assert_frame_slope_below_tolerance;

fn aggregate_share_source(frames: usize) -> String {
    format!(
        "type Holder {{ payload: string, }}\n\
         fn make_holder() -> Holder {{\n\
         \x20   let source = \"returned-\" + \"holder\";\n\
         \x20   let holder = Holder {{ payload: source }};\n\
         \x20   let _len = source.len();\n\
         \x20   holder\n\
         }}\n\
         fn store_then_return() -> string {{\n\
         \x20   let source = \"stored-\" + \"then-returned\";\n\
         \x20   let holder = Holder {{ payload: source }};\n\
         \x20   let _len = holder.payload.len();\n\
         \x20   source\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let source = \"aggregate-\" + \"share\";\n\
         \x20       let holder = Holder {{ payload: source }};\n\
         \x20       let returned = make_holder();\n\
         \x20       let escaped = store_then_return();\n\
         \x20       total = total + source.len() + holder.payload.len()\n\
         \x20           + returned.payload.len() + escaped.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

fn duplicating_return_source(frames: usize) -> String {
    format!(
        "fn duplicate(value: string) -> (string, string) {{ (value, value) }}\n\
         fn duplicate_local() -> (string, string) {{\n\
         \x20   let value = \"local-\" + \"duplicate\";\n\
         \x20   (value, value)\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let source = \"return-\" + \"share\";\n\
         \x20       let returned = duplicate(source);\n\
         \x20       let (left, right) = returned;\n\
         \x20       let local_returned = duplicate_local();\n\
         \x20       let (local_left, local_right) = local_returned;\n\
         \x20       total = total + source.len() + left.len() + right.len()\n\
         \x20           + local_left.len() + local_right.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

fn local_and_param_share_source(frames: usize) -> String {
    format!(
        "fn share_param(value: string) -> i64 {{\n\
         \x20   let alias = value;\n\
         \x20   value.len() + alias.len()\n\
         }}\n\
         fn make_escape() -> string {{\n\
         \x20   let source = \"owned-\" + \"escape\";\n\
         \x20   let alias = source;\n\
         \x20   let _len = alias.len();\n\
         \x20   source\n\
         }}\n\
         fn handoff() -> i64 {{\n\
         \x20   let source = \"last-use-\" + \"handoff\";\n\
         \x20   let alias = source;\n\
         \x20   alias.len()\n\
         }}\n\
         fn sequential_shares() -> i64 {{\n\
         \x20   let source = \"sequential-\" + \"shares\";\n\
         \x20   let first = source;\n\
         \x20   let second = source;\n\
         \x20   first.len() + second.len()\n\
         }}\n\
         fn chained_handoff() -> i64 {{\n\
         \x20   let source = \"chained-\" + \"handoff\";\n\
         \x20   let first = source;\n\
         \x20   let second = first;\n\
         \x20   source.len() + second.len()\n\
         }}\n\
         fn conditional_handoff(flag: bool) -> i64 {{\n\
         \x20   let source = \"conditional-\" + \"handoff\";\n\
         \x20   if flag {{ let alias = source; alias.len() }} else {{ 0 }}\n\
         }}\n\
         fn hew_borrow(value: string) -> i64 {{ value.len() }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let source = \"local-\" + \"share\";\n\
         \x20       let alias = source;\n\
         \x20       let borrowed = \"param-\" + \"share\";\n\
         \x20       let prefixed = \"prefixed-\" + \"user-fn\";\n\
         \x20       let escaped = make_escape();\n\
         \x20       total = total + source.len() + alias.len()\n\
         \x20           + share_param(borrowed) + escaped.len() + handoff()\n\
         \x20           + sequential_shares() + chained_handoff()\n\
         \x20           + conditional_handoff(true)\n\
         \x20           + conditional_handoff(false) + hew_borrow(prefixed);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

#[test]
fn string_aggregate_share_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("string_aggregate_share", aggregate_share_source);
}

#[test]
fn string_duplicating_return_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("string_duplicating_return", duplicating_return_source);
}

#[test]
fn string_local_and_param_share_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "string_local_and_param_share",
        local_and_param_share_source,
    );
}

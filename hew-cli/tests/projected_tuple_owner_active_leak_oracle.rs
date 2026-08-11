//! Ownership oracle for moving a heap leaf out of a still-live tuple owner.
//!
//! The authoritative shape is `let p = (v, 1); let items = p.0;
//! items.len()`. A tuple-field load byte-copies the `Vec` handle. The `let`
//! makes `items` the new owner, so lowering must clear `p.0` before either
//! scope-exit release can run: `items` frees the transferred handle, while
//! `p` keeps structural responsibility for every unmoved sibling.
//!
//! Before this contract was written into MIR, both candidates were excluded
//! fail-closed: the tuple prover saw an active extracted owner, while the Vec
//! prover saw an interior projection. A 64-frame probe leaked exactly 128
//! nodes (the Vec header and backing allocation per frame); the direct Vec
//! control leaked zero.
//!
//! The structural assertions are deliberately independent teeth:
//! the checked MIR requires the root-relative neutralize and its transferee,
//! elaborated MIR requires both disjoint drops on normal and cancellation
//! exits, and LLVM requires the null store to precede both releases.

mod support;

use std::process::Command;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, hew_binary, repo_root, require_codegen};

fn projected_tuple_source(frames: usize) -> String {
    format!(
        "\
fn build(n: i64) -> i64 {{
    let v: Vec<i64> = Vec::new();
    v.push(n);
    v.push(n + 1);
    let p = (v, 1);
    let items = p.0;
    items.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{frames} {{
        total = total + build(i);
    }}
    if total == {expected} {{ 0 }} else {{ 113 }}
}}
",
        expected = frames * 2
    )
}

fn aliased_tuple_source(frames: usize) -> String {
    format!(
        "\
fn build(n: i64) -> i64 {{
    let v: Vec<i64> = Vec::new();
    v.push(n);
    v.push(n + 1);
    let p = (v, 1);
    let q = p;
    let items = q.0;
    items.len() + q.1
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{frames} {{
        total = total + build(i);
    }}
    if total == {expected} {{ 0 }} else {{ 114 }}
}}
",
        expected = frames * 3
    )
}

fn cancellation_source(frames: usize) -> String {
    format!(
        "\
fn build(n: i64) -> i64 {{
    let v: Vec<i64> = Vec::new();
    v.push(n);
    let p = (v, 7);
    let items = p.0;
    var i: i64 = 0;
    while i < n {{
        i = i + 1;
    }}
    items.len() + p.1
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{frames} {{
        total = total + build(i);
    }}
    if total == {expected} {{ 0 }} else {{ 115 }}
}}
",
        expected = frames * 8
    )
}

fn escaping_partial_tuple_source() -> &'static str {
    "\
fn build(n: i64) -> (Vec<i64>, i64) {
    let v: Vec<i64> = Vec::new();
    v.push(n);
    let p = (v, 1);
    let items = p.0;
    let _length = items.len();
    p
}

fn main() -> i64 {
    let returned = build(3);
    returned.0.len()
}
"
}

fn loop_reread_after_transfer_source() -> &'static str {
    "\
fn main() -> i64 {
    let v: Vec<i64> = Vec::new();
    v.push(1);
    let p = (v, 7);
    var i: i64 = 0;
    while i < 2 {
        let before = p.0.len();
        let items = p.0;
        i = i + before + items.len();
    }
    i
}
"
}

fn dump_mir(source: &str, name: &str, stage: &str) -> String {
    let dir = tempfile::Builder::new()
        .prefix("projected-tuple-owner-mir-")
        .tempdir()
        .expect("tempdir");
    let source_path = dir.path().join(format!("{name}.hew"));
    std::fs::write(&source_path, source).expect("write Hew source");
    let output = Command::new(hew_binary())
        .args(["compile", "--dump-mir", stage])
        .arg(&source_path)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile --dump-mir");
    assert!(
        output.status.success(),
        "{stage} MIR dump failed:\n{}",
        describe_output(&output)
    );
    String::from_utf8(output.stdout).expect("MIR dump is UTF-8")
}

fn build_section(mir: &str) -> &str {
    mir.split("fn build ->")
        .nth(1)
        .and_then(|section| section.split("fn main ->").next())
        .expect("build MIR section")
}

fn compile_to_llvm(source: &str, name: &str) -> String {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("projected-tuple-owner-llvm-")
        .tempdir()
        .expect("tempdir");
    let source_path = dir.path().join(format!("{name}.hew"));
    let emit_dir = dir.path().join("emit");
    std::fs::create_dir(&emit_dir).expect("create emit dir");
    std::fs::write(&source_path, source).expect("write Hew source");
    let output = Command::new(hew_binary())
        .args(["compile", "--emit-dir"])
        .arg(&emit_dir)
        .arg(&source_path)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");
    assert!(
        output.status.success(),
        "LLVM emission failed:\n{}",
        describe_output(&output)
    );
    std::fs::read_to_string(emit_dir.join(format!("{name}.ll"))).expect("read emitted LLVM IR")
}

fn llvm_function_body<'a>(ir: &'a str, symbol: &str) -> &'a str {
    let start = ir
        .find(&format!("@{symbol}("))
        .unwrap_or_else(|| panic!("missing @{symbol} in LLVM IR"));
    let body = &ir[start..];
    let end = body
        .find("\n}")
        .map_or(body.len(), |closing_brace| closing_brace + 2);
    &body[..end]
}

fn check_source(source: &str, name: &str) -> std::process::Output {
    let dir = tempfile::Builder::new()
        .prefix("projected-tuple-owner-check-")
        .tempdir()
        .expect("tempdir");
    let source_path = dir.path().join(format!("{name}.hew"));
    std::fs::write(&source_path, source).expect("write Hew source");
    Command::new(hew_binary())
        .arg("check")
        .arg(&source_path)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check")
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn projected_tuple_owner_active_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("projected_tuple_owner_active", projected_tuple_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn whole_tuple_alias_then_projection_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("projected_tuple_owner_alias", aliased_tuple_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "poisoned allocator validation is a Darwin-only ownership gate"
)]
#[test]
fn projected_owner_paths_are_exactly_once_under_malloc_scribble() {
    require_codegen();
    for (name, source) in [
        ("projected_tuple_owner_active", projected_tuple_source(8)),
        ("projected_tuple_owner_alias", aliased_tuple_source(8)),
        ("projected_tuple_owner_cancel", cancellation_source(8)),
    ] {
        let dir = tempfile::Builder::new()
            .prefix("projected-tuple-owner-scribble-")
            .tempdir()
            .expect("tempdir");
        let bin = compile_to_native(&source, dir.path(), name);
        let output = run_under_malloc_scribble(&bin);
        assert!(
            output.status.success(),
            "{name} must preserve the transferred Vec through every read and \
             release it exactly once:\n{}",
            describe_output(&output)
        );
    }
}

#[test]
fn checked_and_elaborated_mir_write_the_disjoint_release_contract() {
    let source = projected_tuple_source(1);
    let checked_dump = dump_mir(&source, "projected_tuple_owner_checked", "checked");
    let checked = build_section(&checked_dump);
    assert_eq!(
        checked.matches("aggregate_projection_neutralize").count(),
        1,
        "the field transfer needs exactly one root-relative neutralization:\n{checked}"
    );
    // Derive the root and transferee locals from the neutralize statement
    // itself rather than pinning local numbers, which renumber on unrelated
    // codegen changes. The semantic teeth stay: the authority must name the
    // original tuple root, exact field 0, and the loaded transferee, and the
    // load/neutralize/bind edges must appear in that order.
    let neutralize_stmt = checked
        .lines()
        .find(|line| line.contains("aggregate_projection_neutralize"))
        .expect("projection neutralize")
        .trim();
    let mut parts = neutralize_stmt.split_whitespace();
    assert_eq!(parts.next(), Some("aggregate_projection_neutralize"));
    let root = parts.next().expect("neutralize names the tuple root");
    assert_eq!(
        parts.next(),
        Some("fields=[0]"),
        "the authority must name the exact transferred field:\n{checked}"
    );
    assert_eq!(parts.next(), Some("->"));
    let transferee = parts
        .next()
        .expect("neutralize names the loaded transferee");
    let load = checked
        .find(&format!("{transferee} = {root}.0"))
        .expect("tuple field load");
    let neutralize = checked
        .find("aggregate_projection_neutralize")
        .expect("projection neutralize");
    let bind = checked
        .lines()
        .scan(0, |offset, line| {
            let line_start = *offset;
            *offset += line.len() + 1;
            Some((line_start, line))
        })
        .find(|(_, line)| line.trim_end().ends_with(&format!("= move {transferee}")))
        .map(|(offset, _)| offset)
        .expect("items binding move");
    assert!(
        load < neutralize && neutralize < bind,
        "the original slot must be cleared after its handle is loaded and \
         before the new owner is exposed:\n{checked}"
    );

    let elaborated_dump = dump_mir(&source, "projected_tuple_owner_elab", "elab");
    let elaborated = build_section(&elaborated_dump);
    let return_plan = elaborated
        .split("return[")
        .nth(1)
        .expect("build return drop plan");
    assert_eq!(
        return_plan.matches("kind=cow_heap(hew_vec_free)").count(),
        1,
        "the projected Vec binding must own one release:\n{return_plan}"
    );
    assert_eq!(
        return_plan.matches("kind=tuple_in_place").count(),
        1,
        "the partially neutralized tuple must keep one structural sibling drop:\n{return_plan}"
    );
}

#[test]
fn cancellation_exit_keeps_both_disjoint_drops() {
    let dump = dump_mir(
        &cancellation_source(1),
        "projected_tuple_owner_cancel",
        "elab",
    );
    let build = build_section(&dump);
    let cancellation_has_both = build.split("cancel[").skip(1).any(|section| {
        let plan = section
            .lines()
            .skip(1)
            .take_while(|line| line.starts_with("      "))
            .collect::<Vec<_>>()
            .join("\n");
        plan.contains("kind=cow_heap(hew_vec_free)") && plan.contains("kind=tuple_in_place")
    });
    assert!(
        cancellation_has_both,
        "a loop-backedge cancellation after the projection transfer must drop \
         the Vec owner and the neutralized tuple exactly once:\n{build}"
    );
}

#[test]
fn llvm_clears_the_tuple_slot_before_either_release() {
    let ir = compile_to_llvm(&projected_tuple_source(1), "projected_tuple_owner_llvm");
    let build = llvm_function_body(&ir, "build");
    assert_eq!(
        build.matches("carrier_path_d0_f0_ptr").count(),
        2,
        "one GEP definition and one null-store use must name the transferred slot:\n{build}"
    );
    let neutralize = build
        .find("store ptr null, ptr %carrier_path_d0_f0_ptr")
        .expect("root field null store");
    let vec_drop = build
        .find("call void @hew_vec_free(")
        .expect("projected Vec release");
    let tuple_drop = build
        .find("call void @\"__hew_tuple_drop_inplace_")
        .expect("tuple structural release");
    assert!(
        neutralize < vec_drop && vec_drop < tuple_drop,
        "the source slot must be null before the projected owner and tuple \
         structural releases run:\n{build}"
    );
}

#[test]
fn whole_tuple_escape_after_projection_transfer_fails_closed() {
    let output = check_source(escaping_partial_tuple_source(), "escape");
    assert!(
        !output.status.success(),
        "a whole tuple whose field slot was cleared must not escape as a \
         null-bearing value:\n{}",
        describe_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("used after it was consumed")
            && stderr.contains("only unmoved sibling projections remain readable"),
        "the refusal must explain the partial-transfer boundary:\n{stderr}"
    );
}

#[test]
fn loop_backedge_cannot_reread_the_cleared_field() {
    let output = check_source(loop_reread_after_transfer_source(), "loop_reread");
    assert!(
        !output.status.success(),
        "a loop backedge must carry the partial-move state to the next \
         iteration:\n{}",
        describe_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("used after it was consumed")
            && stderr.contains("only unmoved sibling projections remain readable"),
        "the backedge refusal must name the partial-transfer boundary:\n{stderr}"
    );
}

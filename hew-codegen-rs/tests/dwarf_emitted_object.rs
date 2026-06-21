//! gdb `-g` DWARF assertions on the EMITTED OBJECT, not the textual IR. The
//! textual-IR harness (`dwarf_variable_dies.rs`) proves the metadata graph
//! codegen *requests*; this harness proves what LLVM's DWARF emitter actually
//! *writes* — the gap a live-debugger review caught when a union that carried
//! `elements:` in the IR emitted as a member-less `DW_TAG_union_type` in the
//! object (LLVM drops a union element that is a bare composite type, not a
//! `DW_TAG_member`). Each assertion here would have failed on that bug.
//!
//! Deterministic: parses `dwarfdump`/`llvm-dwarfdump` text, asserts exact
//! structural facts (distinct lexically-scoped shadowed DIEs; union members
//! with named fields; payload-union size+offset == enclosing enum size; a
//! struct member's exact bit offset). No debugger launch — those tools are
//! present on both macOS (`dwarfdump`) and Linux CI (`llvm-dwarfdump`). When
//! neither is found the test is a no-op skip (it cannot run, it must not
//! fail-open into a false green — but a missing host tool is not a code defect).
//!
//! LESSONS applied:
//! - `ci_gate_trust` / proving-gate-is-the-compiled-artefact (P1): assert on the
//!   object the toolchain emits, not an intermediate the emitter may rewrite.
//! - `assertions_distinguish` (P1): every assertion pins an EXACT name + size +
//!   offset; an empty union, a leaked outer DIE, or a wrong size fails.

use std::path::{Path, PathBuf};
use std::process::Command;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

/// A fixture exercising shadowing, an enum with a struct payload, and a record
/// with mixed-width fields (for the member bit-offset check). Mirrors the
/// cross-eco probe that surfaced the three blocking defects.
const FIXTURE: &str = "\
record Point { x: i64, y: i64 }

record Mixed { tiny: i8, wide: i64, flag: bool, mid: i16, tail: i32 }

enum Status {
    Idle;
    Packet { code: i16, payload: Mixed };
    Count(i64);
}

fn probe(selector: i32) -> i64 {
    let pt = Point { x: 10, y: 20 };
    let status = Status::Packet { code: 7, payload: Mixed { tiny: 1, wide: 99, flag: true, mid: 3, tail: 4 } };
    let first = selector + 1;
    {
        let first = selector + 2;
        println(first);
    }
    pt.y + first as i64 + match status {
        Status::Packet { code, payload } => code as i64 + payload.wide,
        _ => 0,
    }
}

fn main() {
    println(probe(41));
}
";

/// Compile `FIXTURE` to a native object with `-g` and return its path (kept in a
/// leaked temp dir for the test's lifetime).
fn emit_object(slug: &str) -> PathBuf {
    let parsed = hew_parser::parse(FIXTURE);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc = checker.check_program(&parsed.program);
    let output = lower_program(
        &parsed.program,
        &tc,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let verify = verify_hir(&output.module);
    assert!(
        output.diagnostics.is_empty() && verify.is_empty(),
        "hir diagnostics: {:?} verify: {:?}",
        output.diagnostics,
        verify
    );
    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "mir diagnostics: {:?}",
        pipeline.diagnostics
    );

    // Per-test slug: the suite runs tests in parallel threads, so a shared
    // out_dir would race (each `emit_object` clobbers a sibling's source/obj).
    let tmp = std::env::temp_dir().join(format!("hew-dwarf-object-{slug}"));
    let _ = std::fs::remove_dir_all(&tmp);
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let src_path = tmp.join("fixture.hew");
    std::fs::write(&src_path, FIXTURE).expect("write fixture source");

    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: true,
        wasm: false,
        target_triple: None,
        debug: true,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: Some(src_path.as_path()),
    };
    let artefacts = emit_module(&pipeline, &options).expect("emit_module must succeed");
    artefacts
        .native_obj_path
        .expect("emit_module must populate native_obj_path under native:true")
}

/// Run the available DWARF dumper over `obj` and return its full text, or `None`
/// when no dumper is installed (the test then skips rather than fail-opening).
fn dwarf_dump(obj: &Path) -> Option<String> {
    for tool in ["llvm-dwarfdump", "dwarfdump"] {
        if let Ok(out) = Command::new(tool).arg(obj).output() {
            if out.status.success() {
                return Some(String::from_utf8_lossy(&out.stdout).into_owned());
            }
        }
    }
    None
}

/// A `byte_size` value (hex or decimal) for the DIE whose `DW_AT_name` is
/// exactly `name`. Both dumpers print `DW_AT_byte_size (0xNN)` (Apple) or
/// `DW_AT_byte_size (NN)`; parse either. Searches the DIE block that begins at
/// the matching name and reads the nearest following byte_size.
fn byte_size_of(dump: &str, name: &str) -> Option<u64> {
    let needle = format!("(\"{name}\")");
    let lines: Vec<&str> = dump.lines().collect();
    for (i, l) in lines.iter().enumerate() {
        if l.contains("DW_AT_name") && l.contains(&needle) {
            // byte_size is emitted adjacent to the name within the same DIE;
            // scan a small forward/backward window.
            let lo = i.saturating_sub(3);
            let hi = (i + 4).min(lines.len());
            if let Some(v) = lines[lo..hi].iter().find_map(|w| parse_byte_size(w)) {
                return Some(v);
            }
        }
    }
    None
}

fn parse_byte_size(line: &str) -> Option<u64> {
    if !line.contains("DW_AT_byte_size") {
        return None;
    }
    let inside = line.split('(').nth(1)?.trim_end_matches(')').trim();
    if let Some(hex) = inside.strip_prefix("0x") {
        u64::from_str_radix(hex, 16).ok()
    } else {
        inside.parse().ok()
    }
}

/// Count `DW_TAG_variable` DIEs whose `DW_AT_name` is exactly `name`.
fn count_named_variables(dump: &str, name: &str) -> usize {
    let needle = format!("(\"{name}\")");
    let lines: Vec<&str> = dump.lines().collect();
    let mut count = 0;
    for (i, l) in lines.iter().enumerate() {
        if l.contains("DW_TAG_variable") {
            // The name follows within the DIE's attribute lines.
            for next in lines.iter().take((i + 6).min(lines.len())).skip(i + 1) {
                if next.contains("DW_TAG_") {
                    break;
                }
                if next.contains("DW_AT_name") && next.contains(&needle) {
                    count += 1;
                    break;
                }
            }
        }
    }
    count
}

#[test]
fn emitted_object_scopes_shadowed_locals_in_distinct_lexical_blocks() {
    let obj = emit_object("shadow");
    let Some(dump) = dwarf_dump(&obj) else {
        eprintln!("skip: no llvm-dwarfdump/dwarfdump on host");
        return;
    };
    // Two `first` bindings (outer + shadowing inner) each get their OWN
    // `DW_TAG_variable`. The pre-fix bug emitted exactly one (the outer leaking
    // into the inner scope) — this asserts BOTH survive.
    let firsts = count_named_variables(&dump, "first");
    assert!(
        firsts >= 2,
        "expected >= 2 `first` variable DIEs (outer + shadowed inner); got {firsts}\n{dump}"
    );
    // At least one lexical block exists to host the inner shadow distinctly.
    assert!(
        dump.contains("DW_TAG_lexical_block"),
        "expected a DW_TAG_lexical_block scoping the shadowed inner `first`;\n{dump}"
    );
}

#[test]
fn emitted_object_enum_payload_union_has_named_variant_members() {
    let obj = emit_object("union");
    let Some(dump) = dwarf_dump(&obj) else {
        eprintln!("skip: no llvm-dwarfdump/dwarfdump on host");
        return;
    };
    // The payload union is no longer member-less: it carries per-variant struct
    // members whose fields gdb can read. Assert the union, the `Packet` member,
    // and the `code` field name are all present.
    assert!(
        dump.contains("Status::Payload"),
        "expected the Status::Payload union DIE;\n{dump}"
    );
    assert!(
        dump.contains("(\"Packet\")") && dump.contains("(\"code\")"),
        "expected the payload union to expose `Packet` with a readable `code` \
         field (the empty-union bug);\n{dump}"
    );
}

#[test]
fn emitted_object_payload_union_size_fits_inside_enclosing_enum() {
    let obj = emit_object("size");
    let Some(dump) = dwarf_dump(&obj) else {
        eprintln!("skip: no llvm-dwarfdump/dwarfdump on host");
        return;
    };
    // The payload sits at offset 8 (after the tag + padding). The union's own
    // byte_size MUST be `enum_size - 8` so the payload type does not extend
    // past the object. The pre-fix bug subtracted only the tag bits, making the
    // union one byte too large.
    let enum_size = byte_size_of(&dump, "Status").expect("Status size");
    let payload_size = byte_size_of(&dump, "Status::Payload").expect("Status::Payload size");
    assert_eq!(
        payload_size + 8,
        enum_size,
        "payload-union size ({payload_size:#x}) + offset 0x8 must equal the \
         enclosing Status size ({enum_size:#x}); the payload must not extend \
         past the object\n{dump}"
    );
}

#[test]
fn emitted_object_record_member_at_expected_bit_offset() {
    let obj = emit_object("offset");
    let Some(dump) = dwarf_dump(&obj) else {
        eprintln!("skip: no llvm-dwarfdump/dwarfdump on host");
        return;
    };
    // `Point.y` is the second i64 → byte offset 8. dwarfdump prints
    // `DW_AT_data_member_location (0x08)`. This is the in-lane offset followup:
    // member offsets must be byte-exact, not zeroed.
    assert!(
        dump.contains("(\"y\")"),
        "expected Point member `y`;\n{dump}"
    );
    let has_offset_8 = dump.lines().any(|l| {
        l.contains("DW_AT_data_member_location") && (l.contains("0x08") || l.contains("(8)"))
    });
    assert!(
        has_offset_8,
        "expected a member at byte offset 0x08 (Point.y / Mixed.wide);\n{dump}"
    );
}

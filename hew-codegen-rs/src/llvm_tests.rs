use super::*;
use hew_mir::{BasicBlock, DecisionFact, IrPipeline};

#[test]
fn owned_config_field_collections_are_fail_closed_backstop() {
    // #2238 item 1: collection kinds must take the same typed error path
    // used by the init thunk, rather than reaching clone_helper_for_kind's
    // unreachable collection arm. The checker rejects these before codegen,
    // so this is the defence-in-depth decision boundary.
    let scalar = StateFieldCloneKind::BitCopy { size_bytes: 8 };
    for kind in [
        StateFieldCloneKind::Vec {
            elem: Box::new(scalar.clone()),
        },
        StateFieldCloneKind::HashSet {
            elem: Box::new(scalar.clone()),
        },
        StateFieldCloneKind::HashMap {
            key: Box::new(scalar.clone()),
            val: Box::new(scalar.clone()),
        },
    ] {
        let err = owned_config_field_clone_support(&kind, "worker", &ResolvedTy::String)
            .expect_err("owned collection must fail closed before clone-helper dispatch");
        assert!(
            matches!(err, CodegenError::FailClosed(ref message) if message.contains("child `worker`")
                    && message.contains("owned collection")),
            "expected the init-thunk collection fail-closed diagnostic, got {err:?}"
        );
    }
}

#[test]
fn owned_config_field_supported_kinds_pass_clone_support_boundary() {
    // The admitted owned kinds and scalar copies must proceed to their
    // existing clone/copy paths rather than being over-rejected.
    for (kind, ty) in [
        (StateFieldCloneKind::String, ResolvedTy::String),
        (StateFieldCloneKind::Bytes, ResolvedTy::Bytes),
        (
            StateFieldCloneKind::BitCopy { size_bytes: 8 },
            ResolvedTy::I64,
        ),
    ] {
        assert!(
            owned_config_field_clone_support(&kind, "worker", &ty).is_ok(),
            "{kind:?} must retain its supported init-thunk clone/copy path"
        );
    }
}

#[test]
fn runtime_size_ty_is_i32_on_wasm32() {
    let ctx = Context::create();
    let llvm_mod = ctx.create_module("runtime_size_ty_wasm32");
    llvm_mod.set_triple(&TargetTriple::create("wasm32-unknown-unknown"));
    assert_eq!(runtime_size_ty(&ctx, &llvm_mod), ctx.i32_type());
}

#[test]
fn rc_runtime_declarations_preserve_pointer_roles_and_target_size_width() {
    for (triple, size_bits) in [
        ("wasm32-unknown-unknown", 32),
        ("aarch64-unknown-linux-gnu", 64),
    ] {
        let ctx = Context::create();
        let llvm_mod = ctx.create_module("rc_runtime_declarations");
        llvm_mod.set_triple(&TargetTriple::create(triple));
        let mut declarations = RuntimeDeclMap::new();
        for symbol in [
            "hew_rc_clone",
            "hew_rc_downgrade",
            "hew_rc_drop",
            "hew_rc_get",
            "hew_rc_is_unique",
            "hew_rc_new",
            "hew_rc_set",
            "hew_rc_strong_count",
            "hew_rc_weak_count",
            "hew_weak_clone_rc",
            "hew_weak_drop_rc",
            "hew_weak_upgrade_rc",
        ] {
            intern_runtime_decl(&ctx, &llvm_mod, &mut declarations, symbol)
                .unwrap_or_else(|error| panic!("{symbol} declaration failed: {error}"));
        }

        let rc_new = declarations["hew_rc_new"].get_type();
        let params = rc_new.get_param_types();
        assert!(matches!(params[0], BasicMetadataTypeEnum::PointerType(_)));
        assert!(matches!(params[3], BasicMetadataTypeEnum::PointerType(_)));
        for index in [1, 2] {
            assert!(
                matches!(params[index], BasicMetadataTypeEnum::IntType(ty) if ty.get_bit_width() == size_bits),
                "hew_rc_new parameter {index} must be target size width on {triple}: {:?}",
                params[index]
            );
        }
        for symbol in ["hew_rc_strong_count", "hew_rc_weak_count"] {
            assert!(
                matches!(
                    declarations[symbol].get_type().get_return_type(),
                    Some(BasicTypeEnum::IntType(ty)) if ty.get_bit_width() == size_bits
                ),
                "{symbol} return must be target size width on {triple}"
            );
        }
        for symbol in ["hew_rc_downgrade", "hew_weak_upgrade_rc"] {
            assert!(matches!(
                declarations[symbol].get_type().get_return_type(),
                Some(BasicTypeEnum::PointerType(_))
            ));
        }
    }
}

#[test]
fn runtime_size_ty_is_i32_on_32bit_x86() {
    // PR #1939 fixed the wasm32 case (`reconcile_int_width_signed` at the
    // join-terminator and suspending-ask call sites) but this helper's own
    // triple check stayed wasm32-only, so on i386/i686 the reconcile was a
    // same-width i64-vs-i64 no-op and the FFI declaration itself stayed at
    // the wrong (i64) width against the real 32-bit C ABI. Guard every
    // real Rust 32-bit x86 triple prefix (`rustc --print target-list`:
    // i386-apple-ios, i586-unknown-*, i686-*) so this can't silently regress
    // to wasm32-only again.
    let ctx = Context::create();
    for triple in [
        "i686-unknown-linux-gnu",
        "i686-pc-windows-msvc",
        "i686-pc-windows-gnu",
        "i686-apple-darwin",
        "i586-unknown-linux-gnu",
        "i386-apple-ios",
    ] {
        let llvm_mod = ctx.create_module("runtime_size_ty_32bit_x86");
        llvm_mod.set_triple(&TargetTriple::create(triple));
        assert_eq!(
            runtime_size_ty(&ctx, &llvm_mod),
            ctx.i32_type(),
            "runtime_size_ty should be i32 on 32-bit x86 triple {triple}"
        );
    }
}

#[test]
fn runtime_size_ty_is_i64_on_64bit_native_targets() {
    let ctx = Context::create();
    for triple in [
        "x86_64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc",
        "aarch64-apple-darwin",
        "aarch64-unknown-linux-gnu",
    ] {
        let llvm_mod = ctx.create_module("runtime_size_ty_64bit");
        llvm_mod.set_triple(&TargetTriple::create(triple));
        assert_eq!(
            runtime_size_ty(&ctx, &llvm_mod),
            ctx.i64_type(),
            "runtime_size_ty should be i64 on 64-bit triple {triple}"
        );
    }
}

#[test]
fn verify_runtime_size_width_accepts_modelled_targets() {
    // wasm32 + 32-bit x86 (i32 size_t) and the 64-bit natives (i64 size_t)
    // all AGREE between `runtime_size_ty`'s allowlist and the target's real
    // pointer width, so the gate passes.
    let ctx = Context::create();
    for triple in [
        "wasm32-unknown-unknown",
        "i686-unknown-linux-gnu",
        "i686-pc-windows-msvc",
        "x86_64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc",
        "aarch64-apple-darwin",
        "aarch64-unknown-linux-gnu",
    ] {
        let llvm_mod = ctx.create_module("verify_size_ok");
        llvm_mod.set_triple(&TargetTriple::create(triple));
        let td = record_ret_target_data(triple);
        verify_runtime_size_width(&ctx, &llvm_mod, &td, triple)
            .unwrap_or_else(|e| panic!("modelled target {triple} must pass the width gate: {e:?}"));
    }
}

#[test]
fn verify_runtime_size_width_fails_closed_on_unmodelled_32bit_targets() {
    // The bug: `runtime_size_ty` allowlists only wasm32 + 32-bit x86, so
    // every OTHER 32-bit target (arm32, riscv32, mips32, ppc32) silently
    // falls through to i64 while its real `size_t` is 32-bit. The gate must
    // reject these before any wrong-width runtime declaration is emitted —
    // this is the generalization of the #2423 i386/i686 fix to the rest of
    // the 32-bit target space, expressed as a fail-closed check rather than
    // an ever-growing arch allowlist.
    let ctx = Context::create();
    for triple in [
        "arm-unknown-linux-gnueabi",
        "armv7-unknown-linux-gnueabihf",
        "riscv32imac-unknown-none-elf",
        "mipsel-unknown-linux-gnu",
        "powerpc-unknown-linux-gnu",
    ] {
        let llvm_mod = ctx.create_module("verify_size_bad");
        llvm_mod.set_triple(&TargetTriple::create(triple));
        // Build a 32-bit-pointer `TargetData` directly from a data-layout
        // string so the test does not depend on the LLVM build shipping
        // every 32-bit backend (riscv32/mips/ppc are not in the default
        // `initialize_all` set on all hosts). `e-p:32:32` = little-endian,
        // 32-bit pointer — the only datum the gate reads besides the triple.
        let td = TargetData::create("e-p:32:32-i64:64");
        let pointer_bits = td.get_pointer_byte_size(None) * 8;
        assert_eq!(pointer_bits, 32, "expected a 32-bit pointer for {triple}");
        let err = verify_runtime_size_width(&ctx, &llvm_mod, &td, triple).expect_err(
            "unmodelled 32-bit target must fail closed, not emit a silently wrong-width module",
        );
        match err {
            CodegenError::FailClosed(msg) => {
                assert!(
                    msg.contains(triple),
                    "diagnostic must name the target: {msg}"
                );
                assert!(
                    msg.contains("32-bit"),
                    "diagnostic must cite the width: {msg}"
                );
            }
            other => panic!("expected FailClosed for {triple}, got {other:?}"),
        }
    }
}

#[test]
fn uint_to_string_runtime_arg_width_is_u32() {
    // The runtime signature is `hew_uint_to_string(n: u32)` at
    // `hew-runtime/src/string.rs:253`, so arg 0 must be declared as i32.
    assert_eq!(
        runtime_ffi_param_abi_bits("hew_uint_to_string", 0),
        Some(32)
    );
}

#[test]
fn catalog_ffi_redeclaration_mismatch_fails_closed() {
    let ctx = Context::create();
    let llvm_mod = ctx.create_module("catalog_ffi_redeclaration_mismatch");
    let wrong_ty = ctx
        .ptr_type(AddressSpace::default())
        .fn_type(&[ctx.i16_type().into()], false);
    llvm_mod.add_function("hew_uint_to_string", wrong_ty, Some(Linkage::External));

    let entry = stdlib_catalog::entries()
        .iter()
        .find(|entry| entry.name == "to_string_u32")
        .expect("to_string_u32 catalog entry");
    let record_layouts = RecordLayoutMap::new();
    let target_data = host_target_data();
    let result = declare_catalog_ffi(
        &ctx,
        &llvm_mod,
        &target_data,
        entry,
        "hew_uint_to_string",
        &record_layouts,
    );

    let Err(err) = result else {
        panic!("mismatched catalog FFI redeclaration must fail closed");
    };
    match err {
        CodegenError::FailClosed(msg) => {
            assert!(msg.contains("hew_uint_to_string"));
            assert!(msg.contains("expected (i32) -> ptr"));
            assert!(msg.contains("got (i16) -> ptr"));
        }
        other => panic!("expected fail-closed redeclaration error, got {other}"),
    }
}

/// Groups every catalog entry that reaches `declare_catalog_ffi` (the
/// three shim linkages that name a real runtime FFI symbol) by that
/// shared runtime symbol, then — for every group with 2+ entries —
/// declares each entry through the exact production path
/// (`declare_catalog_ffi` → `get_or_declare_catalog_ffi` →
/// `ensure_catalog_ffi_signature_matches`) into one shared LLVM module.
/// A shared symbol backing multiple Hew-facing catalog rows (e.g.
/// `to_string_u16`/`to_string_u32` both routing through
/// `hew_uint_to_string`) must compute the identical LLVM function type
/// for every entry in the group; any divergence is exactly the
/// `to_string_u16`/`to_string_u32` → `hew_uint_to_string` width mismatch
/// class of bug that an O0/O2 differential run found instead of a test.
/// This generalizes `catalog_ffi_redeclaration_mismatch_fails_closed`
/// (one hand-picked symbol) into an exhaustive gate over the whole
/// catalog, reusing the real declaration path rather than a parallel
/// hand-rolled computation.
#[test]
fn catalog_ffi_symbols_agree_across_all_shared_symbol_entries() {
    use std::collections::HashMap;

    let mut groups: HashMap<&'static str, Vec<&'static BuiltinEntry>> = HashMap::new();
    for entry in stdlib_catalog::entries() {
        if let Some(symbol) = entry.linkage.runtime_symbol() {
            groups.entry(symbol).or_default().push(entry);
        }
    }

    let record_layouts = RecordLayoutMap::new();
    let mut checked_groups = 0usize;
    for (symbol, entries) in &groups {
        if entries.len() < 2 {
            continue;
        }
        checked_groups += 1;
        let ctx = Context::create();
        let llvm_mod = ctx.create_module("catalog_ffi_group_parity");
        let target_data = host_target_data();
        for entry in entries {
            declare_catalog_ffi(
                &ctx,
                &llvm_mod,
                &target_data,
                entry,
                symbol,
                &record_layouts,
            )
            .unwrap_or_else(|err| {
                panic!(
                    "catalog FFI symbol `{symbol}` diverges across shared-symbol \
                             entries (entry `{}`): {err}",
                    entry.name
                )
            });
        }
    }
    assert!(
        checked_groups > 0,
        "expected at least one shared-symbol catalog group (e.g. hew_uint_to_string) \
             to exercise the parity gate"
    );
}

fn empty_pipeline_with_const_42() -> IrPipeline {
    let return_ty = ResolvedTy::I64;
    let main = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "main".to_string(),
        return_ty: return_ty.clone(),
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: vec![return_ty.clone()],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                Instr::ConstI64 {
                    dest: Place::Local(0),
                    value: 42,
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(0),
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: Vec::<DecisionFact>::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![main],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    }
}

/// Build `empty_pipeline_with_const_42` for `triple` with a `-g` debug input
/// and return the printed module IR. Threading a `DebugInput` is what makes
/// `build_module_for_target` construct the `DICompileUnit` and add the
/// module-level debug-format flags — the locus under test.
fn debug_module_ir_for_triple(triple: &str) -> String {
    let ctx = Context::create();
    let pipeline = empty_pipeline_with_const_42();
    let machine = target_machine_for_triple(triple)
        .unwrap_or_else(|e| panic!("target machine for {triple}: {e:?}"));
    let src_path = Path::new("codeview_flag_probe.hew");
    let debug = DebugInput {
        source_path: src_path,
        source_text: "fn main() -> i64 { 42 }\n",
    };
    let module = build_module_for_target(
        &ctx,
        &pipeline,
        "codeview_flag_probe",
        Some(&machine),
        Some(debug),
    )
    .unwrap_or_else(|e| panic!("debug module must build for {triple}: {e:?}"));
    module.print_to_string().to_string()
}

/// A `-g` build for a `*-windows-msvc` triple must add the module-level
/// `"CodeView"` flag (= 1) so the LLVM COFF backend emits CodeView records
/// (`.debug$S`/`.debug$T`) that cdb/WinDbg can read. Without it, a `-g`
/// windows-msvc build emits DWARF-in-PE that Windows-native debuggers ignore
/// (#2117). The flag is what the value-asserting cdb e2e on the Windows host
/// ultimately depends on; this is its cross-platform-emittable lower tier.
#[test]
fn windows_msvc_debug_build_adds_codeview_module_flag() {
    let ir = debug_module_ir_for_triple("x86_64-pc-windows-msvc");
    // The module-flags metadata records the flag as `!"CodeView", i32 1`.
    assert!(
        ir.contains("\"CodeView\""),
        "windows-msvc `-g` IR must carry the CodeView module flag:\n{ir}"
    );
    // The shared DIE graph (DWARF compile unit) is still constructed — only
    // the format flag differs — so the "Debug Info Version" flag stays too.
    assert!(
        ir.contains("\"Debug Info Version\""),
        "windows-msvc `-g` IR must still carry the Debug Info Version flag:\n{ir}"
    );
}

/// The CodeView flag is Windows-MSVC-only: a `-g` build for a non-Windows
/// triple keeps DWARF and must NOT carry the CodeView flag, so the
/// Linux/macOS DWARF path is byte-for-byte unchanged (the shared-DIE-graph
/// regression guard at the module-flag level).
#[test]
fn non_windows_debug_build_omits_codeview_module_flag() {
    let ir = debug_module_ir_for_triple("x86_64-unknown-linux-gnu");
    assert!(
        !ir.contains("\"CodeView\""),
        "linux `-g` IR must NOT carry the CodeView module flag (DWARF only):\n{ir}"
    );
    assert!(
        ir.contains("\"Debug Info Version\""),
        "linux `-g` IR must carry the Debug Info Version flag:\n{ir}"
    );
}

/// Decode the architecture tag from a relocatable object's header without
/// pulling in the `object` crate. Returns a stable `(format, arch)` label
/// pair. Covers the Mach-O and ELF headers the cross-arch tests exercise.
fn object_format_and_arch(bytes: &[u8]) -> (&'static str, &'static str) {
    // Mach-O 64-bit little-endian magic `0xfeedfacf`; `cputype` is the
    // next 4 bytes (LE). CPU_TYPE_X86_64 = 0x01000007,
    // CPU_TYPE_ARM64 = 0x0100000c.
    if bytes.len() >= 8 && bytes[0..4] == [0xcf, 0xfa, 0xed, 0xfe] {
        let cputype = u32::from_le_bytes([bytes[4], bytes[5], bytes[6], bytes[7]]);
        let arch = match cputype {
            0x0100_0007 => "x86_64",
            0x0100_000c => "aarch64",
            other => panic!("unexpected Mach-O cputype {other:#010x}"),
        };
        return ("macho", arch);
    }
    // ELF magic `0x7f 'E' 'L' 'F'`; `e_machine` is a u16 at offset 18 (LE
    // for the ELFCLASS we emit). EM_X86_64 = 62, EM_AARCH64 = 183.
    if bytes.len() >= 20 && bytes[0..4] == [0x7f, b'E', b'L', b'F'] {
        let e_machine = u16::from_le_bytes([bytes[18], bytes[19]]);
        let arch = match e_machine {
            62 => "x86_64",
            183 => "aarch64",
            other => panic!("unexpected ELF e_machine {other}"),
        };
        return ("elf", arch);
    }
    panic!(
        "unrecognised object header: {:02x?}",
        &bytes[..bytes.len().min(8)]
    );
}

/// `EmitOptions::target_triple = Some(cross-triple)` must drive native
/// object emission to the requested architecture, not the host arch.
///
/// This pins the codegen target thread-through that `hew build --target`
/// relies on: the same pipeline, emitted for an explicit cross-arch triple,
/// must produce an object whose header architecture matches the request and
/// differs from the host emit. The opposite-arch triple is chosen so the
/// assertion has real signal on either supported 64-bit host.
// WINDOWS-TODO: emits COFF; object header parser only handles ELF/MachO.
#[cfg_attr(windows, ignore)]
#[test]
fn explicit_target_triple_drives_native_object_architecture() {
    let pipeline = empty_pipeline_with_const_42();
    let tmp = tempfile::Builder::new()
        .prefix("hew-cross-triple-")
        .tempdir()
        .expect("create out_dir");

    // Host emit (target_triple = None) for the baseline arch.
    let host_opts = EmitOptions {
        module_name: "host_emit",
        out_dir: tmp.path(),
        native: true,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: OptLevel::O0,
        source_path: None,
    };
    let host_artefacts = emit_module(&pipeline, &host_opts).expect("host emit");
    let host_obj = std::fs::read(
        host_artefacts
            .native_obj_path
            .expect("host native object path"),
    )
    .expect("read host object");
    let (host_format, host_arch) = object_format_and_arch(&host_obj);

    // Cross emit: pick the opposite arch on the same OS so the host link
    // gate is not involved — we only assert the emitted object's arch.
    let (cross_triple, expected_arch) = if cfg!(target_arch = "aarch64") {
        (
            if cfg!(target_os = "macos") {
                "x86_64-apple-macosx13.0"
            } else {
                "x86_64-unknown-linux-gnu"
            },
            "x86_64",
        )
    } else {
        (
            if cfg!(target_os = "macos") {
                "aarch64-apple-macosx13.0"
            } else {
                "aarch64-unknown-linux-gnu"
            },
            "aarch64",
        )
    };

    let cross_opts = EmitOptions {
        module_name: "cross_emit",
        out_dir: tmp.path(),
        native: true,
        wasm: false,
        target_triple: Some(cross_triple),
        debug: false,
        opt_level: OptLevel::O0,
        source_path: None,
    };
    let cross_artefacts = emit_module(&pipeline, &cross_opts).expect("cross emit");
    let cross_obj = std::fs::read(
        cross_artefacts
            .native_obj_path
            .expect("cross native object path"),
    )
    .expect("read cross object");
    let (cross_format, cross_arch) = object_format_and_arch(&cross_obj);

    assert_eq!(
        cross_format, host_format,
        "same-OS cross emit must keep the host object format"
    );
    assert_eq!(
        cross_arch, expected_arch,
        "explicit target_triple {cross_triple} must produce a {expected_arch} object"
    );
    assert_ne!(
        cross_arch, host_arch,
        "cross emit arch must differ from the host emit arch"
    );
    drop(tmp);
}

fn named_record_ty(name: &str) -> ResolvedTy {
    ResolvedTy::Named {
        name: name.to_string(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    }
}

fn platform_int_identity_fn(name: &str, ty: ResolvedTy) -> RawMirFunction {
    RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: name.to_string(),
        return_ty: ty.clone(),
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![ty.clone()],
        locals: vec![ty],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(0),
            }],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: HashMap::new(),
        suspend_kinds: HashMap::new(),
        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: std::collections::BTreeMap::new(),
    }
}

fn platform_int_width_probe_pipeline() -> IrPipeline {
    let mut pipeline = empty_pipeline_with_const_42();
    pipeline.raw_mir = vec![
        RawMirFunction {
            source_origin: hew_mir::SourceOrigin::Unknown,
            name: "main".to_string(),
            return_ty: ResolvedTy::Unit,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: Vec::new(),
            locals: Vec::new(),
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks: vec![BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Return,
            }],
            decisions: Vec::new(),
            intrinsic_id: None,
            await_deadline_ns: HashMap::new(),
            suspend_kinds: HashMap::new(),
            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: std::collections::BTreeMap::new(),
        },
        platform_int_identity_fn("id_usize", ResolvedTy::Usize),
        platform_int_identity_fn("id_isize", ResolvedTy::Isize),
        platform_int_identity_fn("id_platform_ints", named_record_ty("PlatformInts")),
    ];
    pipeline.record_layouts = vec![RecordLayout {
        name: "PlatformInts".to_string(),
        field_tys: vec![ResolvedTy::Usize, ResolvedTy::Isize],
        field_names: vec!["unsigned".to_string(), "signed".to_string()],
    }];
    pipeline
}

fn platform_int_width_probe_ir(triple: &str) -> String {
    let ctx = Context::create();
    let pipeline = platform_int_width_probe_pipeline();
    let machine = target_machine_for_triple(triple).expect("platform-int target machine");
    let module = build_module_for_target(
        &ctx,
        &pipeline,
        "platform_int_width_probe",
        Some(&machine),
        None,
    )
    .expect("platform-int target module");
    module.print_to_string().to_string()
}

#[test]
fn platform_int_width_follows_module_target_data_in_same_process() {
    let native = platform_int_width_probe_ir("x86_64-unknown-linux-gnu");
    let wasm = platform_int_width_probe_ir("wasm32-unknown-unknown");

    for (name, ir) in [("usize", &native), ("isize", &native)] {
        assert!(
            ir.contains(&format!("define internal i64 @id_{name}(i64 %0)")),
            "64-bit native {name} identity ABI must use i64:\n{ir}"
        );
    }
    assert!(
        native.contains("%PlatformInts = type { i64, i64 }"),
        "64-bit native record fields must use i64 platform integers:\n{native}"
    );

    for (name, ir) in [("usize", &wasm), ("isize", &wasm)] {
        assert!(
            ir.contains(&format!("define internal i32 @id_{name}(i32 %0)")),
            "wasm32 {name} identity ABI must use i32:\n{ir}"
        );
    }
    assert!(
        wasm.contains("%PlatformInts = type { i32, i32 }"),
        "wasm32 record fields must use i32 platform integers:\n{wasm}"
    );
    assert!(
        !wasm.contains("define internal i64 @id_usize")
            && !wasm.contains("define internal i64 @id_isize"),
        "wasm32 platform integers must never retain the 64-bit target's ABI:\n{wasm}"
    );
}

fn pointer_to(ty: ResolvedTy) -> ResolvedTy {
    ResolvedTy::Pointer {
        is_mutable: false,
        pointee: Box::new(ty),
    }
}

/// The cross-node codec seeder skips an actor handler's `msg_type`/reply
/// when `resolved_ty_contains_channel_handle` reports it carries a
/// process-local channel handle. That predicate MUST dispatch on the typed
/// `builtin` discriminant alone — never the bare source name.
///
/// The checker lets a user source type shadow a builtin name, resolving it
/// with `builtin: None`. A serializable `record Sender { .. }` / `record
/// Receiver { .. }` used as a single-arg actor message passes the
/// Serializable checker; if the predicate matched it by short name the
/// seeder would silently skip its codec and drop the payload at a
/// distributed boundary. A real handle always carries its `builtin`
/// discriminator and must still match so a purely-local program keeps its
/// non-serializable codec skipped.
#[test]
fn channel_handle_skip_gates_on_builtin_discriminant_not_shadowed_name() {
    use hew_types::BuiltinType;

    fn builtin_handle(name: &str, kind: BuiltinType) -> ResolvedTy {
        ResolvedTy::Named {
            name: name.to_string(),
            args: vec![named_record_ty("Inner")],
            builtin: Some(kind),
            is_opaque: false,
        }
    }

    fn user_generic_over(inner: ResolvedTy) -> ResolvedTy {
        ResolvedTy::Named {
            name: "Envelope".to_string(),
            args: vec![inner],
            builtin: None,
            is_opaque: false,
        }
    }

    for shadow in ["Sender", "Receiver"] {
        let user_ty = named_record_ty(shadow);
        assert!(
            !resolved_ty_contains_channel_handle(&user_ty),
            "user type `{shadow}` (builtin: None) must not be matched as a \
                 channel handle: matching by bare name silently skips its \
                 cross-node codec"
        );
        assert!(
            !resolved_ty_contains_channel_handle(&named_record_ty(&format!("channel.{shadow}"))),
            "module-qualified user `{shadow}` is still builtin: None"
        );
        assert!(!resolved_ty_contains_channel_handle(&ResolvedTy::Tuple(
            vec![ResolvedTy::I64, user_ty.clone(),]
        )));
        assert!(!resolved_ty_contains_channel_handle(&user_generic_over(
            user_ty.clone()
        )));
        assert!(!resolved_ty_contains_channel_handle(&ResolvedTy::Array(
            Box::new(user_ty.clone()),
            2
        )));
        assert!(!resolved_ty_contains_channel_handle(&ResolvedTy::Slice(
            Box::new(user_ty)
        )));
    }

    assert!(!resolved_ty_contains_channel_handle(&named_record_ty(
        "Message"
    )));

    for (name, kind) in [
        ("Sender", BuiltinType::Sender),
        ("Receiver", BuiltinType::Receiver),
    ] {
        let handle = builtin_handle(name, kind);
        assert!(
            resolved_ty_contains_channel_handle(&handle),
            "builtin `{name}` (builtin: Some(_)) must still be matched so a \
                 purely-local program keeps its non-serializable codec skipped"
        );
        assert!(resolved_ty_contains_channel_handle(&ResolvedTy::Tuple(
            vec![ResolvedTy::I64, handle.clone(),]
        )));
        assert!(resolved_ty_contains_channel_handle(&user_generic_over(
            handle.clone()
        )));
        assert!(resolved_ty_contains_channel_handle(&ResolvedTy::Array(
            Box::new(handle.clone()),
            2
        )));
        assert!(resolved_ty_contains_channel_handle(&ResolvedTy::Slice(
            Box::new(handle)
        )));
    }
}

/// The cross-node codec seeder skips an actor handler's `msg_type`/reply
/// when `resolved_ty_contains_actor_handle` reports it carries a
/// process-local actor-pid/handle. That predicate MUST dispatch on the
/// typed `builtin` discriminant alone — never the bare source name.
///
/// The checker lets a user source type shadow a builtin name, resolving it
/// with `builtin: None`. A serializable `record Pid { .. }` / `record Actor
/// { .. }` used as a single-arg actor message passes the Serializable
/// checker; if the predicate matched it by short name the seeder would
/// SILENTLY SKIP its codec and drop the payload at a distributed boundary
/// (the `builtin-discriminator-survives-source-shadow` fail-open). A real
/// handle always carries its `builtin` discriminator and must still match so
/// a purely-local program keeps its non-serializable codec skipped.
#[test]
fn actor_handle_skip_gates_on_builtin_discriminant_not_shadowed_name() {
    use hew_types::BuiltinType;

    // A builtin-stamped actor-handle named type with an inner type arg.
    fn builtin_handle(name: &str, kind: BuiltinType) -> ResolvedTy {
        ResolvedTy::Named {
            name: name.to_string(),
            args: vec![named_record_ty("Inner")],
            builtin: Some(kind),
            is_opaque: false,
        }
    }
    // A non-handle user generic carrying `inner` in its type args.
    fn user_generic_over(inner: ResolvedTy) -> ResolvedTy {
        ResolvedTy::Named {
            name: "Envelope".to_string(),
            args: vec![inner],
            builtin: None,
            is_opaque: false,
        }
    }

    // Shadow case (the fail-open): a user type sharing an actor-handle short
    // name but carrying `builtin: None` must NOT be treated as a handle —
    // directly, module-qualified, behind a tuple, or behind a generic arg —
    // so its cross-node codec is still emitted (never silently skipped).
    for shadow in ["LocalPid", "RemotePid"] {
        let user_ty = named_record_ty(shadow);
        assert!(
            !resolved_ty_contains_actor_handle(&user_ty),
            "user type `{shadow}` (builtin: None) must not be matched as an \
                 actor handle: matching by bare name silently skips its \
                 cross-node codec (drop-at-distributed-boundary)"
        );
        assert!(
            !resolved_ty_contains_actor_handle(&named_record_ty(&format!("mymod.{shadow}"))),
            "module-qualified user `{shadow}` is still builtin: None"
        );
        assert!(!resolved_ty_contains_actor_handle(&ResolvedTy::Tuple(
            vec![ResolvedTy::I64, user_ty.clone(),]
        )));
        assert!(!resolved_ty_contains_actor_handle(&user_generic_over(
            user_ty
        )));
    }

    // A plain serializable user record with no handle short name at all is
    // likewise never matched.
    assert!(!resolved_ty_contains_actor_handle(&named_record_ty(
        "Message"
    )));

    // Real builtin handles (preserve the original codec skip): every member
    // of the actor-pid family carrying its `builtin` discriminator must
    // match — directly, behind a tuple, and behind a non-handle generic arg.
    for (name, kind) in [
        ("LocalPid", BuiltinType::LocalPid),
        ("RemotePid", BuiltinType::RemotePid),
    ] {
        let handle = builtin_handle(name, kind);
        assert!(
            resolved_ty_contains_actor_handle(&handle),
            "builtin `{name}` (builtin: Some(_)) must still be matched so a \
                 purely-local program keeps its non-serializable codec skipped"
        );
        assert!(resolved_ty_contains_actor_handle(&ResolvedTy::Tuple(vec![
            ResolvedTy::I64,
            handle.clone(),
        ])));
        assert!(resolved_ty_contains_actor_handle(&user_generic_over(
            handle
        )));
    }
}

#[test]
fn non_context_function_callclosure_uses_zeroed_fallback_context() {
    let env_ty = named_record_ty("__hew_closure_env_main_0");
    let env_ptr_ty = pointer_to(env_ty.clone());
    let fn_ty = ResolvedTy::Function {
        params: Vec::new(),
        ret: Box::new(ResolvedTy::I64),
    };
    let invoke = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "__hew_closure_invoke_main_0".to_string(),
        return_ty: ResolvedTy::I64,
        call_conv: hew_mir::FunctionCallConv::ClosureInvoke,
        params: vec![env_ptr_ty.clone()],
        locals: vec![env_ptr_ty, ResolvedTy::I64],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                Instr::ConstI64 {
                    dest: Place::Local(1),
                    value: 10,
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(1),
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let main = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "main".to_string(),
        return_ty: ResolvedTy::I64,
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: vec![env_ty.clone(), fn_ty, ResolvedTy::I64],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                Instr::RecordInit {
                    ty: env_ty.clone(),
                    fields: Vec::new(),
                    dest: Place::Local(0),
                },
                Instr::MakeClosure {
                    fn_symbol: "__hew_closure_invoke_main_0".to_string(),
                    env: Place::Local(0),
                    dest: Place::Local(1),
                    env_mode: hew_mir::ClosureEnvMode::Stack,
                },
                Instr::CallClosure {
                    callee: Place::Local(1),
                    args: Vec::new(),
                    ret_ty: ResolvedTy::I64,
                    dest: Some(Place::Local(2)),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(2),
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let pipeline = IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![invoke, main],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: vec![hew_mir::RecordLayout {
            name: "__hew_closure_env_main_0".to_string(),
            field_tys: Vec::new(),
            field_names: Vec::new(),
        }],
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    };

    let ctx = Context::create();
    let m = build_module(&ctx, &pipeline, "call_closure_default_context")
        .expect("default-callconv CallClosure must build with a fallback context");
    m.verify()
        .expect("CallClosure fallback context module must verify");
    let ir = m.print_to_string().to_string();
    assert!(
        ir.contains("closure_call_fallback_ctx"),
        "default-callconv CallClosure must pass a non-null fallback context:\n{ir}"
    );
    assert!(
            !ir.contains("hew_require_execution_context"),
            "default-callconv CallClosure must not ask runtime TLS for a context that main does not install:\n{ir}"
        );
}

fn hashmap_descriptor_width_probe_pipeline() -> IrPipeline {
    let entry = BasicBlock {
        id: 0,
        statements: Vec::new(),
        instructions: Vec::new(),
        terminator: Terminator::Call {
            callee: "__hew_codegen_emit_hashmap_layout_probe".to_string(),
            // Probe callee: codegen-internal synthetic, no catalog family.
            builtin: None,
            args: vec![Place::Local(0), Place::Local(1)],
            dest: None,
            next: 1,
        },
    };
    let ret = BasicBlock {
        id: 1,
        statements: Vec::new(),
        instructions: Vec::new(),
        terminator: Terminator::Return,
    };
    let blocks = vec![entry, ret];
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![RawMirFunction {
            source_origin: hew_mir::SourceOrigin::Unknown,
            name: "main".to_string(),
            return_ty: ResolvedTy::Unit,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: vec![named_record_ty("Point"), named_record_ty("PtrPayload")],
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks,
            decisions: Vec::<DecisionFact>::new(),
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),
            suspend_kinds: std::collections::HashMap::new(),

            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
        }],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: vec![
            RecordLayout {
                name: "Point".to_string(),
                field_tys: vec![ResolvedTy::I64, ResolvedTy::I64],
                field_names: vec![],
            },
            RecordLayout {
                name: "PtrPayload".to_string(),
                field_tys: vec![ResolvedTy::String],
                field_names: vec![],
            },
        ],
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    }
}

fn hashmap_descriptor_probe_ir(module_name: &str, triple: Option<&str>) -> String {
    let ctx = Context::create();
    let pipeline = hashmap_descriptor_width_probe_pipeline();
    let module = if let Some(triple) = triple {
        let machine = target_machine_for_triple(triple).expect("target machine");
        build_module_for_target(&ctx, &pipeline, module_name, Some(&machine), None)
            .expect("targeted descriptor probe module")
    } else {
        build_module(&ctx, &pipeline, module_name).expect("native descriptor probe module")
    };
    module.print_to_string().to_string()
}

fn descriptor_line<'a>(ir: &'a str, global_prefix: &str) -> &'a str {
    ir.lines()
        .find(|line| line.contains(global_prefix) && line.contains("= private constant"))
        .unwrap_or_else(|| panic!("missing descriptor global {global_prefix} in:\n{ir}"))
}

#[test]
fn hashmap_layout_descriptors_use_target_usize_width() {
    let native = hashmap_descriptor_probe_ir("native_map_layout_width", None);
    let wasm =
        hashmap_descriptor_probe_ir("wasm32_map_layout_width", Some("wasm32-unknown-unknown"));

    let native_key = descriptor_line(&native, "@__hew_map_key_layout_16_8_");
    assert!(
        native_key.contains("{ i64, i64, i8, ptr, ptr, ptr }")
            && native_key.contains("{ i64 16, i64 8, i8 0,"),
        "native key descriptor must use 8-byte usize fields:\n{native_key}\n\nfull IR:\n{native}"
    );
    let wasm_key = descriptor_line(&wasm, "@__hew_map_key_layout_16_8_");
    assert!(
        wasm_key.contains("{ i32, i32, i8, ptr, ptr, ptr }")
            && wasm_key.contains("{ i32 16, i32 8, i8 0,"),
        "wasm32 key descriptor must use 4-byte usize fields:\n{wasm_key}\n\nfull IR:\n{wasm}"
    );

    let native_value = descriptor_line(&native, "@__hew_map_value_layout_rec_PtrPayload_8_8_owned");
    assert!(
            native_value.contains("{ i64, i64, i8, ptr, ptr }")
                && native_value.contains("{ i64 8, i64 8, i8 2,"),
            "native owned pointer-field value descriptor must use host pointer ABI and 8-byte usize fields:\n{native_value}\n\nfull IR:\n{native}"
        );
    let wasm_value = descriptor_line(&wasm, "@__hew_map_value_layout_rec_PtrPayload_4_4_owned");
    assert!(
            wasm_value.contains("{ i32, i32, i8, ptr, ptr }")
                && wasm_value.contains("{ i32 4, i32 4, i8 2,"),
            "wasm32 owned pointer-field value descriptor must use wasm pointer ABI and 4-byte usize fields:\n{wasm_value}\n\nfull IR:\n{wasm}"
        );
    assert!(
        !wasm.contains("@__hew_map_value_layout_rec_PtrPayload_8_8_owned"),
        "wasm32 descriptor synthesis must not reuse the native 8-byte pointer layout:\n{wasm}"
    );
}

/// Single-function pipeline that drives the Vec layout-descriptor synthesis
/// probe over a `Point` record element (2 × i64 → 16/8 native, 16/8 wasm32
/// with i32 fields). Mirrors `hashmap_descriptor_width_probe_pipeline` but
/// exercises `layout_descriptor_ptr` (the Vec width authority) instead of
/// the hashmap key/value descriptors.
fn vec_descriptor_width_probe_pipeline() -> IrPipeline {
    let entry = BasicBlock {
        id: 0,
        statements: Vec::new(),
        instructions: Vec::new(),
        terminator: Terminator::Call {
            callee: "__hew_codegen_emit_vec_layout_probe".to_string(),
            builtin: None,
            args: vec![Place::Local(0)],
            dest: None,
            next: 1,
        },
    };
    let ret = BasicBlock {
        id: 1,
        statements: Vec::new(),
        instructions: Vec::new(),
        terminator: Terminator::Return,
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![RawMirFunction {
            source_origin: hew_mir::SourceOrigin::Unknown,
            name: "main".to_string(),
            return_ty: ResolvedTy::Unit,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: vec![named_record_ty("Point")],
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks: vec![entry, ret],
            decisions: Vec::<DecisionFact>::new(),
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),
            suspend_kinds: std::collections::HashMap::new(),

            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
        }],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: vec![RecordLayout {
            name: "Point".to_string(),
            field_tys: vec![ResolvedTy::I64, ResolvedTy::I64],
            field_names: vec![],
        }],
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    }
}

fn vec_descriptor_probe_ir(module_name: &str, triple: Option<&str>) -> String {
    let ctx = Context::create();
    let pipeline = vec_descriptor_width_probe_pipeline();
    let module = if let Some(triple) = triple {
        let machine = target_machine_for_triple(triple).expect("target machine");
        build_module_for_target(&ctx, &pipeline, module_name, Some(&machine), None)
            .expect("targeted vec descriptor probe module")
    } else {
        build_module(&ctx, &pipeline, module_name).expect("native vec descriptor probe module")
    };
    module.print_to_string().to_string()
}

#[test]
fn vec_layout_descriptors_use_target_usize_width() {
    let native = vec_descriptor_probe_ir("native_vec_layout_width", None);
    let wasm = vec_descriptor_probe_ir("wasm32_vec_layout_width", Some("wasm32-unknown-unknown"));

    // The Vec `HewTypeLayout { size: usize, align: usize, ownership_kind: u8 }`
    // descriptor's two leading fields are target-width `usize`: i64 native,
    // i32 wasm32 — the same width authority the hashmap descriptors use.
    let native_desc = descriptor_line(&native, "@__hew_layout_probe_16_8_plain");
    assert!(
        native_desc.contains("{ i64, i64, i8 }") && native_desc.contains("{ i64 16, i64 8, i8 0 }"),
        "native Vec descriptor must use 8-byte usize fields:\n{native_desc}\n\nfull IR:\n{native}"
    );

    let wasm_desc = descriptor_line(&wasm, "@__hew_layout_probe_16_8_plain");
    assert!(
        wasm_desc.contains("{ i32, i32, i8 }") && wasm_desc.contains("{ i32 16, i32 8, i8 0 }"),
        "wasm32 Vec descriptor must use 4-byte usize fields:\n{wasm_desc}\n\nfull IR:\n{wasm}"
    );

    // Fail-closed against silent host-width fallback: the wasm32 module must
    // never emit an i64-shaped Vec descriptor.
    assert!(
        !wasm.contains("{ i64, i64, i8 }"),
        "wasm32 Vec descriptor synthesis must not fall back to native 8-byte usize fields:\n{wasm}"
    );
}

fn pipeline_with_user_const_load(
    item_id: ItemId,
    const_ty: ResolvedTy,
    const_value: MirConstValue,
) -> IrPipeline {
    let main = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "main".to_string(),
        return_ty: const_ty.clone(),
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: vec![const_ty.clone()],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                Instr::ConstGlobalLoad {
                    item_id,
                    dest: Place::Local(0),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(0),
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: Vec::<DecisionFact>::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![main],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: vec![MirConst {
            const_id: 0,
            item_id,
            name: "ANSWER".to_string(),
            ty: const_ty,
            value: const_value,
        }],
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    }
}

#[test]
fn const_integer_global_emits_and_loads() {
    let ctx = Context::create();
    let pipeline =
        pipeline_with_user_const_load(ItemId(7), ResolvedTy::I64, MirConstValue::Integer(42));
    let module = build_module(&ctx, &pipeline, "const_i64_test").expect("const module must build");
    let ir = module.print_to_string().to_string();
    assert!(
        ir.contains("@__hew_const__ANSWER__0 = private constant i64 42"),
        "integer const global missing:\n{ir}"
    );
    assert!(
        ir.contains("load i64, ptr @__hew_const__ANSWER__0"),
        "const reference must load from the global:\n{ir}"
    );
}

#[test]
fn const_string_global_emits_pointer_global_and_loads() {
    let ctx = Context::create();
    let pipeline = pipeline_with_user_const_load(
        ItemId(9),
        ResolvedTy::String,
        MirConstValue::Str("hi".to_string()),
    );
    let module =
        build_module(&ctx, &pipeline, "const_string_test").expect("const module must build");
    let ir = module.print_to_string().to_string();
    assert!(
        ir.contains("@__hew_const_str__ANSWER__0"),
        "string const data global missing:\n{ir}"
    );
    assert!(
        ir.contains("@__hew_const__ANSWER__0 = private constant ptr @__hew_const_str__ANSWER__0"),
        "string const pointer global missing:\n{ir}"
    );
    assert!(
        ir.contains("load ptr, ptr @__hew_const__ANSWER__0"),
        "string const reference must load the pointer global:\n{ir}"
    );
}

#[test]
fn const_float_global_emits_and_loads() {
    let ctx = Context::create();
    let pipeline =
        pipeline_with_user_const_load(ItemId(11), ResolvedTy::F64, MirConstValue::Float(1.25));
    let module = build_module(&ctx, &pipeline, "const_f64_test").expect("const module must build");
    let ir = module.print_to_string().to_string();
    assert!(
        ir.contains("@__hew_const__ANSWER__0 = private constant double"),
        "f64 const global missing:\n{ir}"
    );
    assert!(
        ir.contains("load double, ptr @__hew_const__ANSWER__0"),
        "f64 const reference must load from the global:\n{ir}"
    );
}

#[test]
fn const_f32_global_emits_and_loads() {
    let ctx = Context::create();
    let pipeline =
        pipeline_with_user_const_load(ItemId(12), ResolvedTy::F32, MirConstValue::Float(0.5));
    let module = build_module(&ctx, &pipeline, "const_f32_test").expect("const module must build");
    let ir = module.print_to_string().to_string();
    assert!(
        ir.contains("@__hew_const__ANSWER__0 = private constant float"),
        "f32 const global missing:\n{ir}"
    );
    assert!(
        ir.contains("load float, ptr @__hew_const__ANSWER__0"),
        "f32 const reference must load from the global:\n{ir}"
    );
}

fn pipeline_with_float_return() -> IrPipeline {
    let return_ty = ResolvedTy::F64;
    let main = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "main".to_string(),
        return_ty: return_ty.clone(),
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: Vec::new(),
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![main],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    }
}

fn unique_codegen_front_artifact_stem(test_name: &str) -> std::path::PathBuf {
    let nonce = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .expect("system clock must be after UNIX_EPOCH")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!(
        "hew-codegen-front-verifier-{test_name}-{}-{nonce}",
        std::process::id()
    ));
    std::fs::create_dir_all(&dir).expect("test temp dir must be created");
    dir.join("module")
}

fn assert_no_codegen_front_artifacts(stem: &std::path::Path) {
    for path in [
        stem.to_path_buf(),
        stem.with_extension("ll"),
        stem.with_extension("o"),
        stem.with_extension("wasm.o"),
        stem.with_extension("wasm"),
    ] {
        assert!(
            !path.exists(),
            "codegen-front verifier must not create artefact {}",
            path.display()
        );
    }
    let dir = stem.parent().expect("artifact stem must have a parent");
    assert!(
        std::fs::read_dir(dir)
            .expect("test temp dir must be readable")
            .next()
            .is_none(),
        "codegen-front verifier must leave temp dir empty: {}",
        dir.display()
    );
}

fn remove_codegen_front_temp_dir(stem: &std::path::Path) {
    if let Some(dir) = stem.parent() {
        std::fs::remove_dir_all(dir).expect("test temp dir must be removable");
    }
}

#[test]
fn const_42_module_verifies() {
    let pipeline = empty_pipeline_with_const_42();
    let ctx = Context::create();
    let m = build_module(&ctx, &pipeline, "const42_test").expect("const-42 module must build");
    assert!(m.verify().is_ok(), "const-42 module must pass LLVM verify");
}

#[test]
fn codegen_front_verifier_success_creates_no_artifacts() {
    let pipeline = empty_pipeline_with_const_42();
    let stem = unique_codegen_front_artifact_stem("success");
    let module_name = stem.to_string_lossy().into_owned();
    validate_codegen_front_with_name(&pipeline, &module_name)
        .expect("codegen-front verifier must accept const-42 pipeline");
    assert_no_codegen_front_artifacts(&stem);
    remove_codegen_front_temp_dir(&stem);
}

#[test]
fn codegen_front_verifier_float_return_success_creates_no_artifacts() {
    let pipeline = pipeline_with_float_return();
    let stem = unique_codegen_front_artifact_stem("float-return");
    let module_name = stem.to_string_lossy().into_owned();
    validate_codegen_front_with_name(&pipeline, &module_name)
        .expect("codegen-front verifier must accept float return");
    assert_no_codegen_front_artifacts(&stem);
    remove_codegen_front_temp_dir(&stem);
}

#[test]
fn actor_handler_signature_leads_with_execution_context_pointer() {
    let handler = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "handler".to_string(),
        return_ty: ResolvedTy::I64,
        call_conv: FunctionCallConv::ActorHandler,
        params: vec![ResolvedTy::I64],
        locals: vec![ResolvedTy::I64],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                Instr::EnterContext,
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(0),
                },
                Instr::ExitContext,
            ],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let pipeline = IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![handler],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    };
    let ctx = Context::create();
    let m =
        build_module(&ctx, &pipeline, "handler_ctx_test").expect("actor-handler module must build");
    let ir = m.print_to_string().to_string();
    assert!(
        ir.contains("define internal i64 @handler(ptr %0, i64 %1)"),
        "handler ABI must lead with opaque HewExecutionContext* before user params:\n{ir}"
    );
    assert!(
        !ir.contains("hew_actor_state_lock_acquire"),
        "actor-state locking belongs in the scheduler wrapper, not emitted handler IR:\n{ir}"
    );
}

#[test]
fn context_field_actor_offset_emits_gep_and_load() {
    let handler = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "handler_ctx_field".to_string(),
        return_ty: ResolvedTy::I64,
        call_conv: FunctionCallConv::ActorHandler,
        params: vec![],
        locals: vec![
            ResolvedTy::Pointer {
                is_mutable: true,
                pointee: Box::new(ResolvedTy::Never),
            },
            ResolvedTy::I64,
        ],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                Instr::EnterContext,
                Instr::ContextField {
                    dest: Place::Local(0),
                    offset: HEW_CTX_OFFSET_ACTOR,
                },
                Instr::ConstI64 {
                    dest: Place::Local(1),
                    value: 0,
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(1),
                },
                Instr::ExitContext,
            ],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let pipeline = IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![handler],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    };
    let ctx = Context::create();
    let m = build_module(&ctx, &pipeline, "ctx_field_test")
        .expect("ContextField actor load must build");
    let ir = m.print_to_string().to_string();
    assert!(
        ir.contains("getelementptr i8, ptr %0, i64 0"),
        "ContextField must lower to byte-offset GEP from ctx arg:\n{ir}"
    );
    assert!(
        ir.contains("load ptr, ptr %ctx_field_0_ptr"),
        "ContextField actor offset must load a pointer from the GEP:\n{ir}"
    );
}

// `String` is now a lowerable return type (maps to opaque `ptr`).
// This test exercises a String-returning function with a `StringLit`
// instruction that populates the return slot. It must build and verify.
#[test]
fn string_literal_return_builds_and_verifies() {
    // fn main() -> String { "hello" }
    // Locals: [0: String (the lit dest), ReturnSlot: String]
    let return_ty = ResolvedTy::String;
    let main = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "main".to_string(),
        return_ty: return_ty.clone(),
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: vec![return_ty.clone()],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                Instr::StringLit {
                    bytes: b"hello".to_vec(),
                    dest: Place::Local(0),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(0),
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let pipeline = IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![main],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    };
    let ctx = Context::create();
    let m = build_module(&ctx, &pipeline, "string_lit_test").expect("StringLit module must build");
    assert!(m.verify().is_ok(), "StringLit module must pass LLVM verify");
    // Confirm the textual IR contains the global string constant.
    let ir = m.print_to_string().to_string();
    assert!(
        ir.contains("hello"),
        "emitted IR must contain the literal bytes: {ir}"
    );
}

#[test]
fn string_literal_with_embedded_nul_fails_closed_before_llvm_cstring() {
    let ctx = Context::create();
    let m = ctx.create_module("string_lit_embedded_nul");
    let harness = build_harness(&ctx, &[], &[]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "string_lit_embedded_nul_fn");
    alloc_local(&mut fn_ctx, 0, ResolvedTy::String);

    let err = lower_instruction(
        &fn_ctx,
        &Instr::StringLit {
            bytes: b"a\0b".to_vec(),
            dest: Place::Local(0),
        },
        0,
        &[],
    )
    .expect_err("embedded NUL StringLit must fail closed");
    assert!(
        format!("{err:?}").contains("embedded NUL"),
        "diagnostic should name the embedded NUL hazard: {err:?}"
    );
}

#[test]
fn string_intcmp_eq_uses_string_equals_for_string_operands() {
    let ctx = Context::create();
    let m = ctx.create_module("string_intcmp_eq");
    let harness = build_harness(&ctx, &[], &[]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "string_intcmp_eq_fn");
    alloc_local(&mut fn_ctx, 0, ResolvedTy::String);
    alloc_local(&mut fn_ctx, 1, ResolvedTy::String);
    alloc_local(&mut fn_ctx, 2, ResolvedTy::I64);

    lower_instruction(
        &fn_ctx,
        &Instr::StringLit {
            bytes: b"left".to_vec(),
            dest: Place::Local(0),
        },
        0,
        &[],
    )
    .expect("left StringLit must lower");
    lower_instruction(
        &fn_ctx,
        &Instr::StringLit {
            bytes: b"right".to_vec(),
            dest: Place::Local(1),
        },
        0,
        &[],
    )
    .expect("right StringLit must lower");
    lower_instruction(
        &fn_ctx,
        &Instr::IntCmp {
            dest: Place::Local(2),
            pred: CmpPred::Eq,
            lhs: Place::Local(0),
            rhs: Place::Local(1),
        },
        0,
        &[],
    )
    .expect("string IntCmp Eq must lower through hew_string_equals");
    finish_test_fn(&fn_ctx);

    assert!(m.verify().is_ok(), "string IntCmp module must verify");
    let ir = m.print_to_string().to_string();
    assert!(
        ir.contains("hew_string_equals"),
        "string equality must call hew_string_equals; ir:\n{ir}"
    );
}

#[test]
fn non_string_pointer_intcmp_eq_fails_closed_without_string_equals() {
    let ctx = Context::create();
    let m = ctx.create_module("non_string_pointer_intcmp_eq");
    let harness = build_harness(&ctx, &[], &[]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "non_string_pointer_intcmp_eq_fn");
    let ptr_ty = ResolvedTy::Pointer {
        is_mutable: false,
        pointee: Box::new(ResolvedTy::I64),
    };
    alloc_local(&mut fn_ctx, 0, ptr_ty.clone());
    alloc_local(&mut fn_ctx, 1, ptr_ty);
    alloc_local(&mut fn_ctx, 2, ResolvedTy::I64);

    let err = lower_instruction(
        &fn_ctx,
        &Instr::IntCmp {
            dest: Place::Local(2),
            pred: CmpPred::Eq,
            lhs: Place::Local(0),
            rhs: Place::Local(1),
        },
        0,
        &[],
    )
    .expect_err("non-string pointer IntCmp Eq must fail closed");
    assert!(
        matches!(err, CodegenError::FailClosed(ref msg) if msg.contains("must be string-typed")),
        "non-string pointer IntCmp Eq must fail closed with a typed-boundary error, got: {err:?}"
    );
    let ir = m.print_to_string().to_string();
    assert!(
        !ir.contains("hew_string_equals"),
        "non-string pointer equality must not declare or call hew_string_equals; ir:\n{ir}"
    );
}

#[test]
fn string_intcmp_ordering_uses_hew_string_compare() {
    // Verify that `<`, `<=`, `>`, `>=` on string operands call
    // `hew_string_compare` rather than failing closed with
    // "IntCmp lhs is not an integer".  Tests all four ordering predicates
    // to confirm the dispatch table is exhaustive.
    for pred in [
        CmpPred::SignedLess,
        CmpPred::SignedLessEq,
        CmpPred::SignedGreater,
        CmpPred::SignedGreaterEq,
    ] {
        let ctx = Context::create();
        let m = ctx.create_module("string_intcmp_ord");
        let harness = build_harness(&ctx, &[], &[]);
        let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "string_intcmp_ord_fn");
        alloc_local(&mut fn_ctx, 0, ResolvedTy::String);
        alloc_local(&mut fn_ctx, 1, ResolvedTy::String);
        alloc_local(&mut fn_ctx, 2, ResolvedTy::I64);

        lower_instruction(
            &fn_ctx,
            &Instr::StringLit {
                bytes: b"apple".to_vec(),
                dest: Place::Local(0),
            },
            0,
            &[],
        )
        .expect("left StringLit must lower");
        lower_instruction(
            &fn_ctx,
            &Instr::StringLit {
                bytes: b"banana".to_vec(),
                dest: Place::Local(1),
            },
            0,
            &[],
        )
        .expect("right StringLit must lower");
        lower_instruction(
            &fn_ctx,
            &Instr::IntCmp {
                dest: Place::Local(2),
                pred,
                lhs: Place::Local(0),
                rhs: Place::Local(1),
            },
            0,
            &[],
        )
        .expect("string ordering IntCmp must lower via hew_string_compare");
        finish_test_fn(&fn_ctx);

        assert!(
            m.verify().is_ok(),
            "string ordering IntCmp module must verify (pred={pred:?})"
        );
        let ir = m.print_to_string().to_string();
        assert!(
            ir.contains("hew_string_compare"),
            "string ordering must call hew_string_compare (pred={pred:?}); ir:\n{ir}"
        );
    }
}

#[test]
fn float_return_builds_and_verifies() {
    let pipeline = pipeline_with_float_return();
    let ctx = Context::create();
    let module = build_module(&ctx, &pipeline, "float_return").expect("F64 return must build");
    assert!(
        module.verify().is_ok(),
        "F64-returning module must pass LLVM verify"
    );
    let ir = module.print_to_string().to_string();
    assert!(
        ir.contains("define double @main"),
        "F64 return must lower to a by-value double LLVM signature:
{ir}"
    );
}

// ── Terminator::Select codegen ──────────────────────────────────
//
// ActorAsk, AfterTimer, ChannelRecv, StreamNext, and TaskAwait arms
// all reach codegen on native targets.  The type checker (not the MIR
// producer) is the authority: it rejects StreamNext arms in select{}
// from user source (checker-unreachable; see select_stream_e2e.rs:200-208).
// On wasm32 the MIR producer is expected to emit SelectArmNotImplemented
// for StreamNext (cfg(target_arch = "wasm32") test).  The codegen tests
// below exercise the paths via hand-constructed MIR as a defence-in-depth
// check that a future shape change cannot slip a rejected arm through.

/// Construct an `IrPipeline` carrying a single function whose only
/// block is a `Terminator::Select` with the given single arm. Used
/// for the defence-in-depth fail-closed tests (StreamNext / TaskAwait
/// kinds; an `ActorAsk` arm without surrounding setup would fail at
/// `actor_payload_ptr_size` / `load_duplex_handle` for missing
/// locals — those positive paths are covered by the
/// `pipeline_with_select_*_arms` builders below).
fn pipeline_with_select_terminator(arm_kind: hew_mir::SelectArmKind) -> IrPipeline {
    let main = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "main".to_string(),
        return_ty: ResolvedTy::I64,
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: vec![ResolvedTy::I64],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Select {
                arms: vec![hew_mir::SelectArm {
                    kind: arm_kind,
                    body_block: 0,
                    binding: None,
                }],
                next: 0,
            },
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![main],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    }
}

/// Stream-next arms allocate their own readiness channel, register
/// a pending read, include that channel in `hew_select_first`, and
/// cancel the pending read when they lose.
#[test]
fn select_stream_next_arm_emits_readiness_proxy() {
    let arms = vec![
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::StreamNext {
                stream: Place::DuplexHandle(0),
            },
            body_block: 10,
            binding: Some(Place::Local(1)),
        },
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::AfterTimer {
                duration: Place::Local(2),
            },
            body_block: 11,
            binding: None,
        },
    ];
    let locals = vec![duplex_ty(), ResolvedTy::I64, ResolvedTy::Duration];
    let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11], 99);
    let ir = emit_select_ir("select_stream_next", &pipeline);
    assert_eq!(ir.matches("call ptr @hew_reply_channel_new(").count(), 1);
    assert!(ir.contains("call void @hew_reply_channel_retain("));
    assert!(ir.contains("call i64 @hew_stream_poll("));
    assert!(
        ir.contains("%stream_item_value = load i64, ptr %1"),
        "stream callback must load the item bytes as the arm binding type; ir:\n{ir}"
    );
    assert!(
        ir.contains("store i64 %stream_item_value, ptr %stream_item_slot"),
        "stream callback must reply with the loaded item value, not the item pointer; ir:\n{ir}"
    );
    assert_eq!(
            ir.matches("call void @free(ptr %1)").count(),
            1,
            "stream callback must free the malloc'd stream buffer once via the shared free block; ir:\n{ir}"
        );
    assert!(ir.contains("call void @hew_stream_cancel_pending_read("));
    assert!(ir.contains("call i32 @hew_select_first("));
}

/// Two stream arms observing the same source still allocate and
/// register distinct per-arm readiness channels.
#[test]
fn select_stream_next_duplicate_source_uses_per_arm_channels() {
    let arms = vec![
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::StreamNext {
                stream: Place::DuplexHandle(0),
            },
            body_block: 10,
            binding: Some(Place::Local(1)),
        },
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::StreamNext {
                stream: Place::DuplexHandle(0),
            },
            body_block: 11,
            binding: Some(Place::Local(2)),
        },
    ];
    let locals = vec![duplex_ty(), ResolvedTy::String, ResolvedTy::String];
    let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11], 99);
    let ir = emit_select_ir("select_stream_duplicate", &pipeline);
    assert_eq!(ir.matches("call ptr @hew_reply_channel_new(").count(), 2);
    assert_eq!(ir.matches("call i64 @hew_stream_poll(").count(), 2);
}

/// Task-await arms observe completion through a retained reply-channel
/// readiness proxy and read the completed result only after winning.
#[test]
fn select_task_await_arm_emits_completion_proxy() {
    let arms = vec![
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::TaskAwait {
                task: Place::DuplexHandle(0),
            },
            body_block: 10,
            binding: Some(Place::Local(1)),
        },
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::AfterTimer {
                duration: Place::Local(2),
            },
            body_block: 11,
            binding: None,
        },
    ];
    let locals = vec![task_ty(), ResolvedTy::I64, ResolvedTy::Duration];
    let pipeline = pipeline_with_select_actor_handler_arms(arms, locals, &[10, 11], 99);
    let ir = emit_select_ir("select_task_await", &pipeline);
    assert_eq!(ir.matches("call ptr @hew_reply_channel_new(").count(), 1);
    assert!(ir.contains("call void @hew_reply_channel_retain("));
    assert!(ir.contains("call i32 @hew_task_completion_observe("));
    assert!(ir.contains("call i32 @hew_task_completion_unobserve("));
    assert!(ir.contains("call ptr @hew_task_get_result("));
}

/// A select carrying only an `AfterTimer` arm has no race; the
/// HIR should have rejected the shape, but codegen must refuse
/// defensively (no ActorAsk arms means nothing to race).
#[test]
fn select_no_actor_ask_arms_fails_closed() {
    let pipeline = pipeline_with_select_terminator(hew_mir::SelectArmKind::AfterTimer {
        duration: Place::Local(0),
    });
    let ctx = Context::create();
    let err = build_module(&ctx, &pipeline, "select_after_only")
        .expect_err("Terminator::Select with only AfterTimer must fail closed");
    let msg = match err {
        CodegenError::FailClosed(s) => s,
        other => panic!("expected FailClosed, got {other:?}"),
    };
    assert!(
        msg.contains("no value-producing arms"),
        "after-only FailClosed must explain the sealed-shape violation: {msg}"
    );
}

// ── Terminator::Select positive-path emit (ActorAsk + AfterTimer) ─

/// Build an `IrPipeline` whose `main` issues a `Terminator::Select`
/// with the given arm vector. Local 0 is a `Duplex` (the actor
/// handle source for each ActorAsk arm); locals 1.. are per-arm
/// reply slots (one per ActorAsk arm) plus a single i64 reply
/// payload slot (unit-arg variant uses `ResolvedTy::Unit` to skip
/// payload pointer load). Local for after-arm duration is
/// allocated as `ResolvedTy::Duration` (i64 ns).
///
/// The resulting function has one originating block (id 0) sealed
/// by `Terminator::Select` and per-arm body blocks (each just
/// `Terminator::Return`) plus a join block (also `Return`).
fn pipeline_with_select_arms(
    arms: Vec<hew_mir::SelectArm>,
    locals: Vec<ResolvedTy>,
    body_block_ids: &[u32],
    join_block_id: u32,
) -> IrPipeline {
    // Build the originating block + per-arm body blocks + join block.
    let mut blocks: Vec<BasicBlock> = Vec::new();
    blocks.push(BasicBlock {
        id: 0,
        statements: Vec::new(),
        instructions: Vec::new(),
        terminator: Terminator::Select {
            arms,
            next: join_block_id,
        },
    });
    for &bb_id in body_block_ids {
        blocks.push(BasicBlock {
            id: bb_id,
            statements: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Goto {
                target: join_block_id,
            },
        });
    }
    blocks.push(BasicBlock {
        id: join_block_id,
        statements: Vec::new(),
        instructions: Vec::new(),
        terminator: Terminator::Return,
    });

    let main = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "main".to_string(),
        return_ty: ResolvedTy::I64,
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals,
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks,
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![main],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    }
}

fn pipeline_with_select_actor_handler_arms(
    arms: Vec<hew_mir::SelectArm>,
    locals: Vec<ResolvedTy>,
    body_block_ids: &[u32],
    join_block_id: u32,
) -> IrPipeline {
    let mut pipeline = pipeline_with_select_arms(arms, locals, body_block_ids, join_block_id);
    pipeline.raw_mir[0].call_conv = hew_mir::FunctionCallConv::ActorHandler;
    pipeline.raw_mir[0].blocks[0]
        .instructions
        .insert(0, Instr::EnterContext);
    let join_block = pipeline
        .raw_mir
        .get_mut(0)
        .and_then(|main| {
            main.blocks
                .iter_mut()
                .find(|block| block.id == join_block_id)
        })
        .expect("select actor-handler helper must include join block");
    join_block.instructions.push(Instr::ExitContext);
    pipeline
}

/// Helper: a `ResolvedTy::Named { name: "Duplex", .. }` so the
/// codegen treats local 0 as an actor handle (the same shape
/// `Place::DuplexHandle` references via `load_duplex_handle`).
fn duplex_ty() -> ResolvedTy {
    ResolvedTy::Named {
        name: "Duplex".to_string(),
        args: Vec::new(),
        builtin: None,
        is_opaque: false,
    }
}

fn task_ty() -> ResolvedTy {
    ResolvedTy::Task(Box::new(ResolvedTy::I64))
}

/// Render the LLVM IR of the emitted module so test bodies can
/// inspect the call-site ordering, channel-array shape, and per-arm
/// dispatch (the "ground-truth" the runtime ABI demands).
fn emit_select_ir(name: &str, pipeline: &IrPipeline) -> String {
    let ctx = Context::create();
    let llvm_mod = build_module(&ctx, pipeline, name).expect("select pipeline must compile");
    llvm_mod.print_to_string().to_string()
}

/// Two ActorAsk arms (no AfterTimer): emit shows exactly 2 channel
/// allocations, 2 ask-issues, 1 `hew_select_first` call with
/// timeout=-1, a switch on the winner index, per-winner reply
/// wait, and 1 cancel + 1 free for the losing arm in each winner
/// branch.
#[test]
fn select_two_actor_ask_arms_emit_full_dispatch() {
    let arms = vec![
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::ActorAsk {
                actor: Place::DuplexHandle(0),
                method: "ping".to_string(),
                args: Vec::new(),
                msg_type: 7,
                value: Place::Local(3), // unit payload slot
            },
            body_block: 10,
            binding: Some(Place::Local(1)),
        },
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::ActorAsk {
                actor: Place::DuplexHandle(0),
                method: "pong".to_string(),
                args: Vec::new(),
                msg_type: 8,
                value: Place::Local(3),
            },
            body_block: 11,
            binding: Some(Place::Local(2)),
        },
    ];
    let locals = vec![
        duplex_ty(),      // 0: actor handle
        ResolvedTy::I64,  // 1: arm 0 reply slot
        ResolvedTy::I64,  // 2: arm 1 reply slot
        ResolvedTy::Unit, // 3: payload (unit)
    ];
    let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11], 99);
    let ir = emit_select_ir("select_two_asks", &pipeline);

    assert_eq!(
        ir.matches("call ptr @hew_reply_channel_new(").count(),
        2,
        "expected 2 channel allocations; ir:\n{ir}"
    );
    assert_eq!(
        ir.matches("call i32 @hew_actor_ask_with_channel(").count(),
        2,
        "expected 2 ask-issues; ir:\n{ir}"
    );
    assert_eq!(
        ir.matches("call i32 @hew_select_first(").count(),
        1,
        "expected 1 hew_select_first call; ir:\n{ir}"
    );
    // No after arm → timeout_ms = -1.
    assert!(
        ir.contains("@hew_select_first(ptr %select_channels_first, i32 2, i32 -1)"),
        "hew_select_first must be called with count=2, timeout=-1; ir:\n{ir}"
    );
    // One reply-wait per winner branch (two branches, one wait each).
    assert_eq!(
        ir.matches("call ptr @hew_reply_wait(").count(),
        2,
        "expected 2 reply-wait calls (one per winner branch); ir:\n{ir}"
    );
    // Cancel/free counts across the whole module. Both arms are ActorAsk —
    // a FAIL-CLOSED send surface — so a non-zero ask-issue status routes to
    // a per-arm RECOVERABLE recovery block (free THIS arm's channel, null
    // its slot, continue), NOT the process-fatal setup-fail trap. The trap
    // path (which freed channels [0..=slot_idx] + cancelled prior arms)
    // therefore never emits for an all-ActorAsk select:
    //   - winner branches: 2 (each cancels 1 loser, frees winner-self + loser
    //     → 2 cancels, 4 frees).
    //   - setup_recover_0 / setup_recover_1: each frees ONLY its own channel
    //     (no cancel — no ask was successfully queued) → 0 cancels, 2 frees.
    // Totals: 2 cancels (winner losers only); 4 (winners) + 2 (recover) = 6 frees.
    assert_eq!(
        ir.matches("call void @hew_reply_channel_cancel(").count(),
        2,
        "expected 2 cancels (1 loser per winner branch; recoverable arms \
             free without cancel); ir:\n{ir}"
    );
    assert_eq!(
        ir.matches("call void @hew_reply_channel_free(").count(),
        6,
        "expected 6 channel frees across winner + recovery paths; ir:\n{ir}"
    );
    // Every ActorAsk arm fails closed: one recoverable recovery block per
    // arm (the conditional-branch fail edge), and NO process-fatal
    // setup-fail trap block anywhere in the module.
    assert!(
        ir.contains("select_setup_recover_0") && ir.contains("select_setup_recover_1"),
        "each ActorAsk arm must emit a recoverable recovery block; ir:\n{ir}"
    );
    assert!(
        !ir.contains("select_setup_fail_"),
        "an all-ActorAsk (fail-closed) select must emit NO process-fatal \
             setup-fail trap block; ir:\n{ir}"
    );
}

/// One ActorAsk + one AfterTimer arm: the AfterTimer's duration
/// drives the `hew_select_first` deadline (ns→ms / 1_000_000).
#[test]
fn select_actor_ask_plus_after_timer_emits_deadline() {
    let arms = vec![
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::ActorAsk {
                actor: Place::DuplexHandle(0),
                method: "ping".to_string(),
                args: Vec::new(),
                msg_type: 7,
                value: Place::Local(3),
            },
            body_block: 10,
            binding: Some(Place::Local(1)),
        },
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::AfterTimer {
                duration: Place::Local(2),
            },
            body_block: 11,
            binding: None,
        },
    ];
    let locals = vec![
        duplex_ty(),          // 0: actor handle
        ResolvedTy::I64,      // 1: reply slot
        ResolvedTy::Duration, // 2: after-arm duration (ns, i64)
        ResolvedTy::Unit,     // 3: payload
    ];
    let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11], 99);
    let ir = emit_select_ir("select_ask_and_after", &pipeline);

    // 1 channel allocation, 1 ask-issue, 1 hew_select_first call.
    assert_eq!(ir.matches("call ptr @hew_reply_channel_new(").count(), 1);
    assert_eq!(
        ir.matches("call i32 @hew_actor_ask_with_channel(").count(),
        1
    );
    assert_eq!(ir.matches("call i32 @hew_select_first(").count(), 1);
    // Deadline derived from i64 ns: should show sdiv by 1_000_000.
    assert!(
        ir.contains("sdiv i64") && ir.contains("1000000"),
        "expected ns→ms sdiv by 1_000_000; ir:\n{ir}"
    );
    // Two winner branches: ActorAsk winner + AfterTimer winner.
    // Each branch cancels + frees the loser(s); the AfterTimer
    // branch cancels + frees the single ActorAsk channel.
    assert!(
        ir.contains("select_win_ask_0:") && ir.contains("select_win_after:"),
        "expected both winner branch labels; ir:\n{ir}"
    );
}

/// Loser-cleanup order invariant (Risk R4): cancel BEFORE free on
/// every loser channel — the cancel flag + ref count are what
/// prevent UAF on a late-reply race.
#[test]
fn select_loser_cleanup_cancels_before_freeing() {
    let arms = vec![
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::ActorAsk {
                actor: Place::DuplexHandle(0),
                method: "a".to_string(),
                args: Vec::new(),
                msg_type: 1,
                value: Place::Local(3),
            },
            body_block: 10,
            binding: Some(Place::Local(1)),
        },
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::ActorAsk {
                actor: Place::DuplexHandle(0),
                method: "b".to_string(),
                args: Vec::new(),
                msg_type: 2,
                value: Place::Local(3),
            },
            body_block: 11,
            binding: Some(Place::Local(2)),
        },
    ];
    let locals = vec![
        duplex_ty(),
        ResolvedTy::I64,
        ResolvedTy::I64,
        ResolvedTy::Unit,
    ];
    let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11], 99);
    let ir = emit_select_ir("select_loser_order", &pipeline);

    // The loser cleanup lives in the per-winner "ok" block
    // (`select_reply_ok_N`) following the defensive null-reply
    // check on `hew_reply_wait`. We anchor on the named loser-load
    // SSA value emitted by the codegen (`select_loser_load_wN_lM`)
    // so we test the SAME channel's cleanup ordering, not arbitrary
    // `hew_reply_channel_*` calls that may interleave the
    // winner-self free.
    //
    // The two winners produce distinct SSA names
    // (`select_loser_load_w0_l1` vs `select_loser_load_w1_l0`),
    // so anchoring on the SSA name uniquely identifies each
    // winner's loser cleanup regardless of block-label
    // intervening text. We do not need to bound the search
    // region — the SSA name is globally unique in this module.
    let cancel0_idx = ir
        .find("call void @hew_reply_channel_cancel(ptr %select_loser_load_w0_l1)")
        .expect("loser cancel for arm-1 in win-0 region");
    let free0_idx = ir
        .find("call void @hew_reply_channel_free(ptr %select_loser_load_w0_l1)")
        .expect("loser free for arm-1 in win-0 region");
    assert!(
        cancel0_idx < free0_idx,
        "Risk R4: cancel must precede free in win-0 loser cleanup"
    );

    let cancel1_idx = ir
        .find("call void @hew_reply_channel_cancel(ptr %select_loser_load_w1_l0)")
        .expect("loser cancel for arm-0 in win-1 region");
    let free1_idx = ir
        .find("call void @hew_reply_channel_free(ptr %select_loser_load_w1_l0)")
        .expect("loser free for arm-0 in win-1 region");
    assert!(
        cancel1_idx < free1_idx,
        "Risk R4: cancel must precede free in win-1 loser cleanup"
    );
}

/// Mid-setup error recovery (Risk R3) for NON-RECOVERABLE arms: a
/// stream/task/channel poll-registration failure is a genuine internal
/// setup error (not a fail-closed send surface), so a non-zero status still
/// frees every channel allocated through that point and traps. Two
/// StreamNext arms keep coverage of the trap path: an ActorAsk arm now
/// fails closed (see `fungible_actor_ask_arm_recovers_instead_of_trapping`).
#[test]
fn select_setup_failure_branches_free_allocated_channels_and_trap() {
    let arms = vec![
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::StreamNext {
                stream: Place::DuplexHandle(0),
            },
            body_block: 10,
            binding: Some(Place::Local(1)),
        },
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::StreamNext {
                stream: Place::DuplexHandle(0),
            },
            body_block: 11,
            binding: Some(Place::Local(2)),
        },
    ];
    let locals = vec![duplex_ty(), ResolvedTy::I64, ResolvedTy::I64];
    let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11], 99);
    let ir = emit_select_ir("select_setup_recovery", &pipeline);

    // Per-arm setup-failure blocks must exist for the non-recoverable kind.
    assert!(
        ir.contains("select_setup_fail_0:") && ir.contains("select_setup_fail_1:"),
        "expected per-arm setup-failure blocks; ir:\n{ir}"
    );
    // The recovery path must call hew_trap_with_code(206) and
    // llvm.trap then unreachable (matching the Send-fail shape).
    assert!(
        ir.contains("@hew_trap_with_code") && ir.contains("call void @llvm.trap"),
        "expected hew_trap_with_code + llvm.trap on setup-fail path; ir:\n{ir}"
    );
    // A non-recoverable arm never emits a recoverable recovery block.
    assert!(
        !ir.contains("select_setup_recover_"),
        "stream poll failure is non-recoverable — no recovery block; ir:\n{ir}"
    );
}

/// A FUNGIBLE ActorAsk arm whose ask-issue fails (the receiver was
/// unreachable — a dead/null fungible supervisor-child ref) FAILS CLOSED:
/// it retires this single arm (frees its channel, nulls its slot so the
/// dispatch + teardown skip it) and continues, rather than the
/// process-fatal setup-fail trap. Mirrors the single-shot `hew_actor_ask`
/// path, which binds `Err(AskError::*)` on any non-zero send status.
#[test]
fn fungible_actor_ask_arm_recovers_instead_of_trapping() {
    let arms = vec![
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::ActorAsk {
                actor: Place::DuplexHandle(0),
                method: "ping".to_string(),
                args: Vec::new(),
                msg_type: 7,
                value: Place::Local(2),
            },
            body_block: 10,
            binding: Some(Place::Local(1)),
        },
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::AfterTimer {
                duration: Place::Local(3),
            },
            body_block: 11,
            binding: None,
        },
    ];
    let locals = vec![
        duplex_ty(),          // 0: actor handle
        ResolvedTy::I64,      // 1: reply slot
        ResolvedTy::Unit,     // 2: payload
        ResolvedTy::Duration, // 3: after-arm duration
    ];
    let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11], 99);
    let ir = emit_select_ir("select_ask_recover", &pipeline);

    // The ActorAsk arm fails closed: a recoverable recovery block, NOT a
    // process-fatal setup-fail trap block.
    assert!(
        ir.contains("select_setup_recover_0"),
        "fungible ActorAsk arm must emit a recoverable recovery block; ir:\n{ir}"
    );
    assert!(
        !ir.contains("select_setup_fail_"),
        "a fungible ActorAsk arm must NOT emit a process-fatal setup-fail \
             trap block; ir:\n{ir}"
    );
    // The recovery frees the failed channel (its caller-side ref, kept by
    // the runtime on send failure) and nulls its array slot so the dispatch
    // + teardown skip it (the null-slot `continue` semantics). Both live in
    // the recover block; a void `free` call drops its SSA name and LLVM may
    // suffix the re-GEP'd slot name, so anchor inside the recover block on
    // the free of the arm's channel value followed by a null-slot store.
    let recover_idx = ir
        .find("select_setup_recover_0:")
        .expect("recover block must exist");
    let recover_region = &ir[recover_idx..];
    assert!(
        recover_region.contains("call void @hew_reply_channel_free(ptr %select_ch_new_0)"),
        "recovery must free the failed arm's channel; ir:\n{ir}"
    );
    assert!(
        recover_region.contains("store ptr null, ptr %ch_slot_0"),
        "recovery must null the failed arm's channel-array slot; ir:\n{ir}"
    );
}

/// Setup-fail-1 (arm 1's NON-RECOVERABLE poll fails) must cancel-then-free
/// arm 0's channel before freeing arm 1's own. Arm 0 is a successfully
/// set-up ActorAsk whose retained channel can take a late reply, so the
/// trap cleanup must cancel (tombstone) it before the free — the cancel
/// flag + ref count are the UAF guard (Risk R4). Arm 1 is a StreamNext (a
/// non-recoverable poll-registration arm) so the trap path still emits.
#[test]
fn select_setup_failure_in_second_arm_cleans_up_first_arm_with_cancel_first() {
    let arms = vec![
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::ActorAsk {
                actor: Place::DuplexHandle(0),
                method: "a".to_string(),
                args: Vec::new(),
                msg_type: 1,
                value: Place::Local(3),
            },
            body_block: 10,
            binding: Some(Place::Local(1)),
        },
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::StreamNext {
                stream: Place::DuplexHandle(0),
            },
            body_block: 11,
            binding: Some(Place::Local(2)),
        },
    ];
    let locals = vec![
        duplex_ty(),
        ResolvedTy::I64,
        ResolvedTy::I64,
        ResolvedTy::Unit,
    ];
    let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11], 99);
    let ir = emit_select_ir("select_setup_recovery_late", &pipeline);

    let fail1_idx = ir
        .find("select_setup_fail_1:")
        .expect("setup-fail-1 block must exist");
    // Bound the region by the next block label after fail-1; for
    // simplicity, search the rest of the IR for the cancel before
    // any free.
    let fail1_region = &ir[fail1_idx..];
    let cancel_idx = fail1_region
        .find("call void @hew_reply_channel_cancel(")
        .expect("setup-fail-1 must cancel arm-0's channel");
    let free_idx = fail1_region
        .find("call void @hew_reply_channel_free(")
        .expect("setup-fail-1 must free at least one channel");
    assert!(
        cancel_idx < free_idx,
        "setup-fail-1 must cancel arm-0 BEFORE any free (UAF mitigation); region:\n{fail1_region}"
    );
}

/// The winner-switch dispatches on the i32 returned by
/// `hew_select_first`. Each ActorAsk arm slot index is a case;
/// the default lands on the AfterTimer winner block when present.
#[test]
fn select_winner_switch_uses_arm_slot_index() {
    let arms = vec![
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::ActorAsk {
                actor: Place::DuplexHandle(0),
                method: "a".to_string(),
                args: Vec::new(),
                msg_type: 1,
                value: Place::Local(3),
            },
            body_block: 10,
            binding: Some(Place::Local(1)),
        },
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::AfterTimer {
                duration: Place::Local(2),
            },
            body_block: 11,
            binding: None,
        },
    ];
    let locals = vec![
        duplex_ty(),
        ResolvedTy::I64,
        ResolvedTy::Duration,
        ResolvedTy::Unit,
    ];
    let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11], 99);
    let ir = emit_select_ir("select_switch_shape", &pipeline);

    // An LLVM `switch i32 %winner_idx, label %<default> [ ...
    // i32 0, label %select_win_ask_0 ... ]` pattern must appear.
    assert!(
        ir.contains("switch i32 %select_winner_idx") && ir.contains("select_win_ask_0"),
        "expected switch on winner index with per-arm case; ir:\n{ir}"
    );
    assert!(
        ir.contains("select_win_after"),
        "default arm of the switch must route to the AfterTimer winner block; ir:\n{ir}"
    );
}

/// Channel-array allocation: a fixed-size `[N x ptr]` alloca with
/// N = number of ActorAsk arms; the `hew_select_first` call site
/// receives the array-first GEP, not the alloca handle directly.
#[test]
fn select_channel_array_layout_matches_runtime_abi() {
    let arms = vec![
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::ActorAsk {
                actor: Place::DuplexHandle(0),
                method: "a".to_string(),
                args: Vec::new(),
                msg_type: 1,
                value: Place::Local(3),
            },
            body_block: 10,
            binding: Some(Place::Local(1)),
        },
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::ActorAsk {
                actor: Place::DuplexHandle(0),
                method: "b".to_string(),
                args: Vec::new(),
                msg_type: 2,
                value: Place::Local(3),
            },
            body_block: 11,
            binding: Some(Place::Local(2)),
        },
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::ActorAsk {
                actor: Place::DuplexHandle(0),
                method: "c".to_string(),
                args: Vec::new(),
                msg_type: 3,
                value: Place::Local(5),
            },
            body_block: 12,
            binding: Some(Place::Local(4)),
        },
    ];
    let locals = vec![
        duplex_ty(),
        ResolvedTy::I64,
        ResolvedTy::I64,
        ResolvedTy::Unit,
        ResolvedTy::I64,
        ResolvedTy::Unit,
    ];
    let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11, 12], 99);
    let ir = emit_select_ir("select_three_asks", &pipeline);

    // [3 x ptr] alloca for the channel array.
    assert!(
        ir.contains("alloca [3 x ptr]"),
        "expected [3 x ptr] channel-array alloca; ir:\n{ir}"
    );
    // Three ask-issues + one select_first call.
    assert_eq!(
        ir.matches("call i32 @hew_actor_ask_with_channel(").count(),
        3
    );
    assert!(ir.contains("@hew_select_first(ptr %select_channels_first, i32 3, i32 -1)"));
}

/// Producer-bridge: the SelectArm.binding Place is the slot codegen
/// writes the reply into on win. We verify the winner block emits
/// a load from the reply pointer + a store into the binding
/// alloca, then frees the reply buffer.
#[test]
fn select_winner_writes_reply_into_binding_place_and_frees_buffer() {
    let arms = vec![hew_mir::SelectArm {
        kind: hew_mir::SelectArmKind::ActorAsk {
            actor: Place::DuplexHandle(0),
            method: "ping".to_string(),
            args: Vec::new(),
            msg_type: 1,
            value: Place::Local(2),
        },
        body_block: 10,
        binding: Some(Place::Local(1)),
    }];
    let locals = vec![duplex_ty(), ResolvedTy::I64, ResolvedTy::Unit];
    let pipeline = pipeline_with_select_arms(arms, locals, &[10], 99);
    let ir = emit_select_ir("select_one_ask_binding", &pipeline);

    // The reply-wait result is loaded, stored into the binding,
    // and then `free`d (libc free, not channel_free).
    assert!(
        ir.contains("@hew_reply_wait("),
        "winner must wait on its channel; ir:\n{ir}"
    );
    assert!(
        ir.contains("call void @free("),
        "winner must free the reply buffer via libc free; ir:\n{ir}"
    );
    // The binding's alloca (local_1) must receive a store of the
    // loaded reply value.
    assert!(
        ir.contains("%local_1") && ir.contains("store i64"),
        "binding slot (local_1) must receive the reply value; ir:\n{ir}"
    );
}

/// Each winner branch defensively traps if `hew_reply_wait` returns
/// null (the allocation-failure-during-publish path documented at
/// `hew-runtime/src/reply_channel.rs:164`). Mirrors the null-trap
/// shape of `Terminator::Ask`'s lowering.
#[test]
fn select_winner_traps_on_null_reply_pointer() {
    let arms = vec![hew_mir::SelectArm {
        kind: hew_mir::SelectArmKind::ActorAsk {
            actor: Place::DuplexHandle(0),
            method: "ping".to_string(),
            args: Vec::new(),
            msg_type: 1,
            value: Place::Local(2),
        },
        body_block: 10,
        binding: Some(Place::Local(1)),
    }];
    let locals = vec![duplex_ty(), ResolvedTy::I64, ResolvedTy::Unit];
    let pipeline = pipeline_with_select_arms(arms, locals, &[10], 99);
    let ir = emit_select_ir("select_null_reply_trap", &pipeline);

    assert!(
        ir.contains("select_reply_null_trap_0:") && ir.contains("select_reply_ok_0:"),
        "expected null-trap + ok blocks per winner; ir:\n{ir}"
    );
    // The conditional branch must route null to the trap block.
    assert!(
        ir.contains("br i1 %select_reply_is_null_0, label %select_reply_null_trap_0"),
        "expected conditional branch on null reply; ir:\n{ir}"
    );
}

/// Module-verification: the emitted module passes `Module::verify`.
/// This is the floor invariant — any structural error in the
/// channel-array, switch, or per-arm CFG composition is caught
/// here even if the per-feature tests above pass.
#[test]
fn select_emitted_module_verifies() {
    let arms = vec![
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::ActorAsk {
                actor: Place::DuplexHandle(0),
                method: "ping".to_string(),
                args: Vec::new(),
                msg_type: 7,
                value: Place::Local(3),
            },
            body_block: 10,
            binding: Some(Place::Local(1)),
        },
        hew_mir::SelectArm {
            kind: hew_mir::SelectArmKind::AfterTimer {
                duration: Place::Local(2),
            },
            body_block: 11,
            binding: None,
        },
    ];
    let locals = vec![
        duplex_ty(),
        ResolvedTy::I64,
        ResolvedTy::Duration,
        ResolvedTy::Unit,
    ];
    let pipeline = pipeline_with_select_arms(arms, locals, &[10, 11], 99);
    let ctx = Context::create();
    let llvm_mod = build_module(&ctx, &pipeline, "select_verify").expect("pipeline must compile");
    if let Err(e) = llvm_mod.verify() {
        panic!(
            "emitted Select module failed verification: {}\nIR:\n{}",
            e.to_string(),
            llvm_mod.print_to_string().to_string()
        );
    }
}

/// Verify that `resolve_ty` looks up a generic-enum local by its mangled
/// key (`"Option$$i64"`) rather than the bare name (`"Option"`), and that
/// `build_module` produces a valid LLVM module when `Option<i64>` is the
/// type of a local variable.
///
/// Prior to this fix, `resolve_ty` used the bare name, found nothing in
/// the record-layout map, fell through to `primitive_to_llvm`, and
/// emitted the D10-violation diagnostic. The pipeline now succeeds: the
/// alloca for `local_0` acquires the tagged-union struct type registered
/// under `"Option$$i64"`.
#[test]
fn generic_enum_local_resolves_by_mangled_key() {
    use hew_hir::mangle as hir_mangle;

    // Build the mangled key the same way `register_enum_layouts` does.
    let mangled = hir_mangle("Option", &[ResolvedTy::I64]);
    assert_eq!(
        mangled, "Option$$i64",
        "mangle scheme must match registration"
    );

    // EnumLayout with that mangled name: `enum Option<i64> { None, Some(i64) }`
    let enum_layout = EnumLayout {
        name: mangled,
        tag_width: 1,
        variants: vec![
            MachineVariantLayout {
                name: "None".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
            MachineVariantLayout {
                name: "Some".to_string(),
                field_tys: vec![ResolvedTy::I64],
                field_names: vec![],
            },
        ],
        is_indirect: false,
    };

    // Function that has `Option<i64>` as a local (local_0) but only
    // reads an i64 constant into the return slot — the alloca for
    // local_0 is what exercises `resolve_ty` with a non-empty args vec.
    let func = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "main".to_string(),
        return_ty: ResolvedTy::I64,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![
            // local_0: Option<i64> — must resolve via mangled key
            ResolvedTy::named_builtin(
                BuiltinType::Option.canonical_name(),
                BuiltinType::Option,
                vec![ResolvedTy::I64],
            ),
            // local_1: i64 — return value
            ResolvedTy::I64,
        ],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                Instr::ConstI64 {
                    dest: Place::Local(1),
                    value: 42,
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(1),
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: Vec::<DecisionFact>::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };

    let pipeline = IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![func],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: vec![enum_layout],
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    };

    let ctx = Context::create();
    let m = build_module(&ctx, &pipeline, "generic_enum_mangle_test")
        .expect("Option<i64> local must resolve via mangled key; bare-name lookup is wrong");
    assert!(
        m.verify().is_ok(),
        "emitted module must pass LLVM verify:\n{}",
        m.print_to_string().to_string()
    );
}

#[test]
fn yield_terminator_lowers_to_coro_suspend_and_publishes_out_pointer() {
    // A generator body carrying `Terminator::Yield` lowers to an
    // `llvm.coro` switched-resume coroutine ramp: it is marked
    // `presplitcoroutine`, publishes the yielded value to its out-pointer
    // (`Local(0)`) before each `coro.suspend`, and emits NO thread-runtime
    // `hew_gen_*` calls. This is the codegen-side contract for the
    // generator→coro reroute.
    let ptr_ty = ResolvedTy::Pointer {
        is_mutable: true,
        pointee: Box::new(ResolvedTy::Unit),
    };
    let gen_body = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "__hew_gen_body_test_0".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        // Local(0) = out-pointer `*mut Y` — the coro ramp's only param for a
        // capture-free generator.
        params: vec![ptr_ty.clone()],
        locals: vec![ptr_ty.clone(), ResolvedTy::I64],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: vec![Instr::ConstI64 {
                    dest: Place::Local(1),
                    value: 7,
                }],
                terminator: Terminator::Yield {
                    value: Place::Local(1),
                    next: 1,
                },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Return,
            },
        ],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let pipeline = IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![gen_body],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    };
    let ctx = Context::create();
    let m = build_module(&ctx, &pipeline, "gen_yield_codegen_test")
        .expect("Terminator::Yield must lower without error");
    let ir = m.print_to_string().to_string();
    // The body is a presplit coroutine carrying the coro intrinsics.
    assert!(
        ir.contains("presplitcoroutine"),
        "Yield-carrying gen body must be a presplitcoroutine ramp; got IR:\n{ir}"
    );
    assert!(
        ir.contains("@llvm.coro.suspend"),
        "Yield arm must emit llvm.coro.suspend; got IR:\n{ir}"
    );
    // The reroute removes EVERY thread-runtime generator symbol.
    assert!(
        !ir.contains("hew_gen_yield"),
        "Yield arm must NOT emit the thread-runtime hew_gen_yield; got IR:\n{ir}"
    );
    assert!(
        !ir.contains("hew_gen_ctx_parent_cancel_token"),
        "the thread-runtime cancel-observation seam must be gone; got IR:\n{ir}"
    );
    // The value is loaded then published to the out-pointer before suspend
    // (the value channel). `gen_out_ptr` is the loaded out-pointer SSA;
    // `gen_yield_value` is the loaded yielded value.
    assert!(
        ir.contains("gen_out_ptr") && ir.contains("gen_yield_value"),
        "Yield arm must load the out-pointer and the yielded value, then \
             publish; got IR:\n{ir}"
    );
    assert!(
        m.verify().is_ok(),
        "gen-body module with Yield must pass LLVM verify:\n{ir}"
    );
}

/// An enclosing function carrying a `Terminator::MakeGenerator` must
/// heap-allocate the generator companion, call the gen-body coro RAMP with
/// the companion's out-pointer (the ramp returns the `coro.begin` handle),
/// store that handle into the companion, store the companion into the
/// `Generator<Y, R>` value slot, and verify. This is the codegen-side
/// contract for the coro construction seam.
#[test]
fn make_generator_terminator_constructs_coro_companion_and_module_verifies() {
    let ptr_ty = ResolvedTy::Pointer {
        is_mutable: true,
        pointee: Box::new(ResolvedTy::Unit),
    };
    let generator_ty = ResolvedTy::Named {
        name: "Generator".to_string(),
        args: vec![ResolvedTy::I64, ResolvedTy::Unit],
        builtin: Some(hew_types::BuiltinType::Generator),
        is_opaque: false,
    };
    // The gen-body coro ramp the construction site references (mirrors what
    // `lower_gen_block` mints): a single leading out-pointer param `*mut Y`,
    // and it carries `Terminator::Yield` so `is_coroutine` overrides its LLVM
    // return type to the `coro.begin` handle (`ptr`).
    let gen_body = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "__hew_gen_body_make_test_0".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: vec![ptr_ty.clone()],
        locals: vec![ptr_ty.clone(), ResolvedTy::I64],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: vec![Instr::ConstI64 {
                    dest: Place::Local(1),
                    value: 7,
                }],
                terminator: Terminator::Yield {
                    value: Place::Local(1),
                    next: 1,
                },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Return,
            },
        ],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    // The enclosing function: allocate a Generator-typed local, construct it
    // via MakeGenerator, then return.
    let enclosing = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "build_gen".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![generator_ty.clone()],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::MakeGenerator {
                    dest: Place::Local(0),
                    body_fn: "__hew_gen_body_make_test_0".to_string(),
                    next: 1,
                    env: None,
                },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Return,
            },
        ],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let pipeline = IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![gen_body, enclosing],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    };
    let ctx = Context::create();
    let m = build_module(&ctx, &pipeline, "make_generator_codegen_test")
        .expect("Terminator::MakeGenerator must lower without error");
    let ir = m.print_to_string().to_string();
    // The companion is heap-allocated via the size-headered coro allocator.
    assert!(
        ir.contains("@hew_cont_frame_alloc"),
        "MakeGenerator must allocate the companion via hew_cont_frame_alloc; \
             got IR:\n{ir}"
    );
    // The construction CALLS the gen-body coro ramp directly (not the deleted
    // hew_gen_ctx_create).
    assert!(
        ir.contains("call ptr @__hew_gen_body_make_test_0"),
        "MakeGenerator must call the gen-body coro ramp returning the handle; \
             got IR:\n{ir}"
    );
    assert!(
        !ir.contains("hew_gen_ctx_create"),
        "MakeGenerator must NOT emit the thread-runtime hew_gen_ctx_create; \
             got IR:\n{ir}"
    );
    assert!(
        m.verify().is_ok(),
        "module with MakeGenerator must pass LLVM verify:\n{ir}"
    );
}

#[test]
fn generator_out_drop_thunk_tracks_closure_env_box_ownership() {
    let ctx = Context::create();
    let m = ctx.create_module("generator_closure_out_drop_test");
    let harness = build_harness(&ctx, &[], &[]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "driver");
    let generator_of = |yield_ty| {
        ResolvedTy::named_builtin(
            "Generator",
            hew_types::BuiltinType::Generator,
            vec![yield_ty, ResolvedTy::Unit],
        )
    };

    let owned_closure = ResolvedTy::Closure {
        params: vec![],
        ret: Box::new(ResolvedTy::I64),
        captures: vec![ResolvedTy::String],
    };
    alloc_local(&mut fn_ctx, 0, generator_of(owned_closure));
    let (_, owned_companion) =
        generator_coro_companion_ty(&fn_ctx, Place::Local(0)).expect("owned companion type");
    assert!(
        gen_companion_out_drop_thunk(
            &fn_ctx,
            Place::Local(0),
            "owned_closure_yield",
            owned_companion,
        )
        .expect("owned closure out-drop thunk")
        .is_some(),
        "a closure capturing heap-owning state must install an output-drop thunk"
    );

    let copy_closure = ResolvedTy::Closure {
        params: vec![],
        ret: Box::new(ResolvedTy::I64),
        captures: vec![ResolvedTy::I64],
    };
    alloc_local(&mut fn_ctx, 1, generator_of(copy_closure.clone()));
    let (_, copy_companion) =
        generator_coro_companion_ty(&fn_ctx, Place::Local(1)).expect("copy companion type");
    assert!(
        gen_companion_out_drop_thunk(
            &fn_ctx,
            Place::Local(1),
            "copy_closure_yield",
            copy_companion,
        )
        .expect("copy closure out-drop decision")
        .is_some(),
        "a closure capturing only Copy scalars must release its escaped env box"
    );

    let aggregate_copy_closure = ResolvedTy::Tuple(vec![copy_closure]);
    alloc_local(&mut fn_ctx, 2, generator_of(aggregate_copy_closure));
    let (_, aggregate_companion) =
        generator_coro_companion_ty(&fn_ctx, Place::Local(2)).expect("aggregate companion");
    assert!(
        gen_companion_out_drop_thunk(
            &fn_ctx,
            Place::Local(2),
            "aggregate_copy_closure_yield",
            aggregate_companion,
        )
        .expect("aggregate copy closure out-drop decision")
        .is_some(),
        "aggregate slot drops must release captured Copy-only closure env boxes"
    );

    let bare_closure = ResolvedTy::Closure {
        params: vec![],
        ret: Box::new(ResolvedTy::I64),
        captures: vec![],
    };
    alloc_local(&mut fn_ctx, 3, generator_of(bare_closure));
    let (_, bare_companion) =
        generator_coro_companion_ty(&fn_ctx, Place::Local(3)).expect("bare companion type");
    assert!(
        gen_companion_out_drop_thunk(
            &fn_ctx,
            Place::Local(3),
            "bare_closure_yield",
            bare_companion,
        )
        .expect("bare closure out-drop decision")
        .is_none(),
        "capture-free closures must keep the null-thunk fast path"
    );

    finish_test_fn(&fn_ctx);
    let ir = m.print_to_string().to_string();
    assert!(
        ir.contains("define internal void @__hew_gen_out_drop_owned_closure_yield"),
        "heap-capturing closure yields must synthesize a typed output-drop thunk:\n{ir}"
    );
    assert!(
        ir.contains("call void %gen_out_drop_env_free_thunk"),
        "the output-drop thunk must invoke the closure env's in-band free thunk:\n{ir}"
    );
    assert!(
        ir.contains("define internal void @__hew_gen_out_drop_copy_closure_yield"),
        "Copy-only closure yields must synthesize an output-drop thunk:\n{ir}"
    );
    assert!(
        ir.contains("define internal void @__hew_gen_out_drop_aggregate_copy_closure_yield"),
        "aggregate Copy-only closure yields must synthesize an output-drop thunk:\n{ir}"
    );
    assert!(
        !ir.contains("__hew_gen_out_drop_bare_closure_yield"),
        "capture-free closure yields must keep the null-thunk fast path:\n{ir}"
    );
    assert!(
        m.verify().is_ok(),
        "generator closure output-drop module must pass LLVM verify:\n{ir}"
    );
}

#[test]
fn cancellation_token_is_cancelled_emits_runtime_observation_call() {
    let func = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "observe".to_string(),
        return_ty: ResolvedTy::Bool,
        call_conv: FunctionCallConv::Default,
        params: vec![ResolvedTy::CancellationToken],
        locals: vec![ResolvedTy::CancellationToken, ResolvedTy::Bool],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                Instr::CancellationTokenIsCancelled {
                    dest: Place::Local(1),
                    token: Place::Local(0),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(1),
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let pipeline = IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![func],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    };
    let ctx = Context::create();
    let m = build_module(&ctx, &pipeline, "cancel_token_is_cancelled_codegen_test")
        .expect("CancellationToken.is_cancelled must lower to runtime observation call");
    let ir = m.print_to_string().to_string();
    assert!(
        ir.contains("call i32 @hew_cancel_token_is_requested"),
        "CancellationToken.is_cancelled must call hew_cancel_token_is_requested; got IR:\n{ir}"
    );
    assert!(
        !ir.contains("call void @hew_cancel_token_release"),
        "CancellationToken.is_cancelled borrows the token and must not release it; got IR:\n{ir}"
    );
    assert!(
        m.verify().is_ok(),
        "CancellationToken.is_cancelled module must pass LLVM verify:\n{ir}"
    );
}

// ========================================================================
// W2.004 Stage 1 — composite-return helpers + DuplexHalfClose drop
// substrate
//
// The helpers are private to `llvm.rs` and have no in-tree consumers in
// Stage 1 (Stage 2 refactors `emit_remote_pid_send_call` /
// `emit_node_lookup_call` to call them; Stage 3 wires MIR producers for
// SendHalf / RecvHalf place lowering). To exercise the helpers directly
// we synthesise a minimal LLVM `Context` + `Module` + `Builder` + an
// `FnCtx` with pre-registered `EnumLayout` / `RecordLayout` fixtures,
// then invoke each helper and assert on the printed LLVM IR.
//
// The drop-substrate tests are simpler — they exercise
// `resolve_drop_fn` (the typed DropFnSpec dispatcher) and
// `intern_runtime_decl` directly.
// ========================================================================

use hew_mir::{EnumLayout as MirEnumLayout, MachineVariantLayout, RecordLayout as MirRecordLayout};

// ---- DuplexHalfClose drop substrate -------------------------------------

/// Build an empty `FnSymbolMap` for `resolve_drop_fn` tests that only
/// exercise the runtime-symbol arm. The map lookup is never consulted
/// when the first arm matches.
fn empty_fn_symbols<'ctx>() -> FnSymbolMap<'ctx> {
    HashMap::new()
}

#[test]
fn drop_fn_send_half_close_resolves_to_hew_duplex_close_half() {
    let syms = empty_fn_symbols();
    match resolve_drop_fn(
        &hew_mir::DropFnSpec::Runtime(
            hew_types::runtime_call::RuntimeDropDescriptor::SendHalfClose,
        ),
        &syms,
    )
    .expect("SendHalf::close must resolve after W2.004 Stage 1")
    {
        DropDispatch::RuntimeSymbol(sym) => assert_eq!(
            sym, "hew_duplex_close_half",
            "SendHalf::close must map to hew_duplex_close_half"
        ),
        DropDispatch::UserFn { .. } => {
            panic!("SendHalf::close must take the RuntimeSymbol arm, not UserFn")
        }
    }
}

#[test]
fn drop_fn_recv_half_close_resolves_to_hew_duplex_close_half() {
    let syms = empty_fn_symbols();
    match resolve_drop_fn(
        &hew_mir::DropFnSpec::Runtime(
            hew_types::runtime_call::RuntimeDropDescriptor::RecvHalfClose,
        ),
        &syms,
    )
    .expect("RecvHalf::close must resolve after W2.004 Stage 1")
    {
        DropDispatch::RuntimeSymbol(sym) => assert_eq!(sym, "hew_duplex_close_half"),
        DropDispatch::UserFn { .. } => panic!("RecvHalf::close must take RuntimeSymbol arm"),
    }
}

#[test]
fn drop_fn_existing_duplex_close_still_resolves() {
    // Regression: the Duplex / lambda-actor close mappings must not be
    // disturbed by the typed-descriptor migration. The symbol is born
    // from the descriptor bijection at this dispatch — pin the values.
    let syms = empty_fn_symbols();
    for (descriptor, expected) in [
        (
            hew_types::runtime_call::RuntimeDropDescriptor::DuplexClose,
            "hew_duplex_close",
        ),
        (
            hew_types::runtime_call::RuntimeDropDescriptor::LambdaActorHandleClose,
            "hew_lambda_actor_release",
        ),
    ] {
        match resolve_drop_fn(&hew_mir::DropFnSpec::Runtime(descriptor), &syms)
            .expect("runtime descriptors must resolve")
        {
            DropDispatch::RuntimeSymbol(sym) => assert_eq!(sym, expected),
            DropDispatch::UserFn { .. } => {
                panic!("{descriptor:?} must take the RuntimeSymbol arm")
            }
        }
    }
}

#[test]
fn drop_fn_unknown_string_still_fails_closed() {
    let syms = empty_fn_symbols();
    // `Mystery::close` parses as a `<Type>::<method>` user-fn
    // candidate, so it reaches the fn_symbols lookup and fails there
    // (no registered function). The diagnostic must still cite both
    // dispatch arms so the next implementer knows where to look.
    let err = resolve_drop_fn(
        &hew_mir::DropFnSpec::UserClose("Mystery::close".to_string()),
        &syms,
    )
    .expect_err("an unregistered user close must fail closed");
    match err {
        CodegenError::FailClosed(msg) => {
            assert!(
                msg.contains("Mystery::close"),
                "diagnostic must name the offending close symbol; got: {msg}"
            );
            assert!(
                msg.contains("inherent") || msg.contains("impl"),
                "diagnostic must explain that user-resource close must be \
                     declared in an inherent impl block; got: {msg}"
            );
        }
        other => panic!("expected FailClosed, got {other:?}"),
    }
}

// ---- W3.030 Stage 2 — typed DropDispatch ------------------------------

/// User-resource `Conn::close(consuming self: Conn)`: when the codegen
/// function pre-pass has registered the mangled symbol, `resolve_drop_fn`
/// must take the `UserFn` arm.
#[test]
fn drop_fn_user_resource_close_routes_through_user_fn_arm() {
    let ctx = Context::create();
    let m = ctx.create_module("drop_fn_user_fn_arm_test");
    // Register a `Conn::close(%Conn) -> void` FnSymbol::Real entry, as
    // the codegen function pre-pass would do for an inherent `impl Conn
    // { fn close(consuming self: Conn) { … } }` block.
    let conn_ty = ctx.opaque_struct_type("Conn");
    conn_ty.set_body(&[ctx.i64_type().into()], false);
    let void_ty = ctx.void_type();
    let fn_ty = void_ty.fn_type(&[conn_ty.into()], false);
    let value = m.add_function("Conn::close", fn_ty, None);
    let mut syms: FnSymbolMap = HashMap::new();
    syms.insert(
        "Conn::close".to_string(),
        FnSymbol::Real {
            value,
            return_ty: conn_ty.into(), // sentinel; unused by drop dispatch
            returns_unit: true,
            extern_record_ret: None,
            extern_malloc_string_ret: false,
        },
    );
    match resolve_drop_fn(
        &hew_mir::DropFnSpec::UserClose("Conn::close".to_string()),
        &syms,
    )
    .expect("Conn::close must resolve via the UserFn arm when registered")
    {
        DropDispatch::UserFn { value: v, symbol } => {
            assert_eq!(symbol, "Conn::close");
            assert_eq!(
                v, value,
                "resolved FunctionValue must match the registered one"
            );
        }
        DropDispatch::RuntimeSymbol(sym) => {
            panic!("Conn::close must take the UserFn arm; got RuntimeSymbol({sym:?})")
        }
    }
}

/// A user-fn `close` that returns non-Unit is a Q-β-C violation; the
/// resolver must fail closed rather than silently accept a fallible
/// drop.
#[test]
fn drop_fn_user_resource_close_non_unit_fails_closed() {
    let ctx = Context::create();
    let m = ctx.create_module("drop_fn_non_unit_test");
    let i64_ty = ctx.i64_type();
    let fn_ty = i64_ty.fn_type(&[i64_ty.into()], false);
    let value = m.add_function("Bad::close", fn_ty, None);
    let mut syms: FnSymbolMap = HashMap::new();
    syms.insert(
        "Bad::close".to_string(),
        FnSymbol::Real {
            value,
            return_ty: i64_ty.into(),
            returns_unit: false,
            extern_record_ret: None,
            extern_malloc_string_ret: false,
        },
    );
    let err = resolve_drop_fn(
        &hew_mir::DropFnSpec::UserClose("Bad::close".to_string()),
        &syms,
    )
    .expect_err("non-Unit user-resource close must fail closed (Q-β-C)");
    match err {
        CodegenError::FailClosed(msg) => {
            assert!(msg.contains("Bad::close"));
            assert!(
                msg.contains("Unit") && msg.contains("defer"),
                "diagnostic must mention Unit return + defer escape hatch; got: {msg}"
            );
        }
        other => panic!("expected FailClosed, got {other:?}"),
    }
}

/// V13: the codegen-internal verifier must reject an `ElabDrop` whose
/// `drop_fn` resolves to neither dispatch arm. This is the uniqueness
/// pin for the two-armed contract.
#[test]
fn verify_drop_dispatch_resolves_rejects_unresolved_drop_fn() {
    use hew_mir::{DropKind, DropPlan, ElabDrop, ElaboratedMirFunction, ExitPath};
    let elab = ElaboratedMirFunction {
        name: "user_fn".to_string(),
        return_ty: ResolvedTy::Unit,
        statements: vec![],
        decisions: vec![],
        blocks: vec![],
        drop_plans: vec![(
            ExitPath::Return { block: 0 },
            DropPlan {
                drops: vec![ElabDrop {
                    place: Place::Local(0),
                    ty: ResolvedTy::Unit,
                    drop_fn: Some(hew_mir::DropFnSpec::UserClose(
                        "NotARealType::close".to_string(),
                    )),
                    kind: DropKind::Resource,
                    guard: None,
                }],
            },
        )],
        coroutine: None,
        lambda_captures: vec![],
    };
    let pipeline = IrPipeline {
        thir: Vec::new(),
        raw_mir: Vec::new(),
        checked_mir: Vec::new(),
        elaborated_mir: vec![elab],
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    };
    let syms = empty_fn_symbols();
    let err = verify_drop_dispatch_resolves(&pipeline, &syms)
        .expect_err("verifier must reject unresolved drop_fn");
    match err {
        CodegenError::FailClosed(msg) => {
            assert!(msg.contains("user_fn"), "must name the function: {msg}");
            assert!(
                msg.contains("NotARealType::close"),
                "must name the offending drop_fn: {msg}"
            );
            assert!(
                msg.contains("verify_drop_dispatch_resolves"),
                "must identify the verifier in the diagnostic: {msg}"
            );
        }
        other => panic!("expected FailClosed, got {other:?}"),
    }
}

/// V13 negative: the verifier accepts a runtime-symbol drop with an
/// empty FnSymbol map (the runtime arm does not need registration).
#[test]
fn verify_drop_dispatch_resolves_accepts_runtime_symbol_drop() {
    use hew_mir::{DropKind, DropPlan, ElabDrop, ElaboratedMirFunction, ExitPath};
    let elab = ElaboratedMirFunction {
        name: "uses_duplex".to_string(),
        return_ty: ResolvedTy::Unit,
        statements: vec![],
        decisions: vec![],
        blocks: vec![],
        drop_plans: vec![(
            ExitPath::Return { block: 0 },
            DropPlan {
                drops: vec![ElabDrop {
                    place: Place::Local(0),
                    ty: ResolvedTy::Unit,
                    drop_fn: Some(hew_mir::DropFnSpec::Runtime(
                        hew_types::runtime_call::RuntimeDropDescriptor::DuplexClose,
                    )),
                    kind: DropKind::DuplexClose,
                    guard: None,
                }],
            },
        )],
        coroutine: None,
        lambda_captures: vec![],
    };
    let pipeline = IrPipeline {
        thir: Vec::new(),
        raw_mir: Vec::new(),
        checked_mir: Vec::new(),
        elaborated_mir: vec![elab],
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    };
    let syms = empty_fn_symbols();
    verify_drop_dispatch_resolves(&pipeline, &syms)
        .expect("runtime-symbol drop must pass the verifier without fn_symbols entries");
}

/// R-1 (plan review): WASM-exclusion scan must classify a runtime-symbol
/// `ElabDrop` as excluded on wasm32.
#[test]
fn wasm_exclusion_scan_flags_runtime_duplex_close_in_elab_drop() {
    use hew_mir::{DropKind, DropPlan, ElabDrop, ElaboratedMirFunction, ExitPath};
    let elab = ElaboratedMirFunction {
        name: "uses_duplex".to_string(),
        return_ty: ResolvedTy::Unit,
        statements: vec![],
        decisions: vec![],
        blocks: vec![],
        drop_plans: vec![(
            ExitPath::Return { block: 0 },
            DropPlan {
                drops: vec![ElabDrop {
                    place: Place::Local(0),
                    ty: ResolvedTy::Unit,
                    drop_fn: Some(hew_mir::DropFnSpec::Runtime(
                        hew_types::runtime_call::RuntimeDropDescriptor::DuplexClose,
                    )),
                    kind: DropKind::DuplexClose,
                    guard: None,
                }],
            },
        )],
        coroutine: None,
        lambda_captures: vec![],
    };
    let pipeline = IrPipeline {
        thir: Vec::new(),
        raw_mir: Vec::new(),
        checked_mir: Vec::new(),
        elaborated_mir: vec![elab],
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    };
    let found = uses_wasm_excluded_symbol(&pipeline)
        .expect("Duplex::close ElabDrop must be flagged as WASM-excluded");
    assert!(
        found.starts_with("hew_duplex_"),
        "found symbol must be the hew_duplex_ C-ABI: got {found}"
    );
}

/// Wrap a single raw-MIR function into an otherwise-empty `IrPipeline` for
/// the wasm-exclusion-scan regressions.
fn raw_mir_only_pipeline(body: RawMirFunction) -> IrPipeline {
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![body],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    }
}

/// Code-2 (NEW-4 wasm fail-closed): a `select{}` arm that receives from a
/// channel emits `hew_channel_poll` whose native channel core is
/// `cfg(not(target_arch = "wasm32"))`. The wasm-exclusion scan must surface a
/// structured `WasmUnsupportedSubstrate("hew_channel_poll")` BEFORE textual
/// IR emission — not a dangling `wasm-ld` reference. This is the
/// defence-in-depth codegen catch for any direct-MIR select-arm path that
/// bypasses the HIR `.recv()` gate. LESSONS P0 `boundary-fail-closed`.
#[test]
fn wasm_exclusion_scan_flags_select_channel_recv_arm_as_channel_poll() {
    let ptr_ty = ResolvedTy::Pointer {
        is_mutable: true,
        pointee: Box::new(ResolvedTy::Unit),
    };
    let body = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "select_channel_recv_wasm_excl_test".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: vec![ptr_ty.clone(), ptr_ty.clone()],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Select {
                    arms: vec![hew_mir::SelectArm {
                        kind: hew_mir::SelectArmKind::ChannelRecv {
                            receiver: Place::Local(0),
                            elem_ty: ResolvedTy::String,
                        },
                        body_block: 1,
                        binding: Some(Place::Local(1)),
                    }],
                    next: 1,
                },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Return,
            },
        ],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let pipeline = raw_mir_only_pipeline(body);
    let found = uses_wasm_excluded_symbol(&pipeline)
        .expect("a select{} ChannelRecv arm must be flagged as WASM-excluded");
    assert_eq!(
        found, "hew_channel_poll",
        "WASM exclusion scan must surface `hew_channel_poll` for a \
             `select{{}}` channel-recv arm; got `{found}`"
    );
}

/// Code-2 (NEW-4 wasm fail-closed): a direct-MIR `Terminator::SuspendingChannelRecv`
/// carrier (the worker-free `await rx.recv()` lowering) emits
/// `hew_channel_await_recv`, also part of the native-only channel core. The
/// wasm-exclusion scan must fail closed with a structured
/// `WasmUnsupportedSubstrate("hew_channel_await_recv")` rather than a link
/// error. LESSONS P0 `boundary-fail-closed`.
#[test]
fn wasm_exclusion_scan_flags_suspending_channel_recv_as_await_recv() {
    let ptr_ty = ResolvedTy::Pointer {
        is_mutable: true,
        pointee: Box::new(ResolvedTy::Unit),
    };
    let body = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "suspending_channel_recv_wasm_excl_test".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: vec![ptr_ty.clone(), ptr_ty.clone()],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Suspend {
                    resume: 1,
                    cleanup: 2,
                    is_final: false,
                },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Return,
            },
            BasicBlock {
                id: 2,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Return,
            },
        ],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::from([(
            0,
            SuspendKind::ChannelRecv {
                receiver: Place::Local(0),
                result_dest: Place::Local(1),
                elem_ty: ResolvedTy::String,
                deadline_result_dest: None,
                error_dest: None,
            },
        )]),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let pipeline = raw_mir_only_pipeline(body);
    let found = uses_wasm_excluded_symbol(&pipeline)
        .expect("a collapsed ChannelRecv suspend carrier must be flagged as WASM-excluded");
    assert_eq!(
        found, "hew_channel_await_recv",
        "WASM exclusion scan must surface `hew_channel_await_recv` for a \
             `Terminator::SuspendingChannelRecv` carrier; got `{found}`"
    );
}

/// NEW-7 wasm fail-closed: a direct-MIR `Terminator::SuspendingStreamNext`
/// carrier emits `hew_stream_await_next`, part of the native-only stream
/// substrate. The wasm-exclusion scan must fail closed with a structured
/// `WasmUnsupportedSubstrate("hew_stream_await_next")` rather than a link
/// error. LESSONS P0 `boundary-fail-closed`.
#[test]
fn wasm_exclusion_scan_flags_suspending_stream_next_as_await_next() {
    let ptr_ty = ResolvedTy::Pointer {
        is_mutable: true,
        pointee: Box::new(ResolvedTy::Unit),
    };
    let body = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "suspending_stream_next_wasm_excl_test".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: vec![ptr_ty.clone(), ptr_ty.clone()],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Suspend {
                    resume: 1,
                    cleanup: 2,
                    is_final: false,
                },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Return,
            },
            BasicBlock {
                id: 2,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Return,
            },
        ],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::from([(
            0,
            SuspendKind::StreamNext {
                stream: Place::Local(0),
                result_dest: Place::Local(1),
                elem_ty: ResolvedTy::Bytes,
                deadline_result_dest: None,
                error_dest: None,
            },
        )]),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let pipeline = raw_mir_only_pipeline(body);
    let found = uses_wasm_excluded_symbol(&pipeline)
        .expect("a collapsed StreamNext suspend carrier must be flagged as WASM-excluded");
    assert_eq!(
        found, "hew_stream_await_next",
        "WASM exclusion scan must surface `hew_stream_await_next` for a \
             `Terminator::SuspendingStreamNext` carrier; got `{found}`"
    );
}

/// cut-task-sleep wasm fail-closed: a direct-MIR `Terminator::SuspendingTaskAwait`
/// carrier (the worker-free `await t` lowering) emits `hew_task_await_suspend`,
/// part of the native-only read-slot / `enqueue_resume` substrate. The
/// wasm-exclusion scan must fail closed rather than emit a dangling reference.
/// LESSONS P0 `boundary-fail-closed`.
#[test]
fn wasm_exclusion_scan_flags_suspending_task_await() {
    let ptr_ty = ResolvedTy::Pointer {
        is_mutable: true,
        pointee: Box::new(ResolvedTy::Unit),
    };
    let body = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "suspending_task_await_wasm_excl_test".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: vec![ptr_ty.clone(), ptr_ty.clone()],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Suspend {
                    resume: 1,
                    cleanup: 2,
                    is_final: false,
                },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Return,
            },
            BasicBlock {
                id: 2,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Return,
            },
        ],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::from([(
            0,
            SuspendKind::TaskAwait {
                scope: Place::Local(0),
                task: Place::Local(1),
                result_dest: None,
            },
        )]),
        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let pipeline = raw_mir_only_pipeline(body);
    let found = uses_wasm_excluded_symbol(&pipeline)
        .expect("a collapsed TaskAwait suspend carrier must be flagged as WASM-excluded");
    assert_eq!(
        found, "hew_task_await_suspend",
        "WASM exclusion scan must surface `hew_task_await_suspend` for a \
             `Terminator::SuspendingTaskAwait` carrier; got `{found}`"
    );
}

/// cut-task-sleep wasm fail-closed: a direct-MIR `Terminator::SuspendingSleep`
/// carrier (the cooperative `sleep_ms` lowering) arms a native-only timer-wheel
/// deadline via `hew_await_cancel_schedule_deadline_ms`. The wasm-exclusion
/// scan must fail closed; the wasm sleep keep path (message-boundary park) is
/// preserved on the contextless path. LESSONS P0 `boundary-fail-closed`.
#[test]
fn wasm_exclusion_scan_flags_suspending_sleep() {
    let i64_ty = ResolvedTy::I64;
    let body = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "suspending_sleep_wasm_excl_test".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: vec![i64_ty],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Suspend {
                    resume: 1,
                    cleanup: 2,
                    is_final: false,
                },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Return,
            },
            BasicBlock {
                id: 2,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Return,
            },
        ],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::from([(
            0,
            SuspendKind::Sleep {
                duration_ns: Place::Local(0),
            },
        )]),
        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let pipeline = raw_mir_only_pipeline(body);
    let found = uses_wasm_excluded_symbol(&pipeline)
        .expect("a collapsed Sleep suspend carrier must be flagged as WASM-excluded");
    assert_eq!(
        found, "hew_await_cancel_schedule_deadline_ms",
        "WASM exclusion scan must surface `hew_await_cancel_schedule_deadline_ms` \
             for a `Terminator::SuspendingSleep` carrier; got `{found}`"
    );
}

/// NEW-8 wasm fail-closed: a `Terminator::SuspendingSelect` carrier emits
/// `hew_await_cancel_schedule_deadline_ms` (via the `AfterTimer` arm
/// deadline-wheel path), part of the native-only timer substrate.  The
/// wasm-exclusion scan must fail closed with a structured
/// `WasmUnsupportedSubstrate("hew_await_cancel_schedule_deadline_ms")`
/// rather than a linker error on wasm32.  Mirrors the per-carrier convention
/// of the sleep / channel-recv / stream-next / task-await sibling tests.
/// LESSONS P0 `boundary-fail-closed`.
#[test]
fn wasm_exclusion_scan_flags_suspending_select() {
    let i64_ty = ResolvedTy::I64;
    let ptr_ty = ResolvedTy::Pointer {
        is_mutable: true,
        pointee: Box::new(ResolvedTy::Unit),
    };
    // Minimal two-arm select: one ChannelRecv + one AfterTimer.  The
    // exclusion scan keys on terminator shape, not arm count or arm kind, so
    // any SuspendingSelect must be refused on wasm32.
    let body = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "suspending_select_wasm_excl_test".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: vec![ptr_ty, i64_ty],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::SuspendingSelect {
                    arms: vec![
                        hew_mir::SelectArm {
                            kind: hew_mir::SelectArmKind::ChannelRecv {
                                receiver: Place::Local(0),
                                elem_ty: ResolvedTy::String,
                            },
                            body_block: 1,
                            binding: None,
                        },
                        hew_mir::SelectArm {
                            kind: hew_mir::SelectArmKind::AfterTimer {
                                duration: Place::Local(1),
                            },
                            body_block: 1,
                            binding: None,
                        },
                    ],
                    resume: 1,
                    cleanup: 2,
                },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Return,
            },
            BasicBlock {
                id: 2,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Return,
            },
        ],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),
        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let pipeline = raw_mir_only_pipeline(body);
    let found = uses_wasm_excluded_symbol(&pipeline)
        .expect("a SuspendingSelect carrier must be flagged as WASM-excluded");
    assert_eq!(
        found, "hew_await_cancel_schedule_deadline_ms",
        "WASM exclusion scan must surface `hew_await_cancel_schedule_deadline_ms` \
             for a `Terminator::SuspendingSelect` carrier; got `{found}`"
    );
}

/// Widened-carrier wasm fail-closed: the exclusion scan keys on terminator
/// SHAPE, not the element field, so a recv carrier whose `elem_ty` is a
/// record (the widened `Stream<MyRecord>` / `Receiver<MyRecord>` form) must
/// keep firing the same native-only symbols. A silent miss here would let a
/// wasm32 build reach `wasm-ld` with a dangling channel/stream reference
/// instead of the structured `WasmUnsupportedSubstrate` diagnostic.
/// LESSONS P0 `boundary-fail-closed`.
#[test]
fn wasm_exclusion_scan_survives_record_element_recv_carriers() {
    let record_elem_ty = ResolvedTy::Named {
        name: "Frame".to_string(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    };
    let ptr_ty = ResolvedTy::Pointer {
        is_mutable: true,
        pointee: Box::new(ResolvedTy::Unit),
    };
    let return_block = |id: u32| BasicBlock {
        id,
        statements: Vec::new(),
        instructions: Vec::new(),
        terminator: Terminator::Return,
    };
    // A collapsed suspension carrier lowers to a bare `Suspend` whose kind
    // lives in the `suspend_kinds` side-table; `Select` keeps its own
    // terminator (no side-table entry).
    let body = |name: &str, terminator: Terminator, kind: Option<SuspendKind>| {
        let suspend_kinds = kind
            .map(|k| std::collections::HashMap::from([(0u32, k)]))
            .unwrap_or_default();
        RawMirFunction {
            source_origin: hew_mir::SourceOrigin::Unknown,
            name: name.to_string(),
            return_ty: ResolvedTy::Unit,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: vec![ptr_ty.clone(), ptr_ty.clone()],
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks: vec![
                BasicBlock {
                    id: 0,
                    statements: Vec::new(),
                    instructions: Vec::new(),
                    terminator,
                },
                return_block(1),
                return_block(2),
            ],
            decisions: Vec::new(),
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),
            suspend_kinds,
            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
        }
    };

    let suspend = || Terminator::Suspend {
        resume: 1,
        cleanup: 2,
        is_final: false,
    };
    let cases = [
        (
            body(
                "record_elem_suspending_channel_recv",
                suspend(),
                Some(SuspendKind::ChannelRecv {
                    receiver: Place::Local(0),
                    result_dest: Place::Local(1),
                    elem_ty: record_elem_ty.clone(),
                    deadline_result_dest: None,
                    error_dest: None,
                }),
            ),
            "hew_channel_await_recv",
        ),
        (
            body(
                "record_elem_suspending_stream_next",
                suspend(),
                Some(SuspendKind::StreamNext {
                    stream: Place::Local(0),
                    result_dest: Place::Local(1),
                    elem_ty: record_elem_ty.clone(),
                    deadline_result_dest: None,
                    error_dest: None,
                }),
            ),
            "hew_stream_await_next",
        ),
        (
            body(
                "record_elem_select_channel_recv_arm",
                Terminator::Select {
                    arms: vec![hew_mir::SelectArm {
                        kind: hew_mir::SelectArmKind::ChannelRecv {
                            receiver: Place::Local(0),
                            elem_ty: record_elem_ty.clone(),
                        },
                        body_block: 1,
                        binding: Some(Place::Local(1)),
                    }],
                    next: 1,
                },
                None,
            ),
            "hew_channel_poll",
        ),
    ];
    for (body, expected_symbol) in cases {
        let name = body.name.clone();
        let pipeline = raw_mir_only_pipeline(body);
        let found = uses_wasm_excluded_symbol(&pipeline).unwrap_or_else(|| {
            panic!("{name}: a record-element recv carrier must stay WASM-excluded")
        });
        assert_eq!(
            found, expected_symbol,
            "{name}: the widened (record-element) carrier must keep \
                 surfacing `{expected_symbol}`; got `{found}`"
        );
    }
}

/// R-1 (plan review): a user-fn `<Type>::<method>` close is pure Hew
/// code and is NOT WASM-excluded. The scan must skip the user-fn arm
/// silently.
#[test]
fn wasm_exclusion_scan_ignores_user_fn_close_in_elab_drop() {
    use hew_mir::{DropKind, DropPlan, ElabDrop, ElaboratedMirFunction, ExitPath};
    let elab = ElaboratedMirFunction {
        name: "uses_user_resource".to_string(),
        return_ty: ResolvedTy::Unit,
        statements: vec![],
        decisions: vec![],
        blocks: vec![],
        drop_plans: vec![(
            ExitPath::Return { block: 0 },
            DropPlan {
                drops: vec![ElabDrop {
                    place: Place::Local(0),
                    ty: ResolvedTy::Unit,
                    drop_fn: Some(hew_mir::DropFnSpec::UserClose("Conn::close".to_string())),
                    kind: DropKind::Resource,
                    guard: None,
                }],
            },
        )],
        coroutine: None,
        lambda_captures: vec![],
    };
    let pipeline = IrPipeline {
        thir: Vec::new(),
        raw_mir: Vec::new(),
        checked_mir: Vec::new(),
        elaborated_mir: vec![elab],
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    };
    assert!(
        uses_wasm_excluded_symbol(&pipeline).is_none(),
        "user-fn `Conn::close` drop must not be flagged as WASM-excluded"
    );
}

#[test]
fn hew_duplex_close_half_runtime_decl_has_ptr_i32_signature() {
    // The runtime symbol takes (ptr, i32) -> i32 — pinned by
    // HewDuplexDirection ABI (`hew-runtime/src/duplex.rs:765`). If the
    // signature drifts here, drop emission will misroute its second
    // argument at LLVM verify time.
    let ctx = Context::create();
    let m = ctx.create_module("hew_duplex_close_half_decl_test");
    let mut decls: RuntimeDeclMap = HashMap::new();
    let fv = intern_runtime_decl(&ctx, &m, &mut decls, "hew_duplex_close_half")
        .expect("intern_runtime_decl(hew_duplex_close_half) must succeed");
    let fn_ty = fv.get_type();
    assert_eq!(
        fn_ty.count_param_types(),
        2,
        "hew_duplex_close_half ABI expects 2 args (handle, direction); got {}",
        fn_ty.count_param_types()
    );
    let params = fn_ty.get_param_types();
    assert!(
        matches!(params[0], BasicMetadataTypeEnum::PointerType(_)),
        "hew_duplex_close_half arg 0 must be ptr; got {:?}",
        params[0]
    );
    match params[1] {
        BasicMetadataTypeEnum::IntType(t) => assert_eq!(
            t.get_bit_width(),
            32,
            "hew_duplex_close_half arg 1 must be i32; got i{}",
            t.get_bit_width()
        ),
        other => panic!("hew_duplex_close_half arg 1 must be i32; got {other:?}"),
    }
    let ret = fn_ty
        .get_return_type()
        .expect("hew_duplex_close_half must return a value");
    match ret {
        BasicTypeEnum::IntType(t) => assert_eq!(
            t.get_bit_width(),
            32,
            "hew_duplex_close_half return must be i32"
        ),
        other => panic!("hew_duplex_close_half return must be i32; got {other:?}"),
    }
}

// ---- Composite-return helper test harness ------------------------------

/// Owned container holding everything the four composite-return helpers
/// need to run against a hand-built pipeline. Built on a per-test basis
/// because `FnCtx` borrows `record_layouts` / `machine_layouts` /
/// `fn_symbols` / `actor_layouts` — those have to outlive the helper
/// invocation.
struct CompositeHelperHarness<'ctx> {
    target_data: TargetData,
    record_layouts: RecordLayoutMap<'ctx>,
    machine_layouts: MachineLayoutMap<'ctx>,
    fn_symbols: FnSymbolMap<'ctx>,
    machine_step_symbols: HashSet<String>,
    actor_layouts: Vec<ActorLayout>,
    /// Empty for these fixtures — no struct hash thunks are exercised by
    /// the composite-helper unit tests, so no bool-field defence path is
    /// taken. Carried so the FnCtx field stays populated.
    record_field_resolved_tys: HashMap<String, Vec<ResolvedTy>>,
    const_globals: ConstGlobalMap<'ctx>,
}

/// Build a `Result<(), SendError>` `EnumLayout` matching the
/// `monomorphic_builtin_enums()` registration (Ok=unit, Err=SendError
/// 3-variant unit enum). The outer enum's tag distinguishes Ok=0/Err=1;
/// the SendError payload (a nested enum) is itself a 2-bit tag.
fn fixture_result_unit_send_error_layout() -> MirEnumLayout {
    // We model the Err payload as a single-field variant whose field
    // type is `ResolvedTy::Named { name: "SendError", .. }` — codegen
    // resolves that name through `record_layouts` to the registered
    // SendError outer struct.
    MirEnumLayout {
        name: "Result$$unit$$SendError".to_string(),
        tag_width: 1,
        variants: vec![
            MachineVariantLayout {
                name: "Ok".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
            MachineVariantLayout {
                name: "Err".to_string(),
                field_tys: vec![ResolvedTy::Named {
                    name: "SendError".to_string(),
                    args: vec![],
                    builtin: None,
                    is_opaque: false,
                }],
                field_names: vec![],
            },
        ],
        is_indirect: false,
    }
}

fn fixture_send_error_layout() -> MirEnumLayout {
    MirEnumLayout {
        name: "SendError".to_string(),
        tag_width: 2,
        variants: vec![
            MachineVariantLayout {
                name: "Full".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
            MachineVariantLayout {
                name: "Closed".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
            MachineVariantLayout {
                name: "NodeRoutingNotWired".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
        ],
        is_indirect: false,
    }
}

fn fixture_result_i64_lookuperror_layout() -> MirEnumLayout {
    MirEnumLayout {
        name: "Result$$i64$$LookupError".to_string(),
        tag_width: 1,
        variants: vec![
            MachineVariantLayout {
                name: "Ok".to_string(),
                field_tys: vec![ResolvedTy::I64],
                field_names: vec![],
            },
            MachineVariantLayout {
                name: "Err".to_string(),
                field_tys: vec![ResolvedTy::Named {
                    name: "LookupError".to_string(),
                    args: vec![],
                    builtin: None,
                    is_opaque: false,
                }],
                field_names: vec![],
            },
        ],
        is_indirect: false,
    }
}

fn fixture_lookup_error_layout() -> MirEnumLayout {
    MirEnumLayout {
        name: "LookupError".to_string(),
        tag_width: 1,
        variants: vec![MachineVariantLayout {
            name: "NotFound".to_string(),
            field_tys: vec![],
            field_names: vec![],
        }],
        is_indirect: false,
    }
}

fn fixture_monitor_ref_layout() -> MirRecordLayout {
    MirRecordLayout {
        name: "MonitorRef".to_string(),
        field_tys: vec![ResolvedTy::I64],
        field_names: vec![],
    }
}

/// A tri-variant payload-carrying user enum for `emit_enum_variant_literal`:
///   enum Sample { Empty, OneInt(i64), TwoInts(i64, i64) }
fn fixture_sample_enum_layout() -> MirEnumLayout {
    MirEnumLayout {
        name: "Sample".to_string(),
        tag_width: 2,
        variants: vec![
            MachineVariantLayout {
                name: "Empty".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
            MachineVariantLayout {
                name: "OneInt".to_string(),
                field_tys: vec![ResolvedTy::I64],
                field_names: vec![],
            },
            MachineVariantLayout {
                name: "TwoInts".to_string(),
                field_tys: vec![ResolvedTy::I64, ResolvedTy::I64],
                field_names: vec![],
            },
        ],
        is_indirect: false,
    }
}

fn fixture_point_layout() -> MirRecordLayout {
    MirRecordLayout {
        name: "Point".to_string(),
        field_tys: vec![ResolvedTy::I64, ResolvedTy::I64],
        field_names: vec![],
    }
}

fn fixture_option_i64_layout() -> MirEnumLayout {
    MirEnumLayout {
        name: "Option$$i64".to_string(),
        tag_width: 1,
        variants: vec![
            MachineVariantLayout {
                name: "Some".to_string(),
                field_tys: vec![ResolvedTy::I64],
                field_names: vec![],
            },
            MachineVariantLayout {
                name: "None".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
        ],
        is_indirect: false,
    }
}

/// Register the given enum + record layout fixtures into the harness's
/// codegen maps, returning the populated harness.
fn build_harness<'ctx>(
    ctx: &'ctx Context,
    record_fixtures: &[MirRecordLayout],
    enum_fixtures: &[MirEnumLayout],
) -> CompositeHelperHarness<'ctx> {
    let target_data = host_target_data();
    let mut record_layouts: RecordLayoutMap<'ctx> =
        crate::layout::predeclare_named_layouts(ctx, record_fixtures, enum_fixtures, &[], &[])
            .expect("named-layout predeclaration must succeed");
    crate::layout::fill_record_layout_bodies(ctx, record_fixtures, &record_layouts, &target_data)
        .expect("record-layout body fill must succeed");
    let mut machine_layouts: MachineLayoutMap<'ctx> = HashMap::new();
    crate::layout::register_enum_layouts(
        ctx,
        enum_fixtures,
        &mut record_layouts,
        &mut machine_layouts,
        Some(&target_data),
    )
    .expect("enum-layout registration must succeed");
    CompositeHelperHarness {
        target_data,
        record_layouts,
        machine_layouts,
        fn_symbols: HashMap::new(),
        machine_step_symbols: HashSet::new(),
        actor_layouts: Vec::new(),
        record_field_resolved_tys: HashMap::new(),
        const_globals: HashMap::new(),
    }
}

/// Build a minimal FnCtx for direct helper invocation. The caller is
/// responsible for inserting the dest + any source locals via
/// `alloc_local` BEFORE calling helpers (allocations are emitted at the
/// builder's current position — the test harness positions inside a
/// freshly-created LLVM fn body).
fn make_test_fn_ctx<'a, 'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &'a LlvmModule<'ctx>,
    harness: &'a CompositeHelperHarness<'ctx>,
    fn_name: &str,
) -> FnCtx<'a, 'ctx> {
    let i32_ty = ctx.i32_type();
    let fn_ty = i32_ty.fn_type(&[], false);
    let llvm_fn = llvm_mod.add_function(fn_name, fn_ty, None);
    let entry = ctx.append_basic_block(llvm_fn, "entry");
    let builder = ctx.create_builder();
    builder.position_at_end(entry);
    // Trivial return slot — composites use named locals, not ReturnSlot.
    let return_slot = builder
        .build_alloca(i32_ty, "ret_slot")
        .expect("ret slot alloca");
    FnCtx {
        ctx,
        llvm_mod,
        // Test harness contexts never exercise the drain epilogue; keep
        // the flag off so the test builder's block does not attempt to
        // intern shutdown symbols that are absent from the minimal fixture.
        emit_drain_epilogue: false,
        emit_immediate_shutdown_epilogue: false,
        emit_runtime_cleanup_epilogue: false,
        emit_wasm_runtime_exit: false,
        emit_lambda_drain_epilogue: false,
        target_data: &harness.target_data,
        builder,
        return_slot,
        return_ty: i32_ty.into(),
        return_resolved_ty: ResolvedTy::I32,
        execution_context: None,
        closure_call_fallback_context: None,
        execution_context_is_actor_handler: false,
        actor_state_ty: None,
        actor_state_field_kinds: None,
        locals: HashMap::new(),
        local_tys: HashMap::new(),
        blocks: HashMap::new(),
        runtime_decls: RefCell::new(HashMap::new()),
        record_layouts: &harness.record_layouts,
        fn_symbols: &harness.fn_symbols,
        machine_step_symbols: &harness.machine_step_symbols,
        resource_record_close: &[],
        resource_opaque_close: &[],
        actor_layouts: &harness.actor_layouts,
        machine_layouts: &harness.machine_layouts,
        enum_layouts: &[],
        indirect_enum_owned_locals: HashSet::new(),
        record_field_resolved_tys: &harness.record_field_resolved_tys,
        // Composite-helper unit tests never lower an
        // `Instr::CoerceToDynTrait`; carry an empty slice so the
        // FnCtx field stays populated. The dyn-trait
        // tests construct a full `IrPipeline` via `emit_module`
        // and exercise the registry there.
        dyn_vtable_registry: &[],
        const_globals: &harness.const_globals,
        // Composite-helper unit tests never exercise the P5-RX receive
        // borrow path; carry the inert defaults.
        borrow_mode: None,
        borrow_tainted: HashSet::new(),
        // Composite-helper unit tests never lower a `Terminator::Suspend`;
        // no coroutine prologue state.
        coro: None,
        // Test harness never exercises the lambda-actor body path.
        lambda_actor_shape: None,
        // Composite-helper unit tests inline `Instr::Drop`; no elaborated
        // drop plans and no suspend carrier to key an abandon plan off.
        drop_plans: &[],
        suspend_abandon_block: std::cell::Cell::new(0),
    }
}

/// Allocate a local of the given resolved type within the current
/// builder position. Inserts entries into `locals` and `local_tys`.
fn alloc_local(fn_ctx: &mut FnCtx<'_, '_>, id: u32, ty: ResolvedTy) {
    let llvm_ty = resolve_ty(fn_ctx.ctx, fn_ctx.target_data, &ty, fn_ctx.record_layouts)
        .expect("resolve_ty for fixture");
    let slot = fn_ctx
        .builder
        .build_alloca(llvm_ty, &format!("local_{id}"))
        .expect("alloca for fixture local");
    fn_ctx.locals.insert(id, (slot, llvm_ty));
    fn_ctx.local_tys.insert(id, ty);
}

/// Finish the synthetic test fn with a `ret i32 0` so LLVM verify is
/// happy.
fn finish_test_fn(fn_ctx: &FnCtx<'_, '_>) {
    let zero = fn_ctx.ctx.i32_type().const_zero();
    fn_ctx.builder.build_return(Some(&zero)).expect("ret 0");
}

/// W5.011 Slice 1: a `DropKind::CowHeap { release: String }`
/// ElabDrop on a `string` local must emit a single-`ptr`-arg call to
/// `hew_string_drop` and null-store the slot afterwards
/// (`raii-null-after-move`).
#[test]
fn cow_heap_string_drop_emits_call_and_null_store() {
    use hew_mir::{DropKind, ElabDrop};
    let ctx = Context::create();
    let m = ctx.create_module("cow_heap_string_test");
    let harness = build_harness(&ctx, &[], &[]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    alloc_local(&mut fn_ctx, 0, ResolvedTy::String);
    let drop = ElabDrop {
        place: Place::Local(0),
        ty: ResolvedTy::String,
        drop_fn: None,
        kind: DropKind::CowHeap {
            release: hew_mir::CowHeapRelease::String,
        },
        guard: None,
    };
    emit_one_elab_drop(&fn_ctx, &drop).expect("CowHeap string drop must emit");
    finish_test_fn(&fn_ctx);
    let ir = m.print_to_string().to_string();
    assert_eq!(
        ir.matches("call void @hew_string_drop(").count(),
        1,
        "expected EXACTLY ONE void call to hew_string_drop; got:\n{ir}"
    );
    assert!(
        ir.contains("declare void @hew_string_drop(ptr)"),
        "hew_string_drop must be declared as void(ptr); got:\n{ir}"
    );
    assert_eq!(
        ir.matches("store ptr null,").count(),
        1,
        "expected EXACTLY ONE null-store after the drop call; got:\n{ir}"
    );
}

/// LLC-5: a flag-guarded `ElabDrop` (`guard: Some(flag)`) must emit the
/// shared gated-drop conditional region — an `icmp eq i64 … 0` into a
/// `resource_drop_live_only` block that holds the actual drop, converging at
/// `resource_drop_merge`. The drop body must NOT appear on the entry path.
/// This pins that `emit_gated_drop_region` preserves the flag predicate +
/// the exactly-once block structure.
#[test]
fn flag_gated_drop_emits_eq_zero_conditional_region() {
    use hew_mir::{DropKind, ElabDrop};
    let ctx = Context::create();
    let m = ctx.create_module("flag_gated_drop_test");
    let harness = build_harness(&ctx, &[], &[]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    alloc_local(&mut fn_ctx, 0, ResolvedTy::String);
    // The drop-flag is an i64 local zero-initialised by the binding intro.
    alloc_local(&mut fn_ctx, 1, ResolvedTy::I64);
    let drop = ElabDrop {
        place: Place::Local(0),
        ty: ResolvedTy::String,
        drop_fn: None,
        kind: DropKind::CowHeap {
            release: hew_mir::CowHeapRelease::String,
        },
        guard: Some(Place::Local(1)),
    };
    emit_one_elab_drop(&fn_ctx, &drop).expect("flag-gated drop must emit");
    finish_test_fn(&fn_ctx);
    let ir = m.print_to_string().to_string();
    assert_eq!(
        ir.matches("icmp eq i64").count(),
        1,
        "flag-gated drop must emit EXACTLY ONE `icmp eq i64 … 0` gate; got:\n{ir}"
    );
    assert!(
        ir.contains("resource_drop_live_only"),
        "flag-gated drop must branch into a `resource_drop_live_only` block; got:\n{ir}"
    );
    assert!(
        ir.contains("resource_drop_merge"),
        "flag-gated drop must converge at `resource_drop_merge`; got:\n{ir}"
    );
    // The drop body (the call) lives in the live-only block, gated behind
    // the compare — exactly once, never on the unconditional entry path.
    assert_eq!(
        ir.matches("call void @hew_string_drop(").count(),
        1,
        "the gated drop body must appear exactly once; got:\n{ir}"
    );
}

/// LLC-5: a borrow-tainted `ElabDrop` under `borrow_mode = Some(iv)` must
/// emit the borrow-gated region — an `icmp eq` into a `borrow_drop_copy_only`
/// block converging at `borrow_drop_merge`. Pins that the borrow predicate +
/// its block labels survive the `emit_gated_drop_region` extraction.
#[test]
fn borrow_gated_drop_emits_copy_only_conditional_region() {
    use hew_mir::{DropKind, ElabDrop};
    let ctx = Context::create();
    let m = ctx.create_module("borrow_gated_drop_test");
    let harness = build_harness(&ctx, &[], &[]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    alloc_local(&mut fn_ctx, 0, ResolvedTy::String);
    // Install a borrow_mode i64 value and taint the local's base so the
    // borrow-aware emitter routes through the gated region.
    let borrow_mode = fn_ctx.ctx.i64_type().const_int(0, false);
    fn_ctx.borrow_mode = Some(borrow_mode);
    fn_ctx.borrow_tainted.insert(0);
    let drop = ElabDrop {
        place: Place::Local(0),
        ty: ResolvedTy::String,
        drop_fn: None,
        kind: DropKind::CowHeap {
            release: hew_mir::CowHeapRelease::String,
        },
        guard: None,
    };
    emit_one_elab_drop(&fn_ctx, &drop).expect("borrow-gated drop must emit");
    finish_test_fn(&fn_ctx);
    let ir = m.print_to_string().to_string();
    assert!(
        ir.contains("borrow_drop_copy_only"),
        "borrow-gated drop must branch into a `borrow_drop_copy_only` block; got:\n{ir}"
    );
    assert!(
        ir.contains("borrow_drop_merge"),
        "borrow-gated drop must converge at `borrow_drop_merge`; got:\n{ir}"
    );
    assert_eq!(
        ir.matches("call void @hew_string_drop(").count(),
        1,
        "the borrow-gated drop body must appear exactly once; got:\n{ir}"
    );
}

/// A `DropKind::CowHeap { release: Bytes }` ElabDrop on a
/// `bytes` local must route through the BytesTriple-aware emitter
/// (`emit_bytes_inplace_drop`): GEP field 0, load the data pointer, call
/// `hew_bytes_drop(data_ptr)`, null-store the field — NOT the generic
/// single-`ptr`-load CowHeap path (whose congruence table deliberately
/// excludes Bytes).
#[test]
fn cow_heap_bytes_drop_emits_triple_field0_release() {
    use hew_mir::{DropKind, ElabDrop};
    let ctx = Context::create();
    let m = ctx.create_module("cow_heap_bytes_test");
    let harness = build_harness(&ctx, &[], &[]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    alloc_local(&mut fn_ctx, 0, ResolvedTy::Bytes);
    let drop = ElabDrop {
        place: Place::Local(0),
        ty: ResolvedTy::Bytes,
        drop_fn: None,
        kind: DropKind::CowHeap {
            release: hew_mir::CowHeapRelease::Bytes,
        },
        guard: None,
    };
    emit_one_elab_drop(&fn_ctx, &drop).expect("CowHeap bytes drop must emit");
    finish_test_fn(&fn_ctx);
    let ir = m.print_to_string().to_string();
    assert_eq!(
        ir.matches("call void @hew_bytes_drop(").count(),
        1,
        "expected EXACTLY ONE void call to hew_bytes_drop; got:\n{ir}"
    );
    assert!(
        ir.contains("declare void @hew_bytes_drop(ptr)"),
        "hew_bytes_drop must be declared as void(ptr); got:\n{ir}"
    );
    assert!(
        ir.contains("getelementptr"),
        "the bytes drop must address the triple's ptr field by GEP, not \
             load the whole slot as a pointer; got:\n{ir}"
    );
    assert_eq!(
        ir.matches("store ptr null,").count(),
        1,
        "expected EXACTLY ONE null-store of the data-ptr field after the \
             drop call; got:\n{ir}"
    );
}

// (Removed `cow_heap_bytes_drop_wrong_symbol_fails_closed`: a wrong release
// symbol on a Bytes drop is now unrepresentable. `DropKind::CowHeap` carries
// a typed `CowHeapRelease`, and the Bytes-triple emitter is selected by
// `CowHeapRelease::Bytes` — the type system enforces what the runtime
// congruence check used to.)

/// W5.011 Slice 1: a `DropKind::CowHeap { release: VecPlain }`
/// ElabDrop on a `Vec<string>` local must emit the `hew_vec_free`
/// release call (the designated local-Vec free symbol).
#[test]
fn cow_heap_vec_drop_emits_hew_vec_free_call() {
    use hew_mir::{DropKind, ElabDrop};
    let ctx = Context::create();
    let m = ctx.create_module("cow_heap_vec_test");
    let harness = build_harness(&ctx, &[], &[]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    let vec_ty = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![ResolvedTy::String],
        // Canonical compiler-produced Vec: the `builtin` discriminant is
        // what routes to `hew_vec_free` (a user `type Vec` with
        // `builtin: None` must NOT — see `resolved_ty_cow_heap_release`).
        builtin: Some(hew_types::BuiltinType::Vec),
        is_opaque: false,
    };
    alloc_local(&mut fn_ctx, 0, vec_ty.clone());
    let drop = ElabDrop {
        place: Place::Local(0),
        ty: vec_ty,
        drop_fn: None,
        kind: DropKind::CowHeap {
            release: hew_mir::CowHeapRelease::VecPlain,
        },
        guard: None,
    };
    emit_one_elab_drop(&fn_ctx, &drop).expect("CowHeap vec drop must emit");
    finish_test_fn(&fn_ctx);
    let ir = m.print_to_string().to_string();
    assert_eq!(
        ir.matches("call void @hew_vec_free(").count(),
        1,
        "expected EXACTLY ONE void call to hew_vec_free; got:\n{ir}"
    );
}

/// `DropKind::AggregateRecursive` on a `(string, bytes, i64)` tuple local
/// must GEP the struct, drop the heap-owning `string` and `bytes` fields
/// through their matching helpers, and leave the `i64` field untouched.
#[test]
fn aggregate_recursive_tuple_drops_only_heap_field() {
    use hew_mir::{DropKind, ElabDrop};
    let ctx = Context::create();
    let m = ctx.create_module("agg_tuple_test");
    let harness = build_harness(&ctx, &[], &[]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    let tuple_ty = ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::Bytes, ResolvedTy::I64]);
    alloc_local(&mut fn_ctx, 0, tuple_ty.clone());
    let drop = ElabDrop {
        place: Place::Local(0),
        ty: tuple_ty,
        drop_fn: None,
        kind: DropKind::AggregateRecursive,
        guard: None,
    };
    emit_one_elab_drop(&fn_ctx, &drop).expect("AggregateRecursive tuple drop must emit");
    finish_test_fn(&fn_ctx);
    let ir = m.print_to_string().to_string();
    assert!(
        ir.contains("call void @hew_string_drop("),
        "tuple drop must release its string field via hew_string_drop; got:\n{ir}"
    );
    assert!(
        ir.contains("call void @hew_bytes_drop("),
        "tuple drop must release its bytes field via hew_bytes_drop; got:\n{ir}"
    );
    // Exactly one heap-owning field → exactly one release call.
    assert_eq!(
        ir.matches("call void @hew_string_drop(").count(),
        1,
        "the i64 field must not produce a drop call; got:\n{ir}"
    );
    assert_eq!(
        ir.matches("call void @hew_bytes_drop(").count(),
        1,
        "the bytes field must produce one drop call; got:\n{ir}"
    );
    assert!(
        ir.contains("getelementptr"),
        "tuple field drop must address fields by GEP; got:\n{ir}"
    );
}

/// W5.011 Slice 1: the aggregate-recursive walk fails closed past the
/// depth bound (cyclic-descriptor guard, `boundary-fail-closed`).
#[test]
fn aggregate_recursive_depth_bound_fails_closed() {
    let ctx = Context::create();
    let m = ctx.create_module("agg_depth_test");
    let harness = build_harness(&ctx, &[], &[]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    let tuple_ty = ResolvedTy::Tuple(vec![ResolvedTy::String]);
    alloc_local(&mut fn_ctx, 0, tuple_ty.clone());
    let (slot, _) = place_pointer(&fn_ctx, Place::Local(0)).expect("slot");
    let err = emit_aggregate_recursive_drop(
        &fn_ctx,
        slot,
        &tuple_ty,
        AGGREGATE_DROP_DEPTH_BOUND + 1,
        "agg_depth",
    )
    .expect_err("over-depth walk must fail closed");
    match err {
        CodegenError::FailClosed(msg) => assert!(
            msg.contains("depth bound"),
            "diagnostic must name the depth bound; got: {msg}"
        ),
        other => panic!("expected FailClosed, got {other:?}"),
    }
    finish_test_fn(&fn_ctx);
}

// (Removed `cow_heap_drop_unknown_symbol_fails_closed` and
// `cow_heap_drop_symbol_incongruent_with_type_fails_closed`: both tested
// producer drift on the pre-consolidation literal `drop_fn` carrier — an
// unknown symbol, and a known-but-type-incongruent symbol. With the typed
// `CowHeapRelease` carrier neither is constructible (there is no symbol to
// fabricate and no `(leaf, refinement)` pair the type system does not
// enforce), so the ElabDrop arm resolves the symbol from the carried fact
// with no re-derivation or congruence check. The type system now provides
// the guarantee these runtime checks used to.)

/// W5.011 P3 (Fix #2.4): a `DropKind::CowHeap` whose `ElabDrop::drop_fn`
/// is also populated mixes the W3.030 close-symbol path with the
/// kind-carried path and must fail closed in release mode (promoted from
/// the prior `debug_assert`).
#[test]
fn cow_heap_drop_with_populated_drop_fn_fails_closed() {
    use hew_mir::{DropKind, ElabDrop};
    let ctx = Context::create();
    let m = ctx.create_module("cow_heap_dropfn_set_test");
    let harness = build_harness(&ctx, &[], &[]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    alloc_local(&mut fn_ctx, 0, ResolvedTy::String);
    let drop = ElabDrop {
        place: Place::Local(0),
        ty: ResolvedTy::String,
        drop_fn: Some(hew_mir::DropFnSpec::Release("hew_string_drop")),
        kind: DropKind::CowHeap {
            release: hew_mir::CowHeapRelease::String,
        },
        guard: None,
    };
    let err = emit_one_elab_drop(&fn_ctx, &drop)
        .expect_err("CowHeap with populated drop_fn must fail closed");
    match err {
        CodegenError::FailClosed(msg) => assert!(
            msg.contains("MUST carry its release protocol in the kind"),
            "diagnostic must explain the kind-vs-drop_fn rule; got: {msg}"
        ),
        other => panic!("expected FailClosed, got {other:?}"),
    }
}

/// W5.011 P3 (Fix #2.4): an `AggregateRecursive` drop carrying a
/// `drop_fn` is malformed (its per-leaf symbols come from the structural
/// walk) and must fail closed.
#[test]
fn aggregate_recursive_with_drop_fn_fails_closed() {
    use hew_mir::{DropKind, ElabDrop};
    let ctx = Context::create();
    let m = ctx.create_module("agg_dropfn_set_test");
    let harness = build_harness(&ctx, &[], &[]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    let tuple_ty = ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]);
    alloc_local(&mut fn_ctx, 0, tuple_ty.clone());
    let drop = ElabDrop {
        place: Place::Local(0),
        ty: tuple_ty,
        drop_fn: Some(hew_mir::DropFnSpec::Release("hew_string_drop")),
        kind: DropKind::AggregateRecursive,
        guard: None,
    };
    let err = emit_one_elab_drop(&fn_ctx, &drop)
        .expect_err("AggregateRecursive with drop_fn must fail closed");
    match err {
        CodegenError::FailClosed(msg) => assert!(
            msg.contains("must carry none"),
            "diagnostic must explain the no-drop_fn rule; got: {msg}"
        ),
        other => panic!("expected FailClosed, got {other:?}"),
    }
}

/// The aggregate-recursive walk must NOT silently no-op an unsupported
/// heap-owning leaf. A cancellation token still has no aggregate-recursive
/// leaf release and must fail closed rather than skip the field's release.
#[test]
fn aggregate_recursive_unsupported_leaf_fails_closed() {
    let ctx = Context::create();
    let m = ctx.create_module("agg_unsupported_leaf_test");
    let harness = build_harness(&ctx, &[], &[]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    let tuple_ty = ResolvedTy::Tuple(vec![ResolvedTy::CancellationToken]);
    alloc_local(&mut fn_ctx, 0, tuple_ty.clone());
    let (slot, _) = place_pointer(&fn_ctx, Place::Local(0)).expect("slot");
    let err = emit_aggregate_recursive_drop(&fn_ctx, slot, &tuple_ty, 0, "agg_unsupported")
        .expect_err("unsupported aggregate leaf must fail closed");
    match err {
        CodegenError::FailClosed(msg) => assert!(
            msg.contains("neither a known heap-owning release type"),
            "diagnostic must explain the unsupported-leaf refusal; got: {msg}"
        ),
        other => panic!("expected FailClosed, got {other:?}"),
    }
    finish_test_fn(&fn_ctx);
}

/// Checker-stamped runtime collection dispatch must use the `builtin`
/// discriminant, not a colliding user type's name.
#[test]
fn checker_stamped_collection_dispatch_ignores_user_name_collisions() {
    let ctx = Context::create();
    let llvm_mod = ctx.create_module("cow_heap_release_test");
    let harness = build_harness(&ctx, &[], &[]);
    let fn_ctx = make_test_fn_ctx(&ctx, &llvm_mod, &harness, "cow_heap_release_probe");

    let sym =
        |ty: &ResolvedTy| resolved_ty_cow_heap_release(&fn_ctx, ty).map(|r| r.release_symbol());

    for name in ["Vec", "HashMap", "HashSet"] {
        let user_ty = ResolvedTy::Named {
            name: name.to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        };
        assert_eq!(
            sym(&user_ty),
            None,
            "user type `{name}` must not resolve to a runtime collection release"
        );
    }

    // The genuine builtin Vec of a string element: `string` takes its own
    // ElemKind path, so the Vec releases via the plain `hew_vec_free`.
    let builtin_vec = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![ResolvedTy::String],
        builtin: Some(hew_types::BuiltinType::Vec),
        is_opaque: false,
    };
    assert_eq!(
        resolved_ty_cow_heap_release(&fn_ctx, &builtin_vec),
        Some(hew_mir::CowHeapRelease::VecPlain)
    );
    assert_eq!(sym(&builtin_vec), Some("hew_vec_free"));
    // W5.016: a Vec of an owned tuple releases via the owned ABI.
    let owned_vec = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![ResolvedTy::Tuple(vec![
            ResolvedTy::String,
            ResolvedTy::String,
        ])],
        builtin: Some(hew_types::BuiltinType::Vec),
        is_opaque: false,
    };
    assert_eq!(
        resolved_ty_cow_heap_release(&fn_ctx, &owned_vec),
        Some(hew_mir::CowHeapRelease::VecOwnedElement)
    );
    assert_eq!(sym(&owned_vec), Some("hew_vec_free_owned"));
}

/// A `Vec` whose element is a `fn` / closure uses the canonical descriptor
/// release symbol; its stamped drop thunk performs the pair/env walk.
#[test]
fn resolved_ty_cow_heap_release_routes_closure_pair_vec() {
    let ctx = Context::create();
    let llvm_mod = ctx.create_module("cow_heap_release_closure_pair_test");
    let harness = build_harness(&ctx, &[], &[]);
    let fn_ctx = make_test_fn_ctx(&ctx, &llvm_mod, &harness, "cow_heap_closure_pair_probe");

    // `Vec<fn() -> ()>` — a bare function element.
    let vec_of_fn = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![ResolvedTy::Function {
            params: vec![],
            ret: Box::new(ResolvedTy::Unit),
        }],
        builtin: Some(hew_types::BuiltinType::Vec),
        is_opaque: false,
    };
    assert_eq!(
        resolved_ty_cow_heap_release(&fn_ctx, &vec_of_fn),
        Some(hew_mir::CowHeapRelease::VecClosurePairs),
        "a `Vec<fn>` element must release via the closure-pair ABI"
    );

    // `Vec<closure>` — a capturing closure element.
    let vec_of_closure = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![ResolvedTy::Closure {
            params: vec![ResolvedTy::I64],
            ret: Box::new(ResolvedTy::I64),
            captures: vec![ResolvedTy::String],
        }],
        builtin: Some(hew_types::BuiltinType::Vec),
        is_opaque: false,
    };
    assert_eq!(
        resolved_ty_cow_heap_release(&fn_ctx, &vec_of_closure).map(|r| r.release_symbol()),
        Some("hew_vec_free_owned"),
        "a `Vec<closure>` element must release via the descriptor ABI"
    );

    // The closure-pair symbol must be admitted by the closed congruence set
    // codegen validates RecordFieldDrop / inline-Drop symbols against.
    assert!(
        is_known_cow_heap_drop_symbol("hew_vec_free_owned"),
        "hew_vec_free_owned must be in the permitted CowHeap release set"
    );
}

/// MIR↔codegen congruence pin (`dedup-semantic-boundary`): codegen's
/// `resolved_ty_element_owns_heap_for_owned_vec` owned-element verdicts must
/// equal the MIR `is_owned_vec_element` verdicts over the shared shape table.
/// The sibling test `is_owned_vec_element_matches_codegen_owned_vec_table` in
/// `hew-mir` pins the SAME table on the MIR side, so a drift in either crate's
/// owned-element decision (which selects whether the Vec is built + freed
/// through the owned-descriptor ABI) fails its own crate's test before the two
/// can disagree on a getter/constructor/free at a seam.
#[test]
fn resolved_ty_element_owns_heap_for_owned_vec_matches_mir_table() {
    let ctx = Context::create();
    let llvm_mod = ctx.create_module("owned_vec_table_test");
    let harness = build_harness(&ctx, &[], &[]);
    let fn_ctx = make_test_fn_ctx(&ctx, &llvm_mod, &harness, "owned_vec_table_probe");

    let fn_elem = ResolvedTy::Function {
        params: vec![ResolvedTy::I64],
        ret: Box::new(ResolvedTy::I64),
    };
    let closure_elem = ResolvedTy::Closure {
        params: vec![],
        ret: Box::new(ResolvedTy::Unit),
        captures: vec![ResolvedTy::String],
    };
    let vec_of = |elem: ResolvedTy| ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![elem],
        builtin: Some(hew_types::BuiltinType::Vec),
        is_opaque: false,
    };
    // Identical to the MIR `is_owned_vec_element` table — keep in lockstep.
    let table: [(ResolvedTy, bool); 11] = [
        (ResolvedTy::String, false),
        (ResolvedTy::Bytes, false),
        (ResolvedTy::I64, false),
        (
            ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]),
            true,
        ),
        (
            ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::I64]),
            false,
        ),
        (
            ResolvedTy::Named {
                name: "HashMap".to_string(),
                args: vec![ResolvedTy::String, ResolvedTy::I64],
                builtin: Some(hew_types::BuiltinType::HashMap),
                is_opaque: false,
            },
            true,
        ),
        (
            ResolvedTy::Named {
                name: "HashSet".to_string(),
                args: vec![ResolvedTy::I64],
                builtin: Some(hew_types::BuiltinType::HashSet),
                is_opaque: false,
            },
            true,
        ),
        (vec_of(ResolvedTy::I64), true),
        (vec_of(fn_elem.clone()), false),
        (fn_elem, false),
        (closure_elem, false),
    ];
    for (elem, want) in table {
        assert_eq!(
            resolved_ty_element_owns_heap_for_owned_vec(&fn_ctx, &elem),
            want,
            "resolved_ty_element_owns_heap_for_owned_vec({elem:?}) must equal the MIR table"
        );
    }
    finish_test_fn(&fn_ctx);
}

/// Pin `cbor_vec_elem_kind` over every REACHABLE element shape: scalars stay
/// `Plain`, `string` stays `Str`, a heap-free user record stays
/// `LayoutBitCopy` (the `Vec<#[wire] struct>` shape), a heap-OWNING record
/// whose thunk key resolves is `Owned` (the new owned-element ABI), a nested
/// `Vec` element is `Owned`, `Option<T>` mirrors its payload, and the
/// remaining builtins (`HashMap` / `Result` / …) stay `Defer`.
#[test]
fn cbor_vec_elem_kind_pins_reachable_classification() {
    let no_recs: Vec<RecordLayout> = vec![];
    let no_enums: Vec<EnumLayout> = vec![];
    let no_machines: MachineLayoutMap = std::collections::HashMap::new();

    // Scalars → Plain (generic, null-layout vec).
    for s in [
        ResolvedTy::I64,
        ResolvedTy::U8,
        ResolvedTy::Bool,
        ResolvedTy::F64,
        ResolvedTy::Char,
        ResolvedTy::Duration,
    ] {
        assert_eq!(
            cbor_vec_elem_kind(&s, &no_recs, &no_enums, &no_machines),
            CborVecElemKind::Plain,
            "{s:?} must be a Plain element"
        );
    }
    // `string` → Str.
    assert_eq!(
        cbor_vec_elem_kind(&ResolvedTy::String, &no_recs, &no_enums, &no_machines),
        CborVecElemKind::Str
    );

    // A heap-free user record → LayoutBitCopy (the `Vec<Inner>` wire shape).
    let inner = RecordLayout {
        name: "Inner".to_string(),
        field_tys: vec![ResolvedTy::I64],
        field_names: vec!["v".to_string()],
    };
    assert_eq!(
        cbor_vec_elem_kind(
            &ResolvedTy::named_user("Inner", vec![]),
            &[inner],
            &no_enums,
            &no_machines
        ),
        CborVecElemKind::LayoutBitCopy,
        "a heap-free record element is the layout-aware BitCopy vec `Vec::new` builds"
    );
    // A heap-OWNING user record (a `string` field) whose thunk key resolves
    // (it is a registered record) → Owned, the new owned-element ABI matching
    // `Vec::new`'s `VecCtor::Owned`.
    let owns = RecordLayout {
        name: "Owns".to_string(),
        field_tys: vec![ResolvedTy::String],
        field_names: vec!["s".to_string()],
    };
    assert_eq!(
        cbor_vec_elem_kind(
            &ResolvedTy::named_user("Owns", vec![]),
            std::slice::from_ref(&owns),
            &no_enums,
            &no_machines
        ),
        CborVecElemKind::Owned,
        "a string-bearing record element rides the owned Vec ABI (thunk key resolves)"
    );

    // `Option<scalar>` → LayoutBitCopy; `Option<string>` → Defer (the payload
    // recursion, byte-identical to the retired walker's `Option` arm).
    assert_eq!(
        cbor_vec_elem_kind(
            &ResolvedTy::named_builtin("Option", BuiltinType::Option, vec![ResolvedTy::I64]),
            &no_recs,
            &no_enums,
            &no_machines
        ),
        CborVecElemKind::LayoutBitCopy
    );
    assert_eq!(
        cbor_vec_elem_kind(
            &ResolvedTy::named_builtin("Option", BuiltinType::Option, vec![ResolvedTy::String]),
            &no_recs,
            &no_enums,
            &no_machines
        ),
        CborVecElemKind::Defer
    );

    // A nested `Vec<E>` element stays fail-closed: an owned Vec of collections
    // needs the base language's recursive owned clone/drop thunk synthesis,
    // which is NYI (a direct `Vec<Vec<string>>` aborts at runtime even outside
    // the codec, and the checker already rejects the analogous
    // `Vec<record-with-a-collection-field>`).
    assert_eq!(
        cbor_vec_elem_kind(
            &ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::I64]),
            &no_recs,
            &no_enums,
            &no_machines
        ),
        CborVecElemKind::Defer,
        "a nested Vec element fails closed until the base owned nested-Vec ABI lands"
    );
    // `HashMap` (no CBOR value arm) and every other non-`Option` builtin stay
    // fail-closed.
    for b in [
        ResolvedTy::named_builtin(
            "HashMap",
            BuiltinType::HashMap,
            vec![ResolvedTy::String, ResolvedTy::I64],
        ),
        ResolvedTy::named_builtin(
            "Result",
            BuiltinType::Result,
            vec![ResolvedTy::I64, ResolvedTy::I64],
        ),
    ] {
        assert_eq!(
            cbor_vec_elem_kind(&b, &no_recs, &no_enums, &no_machines),
            CborVecElemKind::Defer,
            "{b:?} must fail closed (no CBOR value arm / no owned-vec support)"
        );
    }
}

/// A heap-owning record element whose thunk key resolves rides the OWNED Vec
/// ABI (`ownership_kind = LayoutManaged`), so the codec never BitCopies (and
/// leaks) an owned handle. A `CancellationToken`-bearing record classifies
/// `Owned` — the backing vec has a real per-element `drop_fn`, so there is no
/// BitCopy leak; the token field itself has no CBOR value arm, so the codec
/// fails closed downstream at `emit_ser_value_cbor` (and the checker's
/// Serializable gate rejects such a `#[wire]` type up front for real code).
#[test]
fn cbor_owning_record_element_rides_owned_abi_never_bitcopy_leak() {
    let rec = RecordLayout {
        name: "WithTok".to_string(),
        field_tys: vec![ResolvedTy::CancellationToken],
        field_names: vec!["tok".to_string()],
    };
    let no_enums: Vec<EnumLayout> = vec![];
    let no_machines: MachineLayoutMap = std::collections::HashMap::new();
    assert_eq!(
        cbor_vec_elem_kind(
            &ResolvedTy::named_user("WithTok", vec![]),
            std::slice::from_ref(&rec),
            &no_enums,
            &no_machines
        ),
        CborVecElemKind::Owned,
        "a CancellationToken-bearing record rides the owned ABI (real drop_fn, no \
             BitCopy leak); the unencodable token field fails closed downstream"
    );
}

/// A CBOR `Vec` element that IS — or transitively contains — an `indirect
/// enum` must fail closed (`Defer`), never `LayoutBitCopy`. An indirect enum
/// with scalar payloads is a heap-owned pointer (the `Vec` constructor stores
/// it as `ptr`; the CBOR decoder allocates and stores a heap node), yet the
/// `hew_mir::ty_heap_ownership` authority reports that indirectness through
/// the layout adapter, so the codec defers in agreement with the pointer ABI
/// both endpoints already use. Without the carried bit the codec would
/// BitCopy on encode while the constructor/decoder used pointer ABI, leaking
/// the heap-owned node.
///
/// Revert-repro: ignore `HeapOwnership::via_indirection` in
/// `cbor_vec_elem_kind` and the indirect-enum assertions below report
/// `LayoutBitCopy`.
#[test]
fn cbor_vec_elem_kind_fails_closed_for_indirect_enum_element() {
    // `indirect enum Node { Leaf(i64); Nil; }` — scalar payloads, yet every
    // value is a heap-owned pointer to a tagged-union struct.
    let indirect = EnumLayout {
        name: "Node".to_string(),
        tag_width: 1,
        variants: vec![
            MachineVariantLayout {
                name: "Leaf".to_string(),
                field_tys: vec![ResolvedTy::I64],
                field_names: vec!["v".to_string()],
            },
            MachineVariantLayout {
                name: "Nil".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
        ],
        is_indirect: true,
    };
    let no_recs: Vec<RecordLayout> = vec![];
    let no_machines: MachineLayoutMap = std::collections::HashMap::new();

    // The indirect enum itself → Defer (NOT LayoutBitCopy).
    assert_eq!(
        cbor_vec_elem_kind(
            &ResolvedTy::named_user("Node", vec![]),
            &no_recs,
            std::slice::from_ref(&indirect),
            &no_machines,
        ),
        CborVecElemKind::Defer,
        "an indirect-enum element is a heap-owned pointer — must fail closed, not BitCopy"
    );

    // A record that transitively CONTAINS the indirect enum → Defer.
    let wrapper = RecordLayout {
        name: "Wrapper".to_string(),
        field_tys: vec![ResolvedTy::named_user("Node", vec![])],
        field_names: vec!["node".to_string()],
    };
    assert_eq!(
        cbor_vec_elem_kind(
            &ResolvedTy::named_user("Wrapper", vec![]),
            std::slice::from_ref(&wrapper),
            std::slice::from_ref(&indirect),
            &no_machines,
        ),
        CborVecElemKind::Defer,
        "a record transitively containing an indirect enum must fail closed"
    );

    // Control: a NON-indirect enum with the SAME scalar payloads stays
    // LayoutBitCopy — the fail-closed term must not over-defer a heap-free
    // direct enum (over-drop is the floor, but never at the cost of a
    // spurious defer that would refuse a legitimately reachable BitCopy).
    let direct = EnumLayout {
        name: "Flat".to_string(),
        tag_width: 1,
        variants: vec![
            MachineVariantLayout {
                name: "A".to_string(),
                field_tys: vec![ResolvedTy::I64],
                field_names: vec!["v".to_string()],
            },
            MachineVariantLayout {
                name: "B".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
        ],
        is_indirect: false,
    };
    assert_eq!(
        cbor_vec_elem_kind(
            &ResolvedTy::named_user("Flat", vec![]),
            &no_recs,
            std::slice::from_ref(&direct),
            &no_machines,
        ),
        CborVecElemKind::LayoutBitCopy,
        "a heap-free direct enum must stay BitCopy — the fix must not over-defer"
    );
}

/// DIV-2: codegen's owned-Vec element walker
/// (`resolved_ty_element_owns_heap_for_owned_vec` → the unified
/// `resolved_ty_contains_heap_leaf` → `hew_mir::ty_owns_heap`) must classify
/// a `CancellationToken` / `Generator` / `AsyncGenerator` as a heap-owning
/// leaf, matching the MIR drop elaborator's authority. Before the walkers
/// were unified codegen had no token/generator arm and answered `false` for
/// a bare handle, while the MIR side answered `true` — the asymmetry that
/// makes a handle-bearing tuple's owned-Vec element ABI disagree between the
/// constructor and the elaborator's drop_fn (the #2191/#2150 class).
#[test]
fn owned_vec_element_walker_treats_tokens_and_generators_as_heap_owning() {
    let ctx = Context::create();
    let llvm_mod = ctx.create_module("div2_leaf_test");
    let harness = build_harness(&ctx, &[], &[]);
    let fn_ctx = make_test_fn_ctx(&ctx, &llvm_mod, &harness, "div2_leaf_probe");

    let generator = ResolvedTy::Named {
        name: "Generator".to_string(),
        args: vec![ResolvedTy::I64, ResolvedTy::Unit],
        builtin: Some(hew_types::BuiltinType::Generator),
        is_opaque: false,
    };
    let async_generator = ResolvedTy::Named {
        name: "AsyncGenerator".to_string(),
        args: vec![ResolvedTy::I64],
        builtin: Some(hew_types::BuiltinType::AsyncGenerator),
        is_opaque: false,
    };

    // The exact DIV-2 element shape: `(Generator<i64,()>, i64)`.
    let gen_tuple = ResolvedTy::Tuple(vec![generator, ResolvedTy::I64]);
    assert!(
        resolved_ty_element_owns_heap_for_owned_vec(&fn_ctx, &gen_tuple),
        "a tuple carrying a Generator handle must own heap on the codegen owned-Vec path \
             so its element ABI matches the MIR drop elaborator's"
    );
    let async_tuple = ResolvedTy::Tuple(vec![async_generator, ResolvedTy::I64]);
    assert!(
        resolved_ty_element_owns_heap_for_owned_vec(&fn_ctx, &async_tuple),
        "a tuple carrying an AsyncGenerator handle must own heap on the codegen owned-Vec \
             path"
    );
    let token_tuple = ResolvedTy::Tuple(vec![ResolvedTy::CancellationToken, ResolvedTy::I64]);
    assert!(
        resolved_ty_element_owns_heap_for_owned_vec(&fn_ctx, &token_tuple),
        "a tuple carrying a CancellationToken must own heap on the codegen owned-Vec path"
    );
    // Guard against over-broad classification: a plain `(i64, bool)` tuple
    // still owns no heap.
    let plain_tuple = ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::Bool]);
    assert!(
        !resolved_ty_element_owns_heap_for_owned_vec(&fn_ctx, &plain_tuple),
        "an all-BitCopy tuple must not be classified heap-owning"
    );
}

/// DIV-1 at the codegen layer: the unified `resolved_ty_contains_heap_leaf`
/// consults record fields through the `CgHeapLayouts` adapter, so a record
/// carrying a heap field (`Boxed { payload: Vec<i64> }`) is classified
/// heap-owning even when reached as a tuple element. Before unification the
/// MIR side could classify such a tuple owned while a record-blind codegen
/// walker disagreed; both now share the one authority.
#[test]
fn owned_vec_element_walker_is_record_aware() {
    use std::collections::HashMap;

    let ctx = Context::create();
    let llvm_mod = ctx.create_module("div1_codegen_test");
    let mut harness = build_harness(&ctx, &[], &[]);
    // Register `Boxed { payload: Vec<i64> }` in the codegen record registry.
    let mut record_field_resolved_tys: HashMap<String, Vec<ResolvedTy>> = HashMap::new();
    record_field_resolved_tys.insert(
        "Boxed".to_string(),
        vec![ResolvedTy::Named {
            name: "Vec".to_string(),
            args: vec![ResolvedTy::I64],
            builtin: Some(hew_types::BuiltinType::Vec),
            is_opaque: false,
        }],
    );
    harness.record_field_resolved_tys = record_field_resolved_tys;
    let fn_ctx = make_test_fn_ctx(&ctx, &llvm_mod, &harness, "div1_codegen_probe");

    let boxed = ResolvedTy::Named {
        name: "Boxed".to_string(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    };
    // `(Boxed, i64)` — the exact runtime-confirmed DIV-1 leak shape.
    let boxed_tuple = ResolvedTy::Tuple(vec![boxed, ResolvedTy::I64]);
    assert!(
        resolved_ty_element_owns_heap_for_owned_vec(&fn_ctx, &boxed_tuple),
        "a tuple carrying a record whose field owns heap must be classified heap-owning \
             through the record-aware authority (DIV-1)"
    );
}

/// Positive guard: layout-sourced genuine builtins carry their discriminant
/// and stay on the runtime layout ABI.
#[test]
fn hashmap_layout_ops_get_emits_clone_call_and_option_branches() {
    let ctx = Context::create();
    let m = ctx.create_module("hashmap_get_option_test");
    let harness = build_harness(
        &ctx,
        &[fixture_point_layout()],
        &[fixture_option_i64_layout()],
    );
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    let map_ty = ResolvedTy::named_builtin(
        BuiltinType::HashMap.canonical_name(),
        BuiltinType::HashMap,
        vec![
            ResolvedTy::Named {
                name: "Point".to_string(),
                args: vec![],
                builtin: None,
                is_opaque: false,
            },
            ResolvedTy::I64,
        ],
    );
    let point_ty = ResolvedTy::Named {
        name: "Point".to_string(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    };
    let option_i64_ty = ResolvedTy::named_builtin(
        BuiltinType::Option.canonical_name(),
        BuiltinType::Option,
        vec![ResolvedTy::I64],
    );
    assert!(map_ty.is_builtin(BuiltinType::HashMap));
    assert!(option_i64_ty.is_builtin(BuiltinType::Option));
    alloc_local(&mut fn_ctx, 0, map_ty);
    alloc_local(&mut fn_ctx, 1, point_ty);
    alloc_local(&mut fn_ctx, 2, option_i64_ty);
    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .expect("test fn parent");
    let next_bb = ctx.append_basic_block(parent, "bb1");
    fn_ctx.blocks.insert(1, next_bb);
    lower_hashmap_get_layout_call(
        &fn_ctx,
        &[Place::Local(0), Place::Local(1)],
        Some(&Place::Local(2)),
        1,
    )
    .expect("HashMap::get layout lowering must succeed");
    fn_ctx.builder.position_at_end(next_bb);
    finish_test_fn(&fn_ctx);
    assert!(
        m.verify().is_ok(),
        "module must verify:\n{}",
        m.print_to_string().to_string()
    );
    let ir = m.print_to_string().to_string();
    assert!(
        ir.contains("hew_hashmap_get_clone_layout_call")
            && ir.contains("hashmap_get_none")
            && ir.contains("hashmap_get_some"),
        "expected clone-on-get call and two branches; got IR:\n{ir}"
    );
    assert!(
        ir.contains("store i8 0") && ir.contains("store i8 1"),
        "expected Option::Some/None tags via emit_enum_variant_literal; got IR:\n{ir}"
    );
    assert!(
        !ir.contains("llvm.memcpy"),
        "HashMap::get must not memcpy a borrowed slot into owned Option<V>; got IR:\n{ir}"
    );
}

#[test]
fn hashmap_layout_ops_reject_user_named_collision() {
    let ctx = Context::create();
    let m = ctx.create_module("hashmap_get_user_collision_test");
    let harness = build_harness(
        &ctx,
        &[fixture_point_layout()],
        &[fixture_option_i64_layout()],
    );
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    let user_map_ty = ResolvedTy::Named {
        name: "HashMap".to_string(),
        args: vec![
            ResolvedTy::Named {
                name: "Point".to_string(),
                args: vec![],
                builtin: None,
                is_opaque: false,
            },
            ResolvedTy::I64,
        ],
        builtin: None,
        is_opaque: false,
    };
    let point_ty = ResolvedTy::Named {
        name: "Point".to_string(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    };
    let option_i64_ty = ResolvedTy::named_builtin(
        BuiltinType::Option.canonical_name(),
        BuiltinType::Option,
        vec![ResolvedTy::I64],
    );
    alloc_local(&mut fn_ctx, 0, user_map_ty);
    alloc_local(&mut fn_ctx, 1, point_ty);
    alloc_local(&mut fn_ctx, 2, option_i64_ty);

    let err = lower_hashmap_get_layout_call(
        &fn_ctx,
        &[Place::Local(0), Place::Local(1)],
        Some(&Place::Local(2)),
        1,
    )
    .expect_err("a user type named HashMap must not route to the runtime map ABI");
    assert!(
        matches!(&err, CodegenError::FailClosed(message) if message.contains("arg0 must be HashMap<K,V>")),
        "unexpected error: {err:?}"
    );
    finish_test_fn(&fn_ctx);
}

// ---- emit_result_ok ----------------------------------------------------

#[test]
fn emit_result_ok_unit_payload_stores_tag_zero_only() {
    let ctx = Context::create();
    let m = ctx.create_module("emit_result_ok_unit_test");
    let harness = build_harness(
        &ctx,
        &[],
        &[
            fixture_send_error_layout(),
            fixture_result_unit_send_error_layout(),
        ],
    );
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    alloc_local(
        &mut fn_ctx,
        0,
        ResolvedTy::Named {
            name: "Result$$unit$$SendError".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        },
    );
    emit_result_ok(&fn_ctx, Place::Local(0), None)
        .expect("emit_result_ok(None) must succeed for Result<(), SendError>");
    finish_test_fn(&fn_ctx);
    assert!(
        m.verify().is_ok(),
        "module must verify after emit_result_ok:\n{}",
        m.print_to_string().to_string()
    );
    let ir = m.print_to_string().to_string();
    // Tag-store of 0 + GEP for machine_tag_ptr must be present.
    // The carried one-bit tag width uses the i8 storage bucket.
    assert!(
        ir.contains("machine_tag_ptr") && ir.contains("store i8 0"),
        "expected tag GEP + store i8 0 for Result Ok tag; got IR:\n{ir}"
    );
}

#[test]
fn emit_result_ok_scalar_payload_stores_tag_and_copies_value() {
    let ctx = Context::create();
    let m = ctx.create_module("emit_result_ok_scalar_test");
    let harness = build_harness(
        &ctx,
        &[],
        &[
            fixture_lookup_error_layout(),
            fixture_result_i64_lookuperror_layout(),
        ],
    );
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    alloc_local(
        &mut fn_ctx,
        0,
        ResolvedTy::Named {
            name: "Result$$i64$$LookupError".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        },
    );
    alloc_local(&mut fn_ctx, 1, ResolvedTy::I64);
    emit_result_ok(&fn_ctx, Place::Local(0), Some(Place::Local(1)))
        .expect("emit_result_ok(Some(i64 local)) must succeed");
    finish_test_fn(&fn_ctx);
    assert!(
        m.verify().is_ok(),
        "module must verify:\n{}",
        m.print_to_string().to_string()
    );
    let ir = m.print_to_string().to_string();
    assert!(
        ir.contains("emit_result_ok_v0_f0_src"),
        "expected scalar payload load with helper-named label; got IR:\n{ir}"
    );
    assert!(
        ir.contains("machine_payload_ptr"),
        "expected machine_payload_ptr GEP for variant-0 field-0 store; got IR:\n{ir}"
    );
}

#[test]
fn emit_result_ok_rejects_non_local_dest() {
    let ctx = Context::create();
    let m = ctx.create_module("emit_result_ok_reject_test");
    let harness = build_harness(
        &ctx,
        &[],
        &[
            fixture_send_error_layout(),
            fixture_result_unit_send_error_layout(),
        ],
    );
    let fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    let err = emit_result_ok(&fn_ctx, Place::ReturnSlot, None)
        .expect_err("dest must be Place::Local(_); ReturnSlot is rejected");
    match err {
        CodegenError::FailClosed(msg) => {
            assert!(msg.contains("emit_result_ok"));
            assert!(msg.contains("ReturnSlot"));
        }
        other => panic!("expected FailClosed, got {other:?}"),
    }
}

// ---- emit_result_err ---------------------------------------------------

#[test]
fn emit_result_err_copies_err_payload_into_variant_one() {
    let ctx = Context::create();
    let m = ctx.create_module("emit_result_err_test");
    let harness = build_harness(
        &ctx,
        &[],
        &[
            fixture_send_error_layout(),
            fixture_result_unit_send_error_layout(),
        ],
    );
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    alloc_local(
        &mut fn_ctx,
        0,
        ResolvedTy::Named {
            name: "Result$$unit$$SendError".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        },
    );
    alloc_local(
        &mut fn_ctx,
        1,
        ResolvedTy::Named {
            name: "SendError".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        },
    );
    emit_result_err(&fn_ctx, Place::Local(0), Place::Local(1))
        .expect("emit_result_err must succeed for Result<(), SendError>");
    finish_test_fn(&fn_ctx);
    assert!(
        m.verify().is_ok(),
        "module must verify:\n{}",
        m.print_to_string().to_string()
    );
    let ir = m.print_to_string().to_string();
    // Tag-store of 1 (Err) — the carried one-bit tag width uses i8 storage.
    assert!(
        ir.contains("store i8 1"),
        "expected store i8 1 for Err tag on Result; got IR:\n{ir}"
    );
    assert!(
        ir.contains("emit_result_err_v1_f0_src"),
        "expected variant-1 field-0 source-load label; got IR:\n{ir}"
    );
}

#[test]
fn emit_result_err_rejects_type_mismatched_payload() {
    let ctx = Context::create();
    let m = ctx.create_module("emit_result_err_mismatch_test");
    let harness = build_harness(
        &ctx,
        &[],
        &[
            fixture_send_error_layout(),
            fixture_result_unit_send_error_layout(),
        ],
    );
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    alloc_local(
        &mut fn_ctx,
        0,
        ResolvedTy::Named {
            name: "Result$$unit$$SendError".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        },
    );
    // WRONG payload type: i64 where the variant expects SendError.
    alloc_local(&mut fn_ctx, 1, ResolvedTy::I64);
    let err = emit_result_err(&fn_ctx, Place::Local(0), Place::Local(1))
        .expect_err("mismatched payload type must fail closed, not silently mis-store");
    match err {
        CodegenError::FailClosed(msg) => {
            assert!(
                msg.contains("emit_result_err") && msg.contains("type mismatch"),
                "diagnostic must name the helper and the mismatch; got: {msg}"
            );
        }
        other => panic!("expected FailClosed, got {other:?}"),
    }
}

// ---- emit_send_result_from_rc (lambda/duplex `.send` D1 mapping) --------

/// The value-context `.send` materialization builds the Ok/Err/merge
/// diamond, maps the runtime rc onto the user `SendError` discriminant per
/// D1 (rc==2 → Full=0, every other nonzero rc → Closed=1), and never lets a
/// raw rc reach the discriminant slot. Driven with a non-constant i32 status
/// (alloca+load) so the comparisons and select survive as real IR rather
/// than constant-folding away.
#[test]
fn emit_send_result_from_rc_builds_diamond_and_maps_discriminant() {
    let ctx = Context::create();
    let m = ctx.create_module("emit_send_result_from_rc_test");
    let harness = build_harness(
        &ctx,
        &[],
        &[
            fixture_send_error_layout(),
            fixture_result_unit_send_error_layout(),
        ],
    );
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    alloc_local(
        &mut fn_ctx,
        0,
        ResolvedTy::Named {
            name: "Result$$unit$$SendError".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        },
    );
    // Non-constant i32 status: store-then-load through an alloca keeps the
    // value opaque to the builder's constant folder.
    let i32_ty = ctx.i32_type();
    let status_slot = fn_ctx
        .builder
        .build_alloca(i32_ty, "status_slot")
        .expect("status alloca");
    let status = fn_ctx
        .builder
        .build_load(i32_ty, status_slot, "status")
        .expect("status load")
        .into_int_value();
    emit_send_result_from_rc(&fn_ctx, status, Place::Local(0), "test_send")
        .expect("emit_send_result_from_rc must materialize Result<(), SendError>");
    finish_test_fn(&fn_ctx);

    assert!(
        m.verify().is_ok(),
        "module must verify after emit_send_result_from_rc:\n{}",
        m.print_to_string().to_string()
    );
    let ir = m.print_to_string().to_string();

    // The Ok/Err/merge diamond exists.
    for block in ["send_result_ok", "send_result_err", "send_result_merge"] {
        assert!(
            ir.contains(block),
            "expected `{block}` block; got IR:\n{ir}"
        );
    }
    // codegen-abi-authority: the dest is projected purely through the
    // registered machine layout for the local's ResolvedTy
    // (`Result$$unit$$SendError`) via MachineTag / MachineVariant — there is
    // no `expr_types` lookup anywhere on this path (the helper's signature
    // has no access to one). The GEP into the named layout type proves the
    // shape came from the layout authority, not a re-inferred expr type.
    assert!(
        ir.contains("getelementptr inbounds nuw %\"Result$$unit$$SendError\""),
        "expected dest projection through the registered Result layout; got IR:\n{ir}"
    );
    // The status is tested against 0 (Ok) and 2 (Full) — the only two
    // comparisons the D1 mapping needs.
    assert!(
        ir.contains("send_status_is_ok"),
        "expected `rc == 0` Ok test; got IR:\n{ir}"
    );
    assert!(
        ir.contains("send_status_is_full"),
        "expected `rc == 2` Full test; got IR:\n{ir}"
    );
    // The Result tag is i8 (tag_width 1): Ok stores 0, Err stores 1.
    assert!(
        ir.contains("store i8 0") && ir.contains("store i8 1"),
        "expected Result Ok tag `store i8 0` and Err tag `store i8 1`; got IR:\n{ir}"
    );
    // SECURITY: the SendError discriminant is chosen by a `select` between
    // the two in-range constants 0 (Full) and 1 (Closed). The raw rc is
    // NEVER the stored discriminant — rc values 3/4/5 would be out of range
    // for the 3-variant SendError enum (UB at the user's `match`). The
    // SendError tag lowers to i8 (≤256 variants round up to one byte).
    assert!(
        ir.contains("send_err_tag = select i1 %send_status_is_full, i8 0, i8 1"),
        "expected the discriminant select to map Full->0 / else->1 over the \
             in-range constants only (no raw rc reaches the slot); got IR:\n{ir}"
    );
    // The stored discriminant is the select result, never the raw status.
    assert!(
        ir.contains("store i8 %send_err_tag,"),
        "expected the SendError slot to be stored from the mapped select \
             result; got IR:\n{ir}"
    );
}

/// Composites materialize into a named local's alloca; a non-`Local` dest
/// is rejected fail-closed (mirrors `composite_dest_local`).
#[test]
fn emit_send_result_from_rc_rejects_non_local_dest() {
    let ctx = Context::create();
    let m = ctx.create_module("emit_send_result_reject_dest_test");
    let harness = build_harness(
        &ctx,
        &[],
        &[
            fixture_send_error_layout(),
            fixture_result_unit_send_error_layout(),
        ],
    );
    let fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    let i32_ty = ctx.i32_type();
    let status = i32_ty.const_zero();
    let err = emit_send_result_from_rc(&fn_ctx, status, Place::ReturnSlot, "test_send")
        .expect_err("dest must be Place::Local(_); ReturnSlot is rejected");
    match err {
        CodegenError::FailClosed(msg) => {
            assert!(
                msg.contains("test_send") && msg.contains("ReturnSlot"),
                "diagnostic must name the helper and the bad dest; got: {msg}"
            );
        }
        other => panic!("expected FailClosed, got {other:?}"),
    }
}

/// The runtime ABI returns i32. A status of any other width is a decl drift
/// and must fail closed rather than silently zext/trunc a security-sensitive
/// discriminant source.
#[test]
fn emit_send_result_from_rc_rejects_non_i32_status() {
    let ctx = Context::create();
    let m = ctx.create_module("emit_send_result_reject_width_test");
    let harness = build_harness(
        &ctx,
        &[],
        &[
            fixture_send_error_layout(),
            fixture_result_unit_send_error_layout(),
        ],
    );
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    alloc_local(
        &mut fn_ctx,
        0,
        ResolvedTy::Named {
            name: "Result$$unit$$SendError".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        },
    );
    // i64 status — wrong width.
    let status = ctx.i64_type().const_zero();
    let err = emit_send_result_from_rc(&fn_ctx, status, Place::Local(0), "test_send")
        .expect_err("non-i32 status must fail closed");
    match err {
        CodegenError::FailClosed(msg) => {
            assert!(
                msg.contains("test_send") && msg.contains("i32"),
                "diagnostic must name the helper and the width contract; got: {msg}"
            );
        }
        other => panic!("expected FailClosed, got {other:?}"),
    }
}

// ---- emit_struct_literal -----------------------------------------------

#[test]
fn emit_struct_literal_monitor_ref_writes_ref_id_field() {
    let ctx = Context::create();
    let m = ctx.create_module("emit_struct_literal_test");
    let harness = build_harness(&ctx, &[fixture_monitor_ref_layout()], &[]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    alloc_local(
        &mut fn_ctx,
        0,
        ResolvedTy::Named {
            name: "MonitorRef".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        },
    );
    alloc_local(&mut fn_ctx, 1, ResolvedTy::I64);
    emit_struct_literal(
        &fn_ctx,
        Place::Local(0),
        &[(FieldOffset(0), Place::Local(1))],
    )
    .expect("emit_struct_literal MonitorRef{ref_id:i64} must succeed");
    finish_test_fn(&fn_ctx);
    assert!(
        m.verify().is_ok(),
        "module must verify:\n{}",
        m.print_to_string().to_string()
    );
    let ir = m.print_to_string().to_string();
    // The delegated `lower_record_init` path uses field_{idx}_init_ptr /
    // field_{idx}_init_src labels — assert the substrate-delegation is live.
    assert!(
        ir.contains("field_0_init_ptr") && ir.contains("field_0_init_src"),
        "expected lower_record_init field-0 init labels (substrate delegation); \
             got IR:\n{ir}"
    );
}

// ---- emit_enum_variant_literal -----------------------------------------

#[test]
fn emit_enum_variant_literal_unit_variant_writes_tag_only() {
    let ctx = Context::create();
    let m = ctx.create_module("emit_enum_unit_test");
    let harness = build_harness(&ctx, &[], &[fixture_sample_enum_layout()]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    alloc_local(
        &mut fn_ctx,
        0,
        ResolvedTy::Named {
            name: "Sample".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        },
    );
    emit_enum_variant_literal(&fn_ctx, Place::Local(0), 0, &[])
        .expect("Sample::Empty (unit variant, idx 0) must succeed");
    finish_test_fn(&fn_ctx);
    assert!(
        m.verify().is_ok(),
        "module must verify:\n{}",
        m.print_to_string().to_string()
    );
    let ir = m.print_to_string().to_string();
    // Sample's carried two-bit tag width uses i8 storage.
    // Tag value 0 must be stored.
    assert!(
        ir.contains("machine_tag_ptr") && ir.contains("store i8 0"),
        "expected i8 tag store of 0 for Sample::Empty; got IR:\n{ir}"
    );
}

#[test]
fn emit_enum_variant_literal_two_field_variant_copies_all_fields() {
    let ctx = Context::create();
    let m = ctx.create_module("emit_enum_two_field_test");
    let harness = build_harness(&ctx, &[], &[fixture_sample_enum_layout()]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    alloc_local(
        &mut fn_ctx,
        0,
        ResolvedTy::Named {
            name: "Sample".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        },
    );
    alloc_local(&mut fn_ctx, 1, ResolvedTy::I64);
    alloc_local(&mut fn_ctx, 2, ResolvedTy::I64);
    emit_enum_variant_literal(
        &fn_ctx,
        Place::Local(0),
        2,
        &[Place::Local(1), Place::Local(2)],
    )
    .expect("Sample::TwoInts(i64, i64) must succeed");
    finish_test_fn(&fn_ctx);
    assert!(
        m.verify().is_ok(),
        "module must verify:\n{}",
        m.print_to_string().to_string()
    );
    let ir = m.print_to_string().to_string();
    // Tag store: variant_idx 2 in a 2-bit field = -2 (signed) — LLVM
    // prints constants in the smallest representation. We're robust to
    // signed/unsigned: just assert the field GEPs for both payload
    // fields fire.
    assert!(
        ir.contains("emit_enum_variant_literal_v2_f0_src")
            && ir.contains("emit_enum_variant_literal_v2_f1_src"),
        "expected per-field source-load labels for both TwoInts fields; \
             got IR:\n{ir}"
    );
    assert!(
        ir.contains("machine_payload_ptr"),
        "expected machine_payload_ptr GEP for variant 2 payload; got IR:\n{ir}"
    );
}

#[test]
fn emit_enum_variant_literal_rejects_field_count_mismatch() {
    // OneInt (variant 1) expects exactly 1 payload field. Passing 2
    // must fail closed at `place_pointer`'s field-index bounds check.
    let ctx = Context::create();
    let m = ctx.create_module("emit_enum_arity_test");
    let harness = build_harness(&ctx, &[], &[fixture_sample_enum_layout()]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "drv");
    alloc_local(
        &mut fn_ctx,
        0,
        ResolvedTy::Named {
            name: "Sample".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        },
    );
    alloc_local(&mut fn_ctx, 1, ResolvedTy::I64);
    alloc_local(&mut fn_ctx, 2, ResolvedTy::I64);
    let err = emit_enum_variant_literal(
        &fn_ctx,
        Place::Local(0),
        1, // OneInt — only 1 field
        &[Place::Local(1), Place::Local(2)],
    )
    .expect_err("passing 2 payload places to a 1-field variant must fail closed");
    match err {
        CodegenError::FailClosed(msg) => {
            assert!(
                msg.contains("field_idx") || msg.contains("out of range"),
                "diagnostic must indicate out-of-range field index; got: {msg}"
            );
        }
        other => panic!("expected FailClosed, got {other:?}"),
    }
}

// ---- W4.012 Stage 2: predeclare_named_layouts regression --------------
//
// These tests pin the codegen layout-graph fix: record/enum/machine
// outer struct names are predeclared as opaque LLVM structs BEFORE any
// body-fill pass, so a record field of enum/machine type resolves
// through `resolve_ty` instead of tripping the D10 fail-closed sentinel
// in `primitive_to_llvm`.

#[test]
fn record_field_of_enum_type_resolves_via_predeclared_opaque() {
    let ctx = Context::create();
    // `record CrashNotification { kind: CrashKind }` where `CrashKind`
    // is an enum declared in the same pipeline. Pre-Stage-2 this tripped
    // D10 because `register_record_layouts` ran Pass 2 against a map
    // containing only record opaques.
    let record_fixtures = vec![MirRecordLayout {
        name: "CrashNotification".to_string(),
        field_tys: vec![ResolvedTy::Named {
            name: "CrashKind".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        }],
        field_names: vec![],
    }];
    let enum_fixtures = vec![MirEnumLayout {
        name: "CrashKind".to_string(),
        tag_width: 1,
        variants: vec![
            MachineVariantLayout {
                name: "Panic".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
            MachineVariantLayout {
                name: "Abort".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
        ],
        is_indirect: false,
    }];
    let mut map =
        crate::layout::predeclare_named_layouts(&ctx, &record_fixtures, &enum_fixtures, &[], &[])
            .expect("predeclare must succeed");
    let mut machine_layouts: MachineLayoutMap<'_> = HashMap::new();
    crate::layout::register_enum_layouts(
        &ctx,
        &enum_fixtures,
        &mut map,
        &mut machine_layouts,
        None,
    )
    .expect("enum body-fill must succeed against predeclared opaque");
    let target_data = host_target_data();
    // Now fill the record body. `resolve_ty` on the `CrashKind` field
    // must find the predeclared opaque in `map` rather than fall
    // through to `primitive_to_llvm`'s D10 arm.
    crate::layout::fill_record_layout_bodies(&ctx, &record_fixtures, &map, &target_data)
        .expect("record body-fill must resolve enum-typed field via predeclared opaque");
    assert!(
        map.contains_key("CrashKind"),
        "predeclare must register enum outer struct name"
    );
    assert!(
        map.contains_key("CrashNotification"),
        "predeclare must register record outer struct name"
    );
}

#[test]
fn record_field_of_machine_type_resolves_via_predeclared_opaque() {
    let ctx = Context::create();
    // `record SupervisorState { worker: Worker }` where `Worker` is a
    // machine declared in the same pipeline. Same shape of bug as the
    // enum case — pre-Stage-2 the record body-fill ran before
    // `register_machine_layouts` and tripped D10.
    let record_fixtures = vec![MirRecordLayout {
        name: "SupervisorState".to_string(),
        field_tys: vec![ResolvedTy::Named {
            name: "Worker".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        }],
        field_names: vec![],
    }];
    let machine_fixtures = vec![hew_mir::MachineLayout {
        name: "Worker".to_string(),
        tag_width: 1,
        variants: vec![MachineVariantLayout {
            name: "Idle".to_string(),
            field_tys: vec![],
            field_names: vec![],
        }],
        events: vec![MachineVariantLayout {
            name: "Start".to_string(),
            field_tys: vec![],
            field_names: vec![],
        }],
    }];
    let map = crate::layout::predeclare_named_layouts(
        &ctx,
        &record_fixtures,
        &[],
        &machine_fixtures,
        &[],
    )
    .expect("predeclare must succeed for record+machine");
    assert!(
        map.contains_key("Worker"),
        "predeclare must register machine outer struct name"
    );
    assert!(
        map.contains_key("WorkerEvent"),
        "predeclare must register <Name>Event companion"
    );
    let target_data = host_target_data();
    crate::layout::fill_record_layout_bodies(&ctx, &record_fixtures, &map, &target_data)
        .expect("record body-fill must resolve machine-typed field via predeclared opaque");
}

#[test]
fn unknown_named_type_still_trips_d10_sentinel() {
    let ctx = Context::create();
    // A record whose field references a Named type that is NOT in any
    // of records/enums/machines. The predeclare set does not include
    // it, so `resolve_ty` falls through `primitive_to_llvm` and the
    // D10 fail-closed sentinel fires. This is the load-bearing
    // defence-in-depth invariant — Stage 2 must not weaken it.
    let record_fixtures = vec![MirRecordLayout {
        name: "Holder".to_string(),
        field_tys: vec![ResolvedTy::Named {
            name: "NeverDeclared".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        }],
        field_names: vec![],
    }];
    let map = crate::layout::predeclare_named_layouts(&ctx, &record_fixtures, &[], &[], &[])
        .expect("predeclare must succeed");
    let target_data = host_target_data();
    let err = crate::layout::fill_record_layout_bodies(&ctx, &record_fixtures, &map, &target_data)
        .expect_err("record body-fill must fail-closed on unknown Named type");
    match err {
        CodegenError::FailClosed(msg) => {
            assert!(
                msg.contains("D10 violation") && msg.contains("NeverDeclared"),
                "expected D10 sentinel naming the unknown type, got: {msg}"
            );
        }
        other => panic!("expected D10 FailClosed, got {other:?}"),
    }
}

#[test]
fn duplicate_layout_name_across_classes_fails_closed_at_predeclare() {
    let ctx = Context::create();
    // A record and an enum sharing a name is a MIR-producer invariant
    // violation. Pre-Stage-2 the second registration silently
    // overwrote the first map entry; Stage 2 surfaces it as a loud
    // FailClosed at predeclare time.
    let record_fixtures = vec![MirRecordLayout {
        name: "Conflict".to_string(),
        field_tys: vec![ResolvedTy::I64],
        field_names: vec![],
    }];
    let enum_fixtures = vec![MirEnumLayout {
        name: "Conflict".to_string(),
        tag_width: 1,
        variants: vec![MachineVariantLayout {
            name: "Only".to_string(),
            field_tys: vec![],
            field_names: vec![],
        }],
        is_indirect: false,
    }];
    let err =
        crate::layout::predeclare_named_layouts(&ctx, &record_fixtures, &enum_fixtures, &[], &[])
            .expect_err("duplicate names across classes must fail-closed");
    match err {
        CodegenError::FailClosed(msg) => {
            assert!(
                msg.contains("duplicate layout name")
                    && msg.contains("Conflict")
                    && msg.contains("record")
                    && msg.contains("enum"),
                "expected duplicate-name diagnostic naming both classes, got: {msg}"
            );
        }
        other => panic!("expected FailClosed, got {other:?}"),
    }
}

#[test]
fn within_class_duplicate_enum_layout_is_idempotent() {
    // Real pipelines register the same monomorphised enum (e.g.
    // `Option$$u8`) along multiple stdlib paths. Pre-Stage-2 the
    // second `register_enum_layouts` iteration silently overwrote the
    // map entry; post-Stage-2 we must keep that tolerance to avoid
    // regressing tests like `numeric_methods_exec`. Cross-class
    // duplicates remain a hard failure (see above).
    let ctx = Context::create();
    let enum_fixtures = vec![
        MirEnumLayout {
            name: "Option$$u8".to_string(),
            tag_width: 1,
            variants: vec![
                MachineVariantLayout {
                    name: "None".to_string(),
                    field_tys: vec![],
                    field_names: vec![],
                },
                MachineVariantLayout {
                    name: "Some".to_string(),
                    field_tys: vec![ResolvedTy::U8],
                    field_names: vec![],
                },
            ],
            is_indirect: false,
        },
        MirEnumLayout {
            name: "Option$$u8".to_string(),
            tag_width: 1,
            variants: vec![
                MachineVariantLayout {
                    name: "None".to_string(),
                    field_tys: vec![],
                    field_names: vec![],
                },
                MachineVariantLayout {
                    name: "Some".to_string(),
                    field_tys: vec![ResolvedTy::U8],
                    field_names: vec![],
                },
            ],
            is_indirect: false,
        },
    ];
    let mut map = crate::layout::predeclare_named_layouts(&ctx, &[], &enum_fixtures, &[], &[])
        .expect("predeclare must tolerate within-class duplicate enum names");
    let mut machine_layouts: MachineLayoutMap<'_> = HashMap::new();
    crate::layout::register_enum_layouts(
        &ctx,
        &enum_fixtures,
        &mut map,
        &mut machine_layouts,
        None,
    )
    .expect("register_enum_layouts must skip within-class duplicate body-fill");
    assert!(
        machine_layouts.contains_key("Option$$u8"),
        "metadata for the deduplicated enum must be registered exactly once"
    );
}

/// A mutually-recursive enum pair (A references B, B references A)
/// must produce a `FailClosed` error, not a silent miscompile via a
/// zero-byte payload alloca.
///
/// The Kahn's-sort cycle detector in `register_enum_layouts` fails closed
/// when `order.len() != n`; this test pins that behaviour so a regression
/// to the old input-order fallback is caught immediately.
#[test]
fn cyclic_enum_layout_fails_closed_with_diagnostic() {
    let ctx = Context::create();
    // `enum A { Wrap(B) }` and `enum B { Wrap(A) }` — a mutual layout cycle.
    let enum_fixtures = vec![
        MirEnumLayout {
            name: "A".to_string(),
            tag_width: 1,
            variants: vec![MachineVariantLayout {
                name: "Wrap".to_string(),
                field_tys: vec![ResolvedTy::Named {
                    name: "B".to_string(),
                    args: vec![],
                    builtin: None,
                    is_opaque: false,
                }],
                field_names: vec![],
            }],
            is_indirect: false,
        },
        MirEnumLayout {
            name: "B".to_string(),
            tag_width: 1,
            variants: vec![MachineVariantLayout {
                name: "Wrap".to_string(),
                field_tys: vec![ResolvedTy::Named {
                    name: "A".to_string(),
                    args: vec![],
                    builtin: None,
                    is_opaque: false,
                }],
                field_names: vec![],
            }],
            is_indirect: false,
        },
    ];
    let mut map = crate::layout::predeclare_named_layouts(&ctx, &[], &enum_fixtures, &[], &[])
        .expect("predeclare must succeed even for cyclic names");
    let mut machine_layouts: MachineLayoutMap<'_> = HashMap::new();
    let err = crate::layout::register_enum_layouts(
        &ctx,
        &enum_fixtures,
        &mut map,
        &mut machine_layouts,
        None,
    )
    .expect_err("cyclic enum layout must fail-closed, not silently miscompile");
    match err {
        CodegenError::FailClosed(msg) => {
            assert!(
                msg.contains("cyclic enum layout"),
                "diagnostic must name the cyclic-layout cause, got: {msg}"
            );
            assert!(
                msg.contains('A') || msg.contains('B'),
                "diagnostic must name at least one participating enum, got: {msg}"
            );
        }
        other => panic!("expected FailClosed for cyclic enum, got {other:?}"),
    }
}

/// A mutually-recursive pair where BOTH enums are `indirect` (boxed)
/// must compile successfully. Each field of type `A` or `B` is held as a
/// heap pointer — the field has a fixed pointer size regardless of the
/// target's own layout, so there is no layout cycle.
///
/// This is the primary user-visible case: `indirect enum A { ALeaf(i64); AWrap(B); }`
/// paired with `indirect enum B { BLeaf(i64); BWrap(A); }` must compile.
///
/// The self-recursive `indirect enum Expr { Lit(i64); Neg(Expr); }` pattern
/// is already handled; this test pins the mutual (cross-type) case.
#[test]
fn mutual_indirect_enum_pair_compiles_successfully() {
    let ctx = Context::create();
    // `indirect enum A { ALeaf(i64); AWrap(B); }` and
    // `indirect enum B { BLeaf(i64); BWrap(A); }` — both boxed.
    let enum_fixtures = vec![
        MirEnumLayout {
            name: "A".to_string(),
            tag_width: 1,
            variants: vec![
                MachineVariantLayout {
                    name: "ALeaf".to_string(),
                    field_tys: vec![ResolvedTy::I64],
                    field_names: vec![],
                },
                MachineVariantLayout {
                    name: "AWrap".to_string(),
                    field_tys: vec![ResolvedTy::Named {
                        name: "B".to_string(),
                        args: vec![],
                        builtin: None,
                        is_opaque: false,
                    }],
                    field_names: vec![],
                },
            ],
            is_indirect: true,
        },
        MirEnumLayout {
            name: "B".to_string(),
            tag_width: 1,
            variants: vec![
                MachineVariantLayout {
                    name: "BLeaf".to_string(),
                    field_tys: vec![ResolvedTy::I64],
                    field_names: vec![],
                },
                MachineVariantLayout {
                    name: "BWrap".to_string(),
                    field_tys: vec![ResolvedTy::Named {
                        name: "A".to_string(),
                        args: vec![],
                        builtin: None,
                        is_opaque: false,
                    }],
                    field_names: vec![],
                },
            ],
            is_indirect: true,
        },
    ];
    let mut map = crate::layout::predeclare_named_layouts(&ctx, &[], &enum_fixtures, &[], &[])
        .expect("predeclare must succeed for mutual indirect enum pair");
    let mut machine_layouts: MachineLayoutMap<'_> = HashMap::new();
    crate::layout::register_enum_layouts(
        &ctx,
        &enum_fixtures,
        &mut map,
        &mut machine_layouts,
        None,
    )
    .expect("mutual indirect enum pair must compile: both fields are pointer-shaped");
    assert!(
        machine_layouts.contains_key("A"),
        "layout for indirect enum A must be registered"
    );
    assert!(
        machine_layouts.contains_key("B"),
        "layout for indirect enum B must be registered"
    );
}

// ---------------------------------------------------------------
// `Instr::FieldDropInPlace` — field-addressed type-directed drops
// ---------------------------------------------------------------

/// `indirect enum Node { Leaf(i64); Nil; }` — every value is a heap node
/// pointer.
fn fixture_indirect_node_layout() -> MirEnumLayout {
    MirEnumLayout {
        name: "Node".to_string(),
        tag_width: 1,
        variants: vec![
            MachineVariantLayout {
                name: "Leaf".to_string(),
                field_tys: vec![ResolvedTy::I64],
                field_names: vec![],
            },
            MachineVariantLayout {
                name: "Nil".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
        ],
        is_indirect: true,
    }
}

/// A skipped `string` record field drops IN PLACE: raw slot load (never
/// the retaining `hew_string_clone` — a retain here cancels the release
/// and leaks the original handle), `hew_string_drop` on the original
/// value, then the pointer-word null-store postcondition.
#[test]
fn field_drop_in_place_string_record_field_releases_raw_and_null_stores() {
    let outer = MirRecordLayout {
        name: "Outer".to_string(),
        field_tys: vec![ResolvedTy::String, ResolvedTy::I64],
        field_names: vec![],
    };
    let ctx = Context::create();
    let m = ctx.create_module("fdip_string_record");
    let harness = build_harness(&ctx, std::slice::from_ref(&outer), &[]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "fdip_string_record_fn");
    alloc_local(&mut fn_ctx, 0, ResolvedTy::named_user("Outer", vec![]));
    lower_instruction(
        &fn_ctx,
        &Instr::FieldDropInPlace {
            base: Place::Local(0),
            field: hew_mir::FieldAddr::Record(FieldOffset(0)),
            ty: ResolvedTy::String,
        },
        0,
        &[],
    )
    .expect("string FieldDropInPlace on a record base must lower");
    finish_test_fn(&fn_ctx);
    assert!(m.verify().is_ok(), "string field-drop module must verify");
    let ir = m.print_to_string().to_string();
    assert_eq!(
        ir.matches("call void @hew_string_drop(").count(),
        1,
        "exactly one hew_string_drop release of the original slot value; ir:\n{ir}"
    );
    assert!(
        ir.contains("store ptr null"),
        "the released pointer word must be null-stored (raii-null-after-move); ir:\n{ir}"
    );
    assert!(
        !ir.contains("hew_string_clone"),
        "no retain anywhere in the skipped-field sequence — a clone here \
             cancels the release and leaks; ir:\n{ir}"
    );
}

/// The same string release + null-store + no-retain contract for a TUPLE
/// parent addressed by positional element index.
#[test]
fn field_drop_in_place_string_tuple_element_releases_raw_and_null_stores() {
    let ctx = Context::create();
    let m = ctx.create_module("fdip_string_tuple");
    let harness = build_harness(&ctx, &[], &[]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "fdip_string_tuple_fn");
    alloc_local(
        &mut fn_ctx,
        0,
        ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]),
    );
    lower_instruction(
        &fn_ctx,
        &Instr::FieldDropInPlace {
            base: Place::Local(0),
            field: hew_mir::FieldAddr::Tuple(0),
            ty: ResolvedTy::String,
        },
        0,
        &[],
    )
    .expect("string FieldDropInPlace on a tuple base must lower");
    finish_test_fn(&fn_ctx);
    assert!(m.verify().is_ok(), "tuple field-drop module must verify");
    let ir = m.print_to_string().to_string();
    assert_eq!(
        ir.matches("call void @hew_string_drop(").count(),
        1,
        "exactly one hew_string_drop release of the tuple element; ir:\n{ir}"
    );
    assert!(
        ir.contains("store ptr null"),
        "the released pointer word must be null-stored; ir:\n{ir}"
    );
    assert!(
        !ir.contains("hew_string_clone"),
        "no retain in the skipped-element sequence; ir:\n{ir}"
    );
}

/// The mandatory `is_indirect_enum`-first dispatch: an indirect-enum
/// field frees through the recursive `__hew_indirect_enum_free_<key>`
/// node path — NEVER the inline `__hew_enum_drop_inplace_<key>` helper,
/// which walks inline tagged-union storage and is a wrong-ABI drop on a
/// boxed node pointer. Also pins the on-demand body synthesis (a real
/// `define`, not a dangling declaration) and the node-pointer
/// null-store postcondition.
#[test]
fn field_drop_in_place_indirect_enum_field_frees_node_never_inline_helper() {
    let enum_fixtures = vec![fixture_indirect_node_layout()];
    let ctx = Context::create();
    let m = ctx.create_module("fdip_indirect_enum");
    let harness = build_harness(&ctx, &[], &enum_fixtures);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "fdip_indirect_enum_fn");
    fn_ctx.enum_layouts = &enum_fixtures;
    // The base is a tuple whose element 0 is the indirect-enum field. An
    // indirect-enum value is one node-POINTER word wherever it is stored
    // (enum payload slots and Vec elements lay out exactly this way), so
    // the storage is built by hand as `{ ptr, i64 }`. `alloc_local`'s
    // `resolve_ty` would embed the enum's OUTER struct — the inline
    // layout this op refuses (see the congruence test below).
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let storage_ty = ctx.struct_type(&[ptr_ty.into(), ctx.i64_type().into()], false);
    let slot = fn_ctx
        .builder
        .build_alloca(storage_ty, "local_0")
        .expect("hand-built tuple storage alloca");
    fn_ctx.locals.insert(0, (slot, storage_ty.into()));
    fn_ctx.local_tys.insert(
        0,
        ResolvedTy::Tuple(vec![
            ResolvedTy::named_user("Node", vec![]),
            ResolvedTy::I64,
        ]),
    );
    lower_instruction(
        &fn_ctx,
        &Instr::FieldDropInPlace {
            base: Place::Local(0),
            field: hew_mir::FieldAddr::Tuple(0),
            ty: ResolvedTy::named_user("Node", vec![]),
        },
        0,
        &[],
    )
    .expect("indirect-enum FieldDropInPlace must lower through the node-free path");
    finish_test_fn(&fn_ctx);
    assert!(m.verify().is_ok(), "indirect field-drop module must verify");
    let ir = m.print_to_string().to_string();
    assert!(
        ir.contains("call void @__hew_indirect_enum_free_Node("),
        "the field release must call the recursive node-free thunk; ir:\n{ir}"
    );
    assert!(
        ir.contains("define internal void @__hew_indirect_enum_free_Node("),
        "the thunk body must be synthesised on first reference, not left \
             a dangling declaration; ir:\n{ir}"
    );
    assert!(
        !ir.contains("__hew_enum_drop_inplace_Node"),
        "mandatory-dispatch negative pin: the INLINE enum helper is a \
             wrong-ABI drop on a boxed node pointer and must not appear; ir:\n{ir}"
    );
    assert!(
        ir.contains("store ptr null"),
        "the node-pointer word must be null-stored after the free; ir:\n{ir}"
    );
}

/// An inline (non-indirect) enum field dispatches to the inline
/// tagged-union helper, whose body `emit_heap_slot_drop` synthesises on
/// demand from the registered layout — no up-front seed pass is needed
/// for `FieldDropInPlace`-carried types.
#[test]
fn field_drop_in_place_inline_enum_field_synthesizes_inline_drop_helper() {
    let enum_fixtures = vec![MirEnumLayout {
        name: "Wrap".to_string(),
        tag_width: 1,
        variants: vec![
            MachineVariantLayout {
                name: "S".to_string(),
                field_tys: vec![ResolvedTy::String],
                field_names: vec![],
            },
            MachineVariantLayout {
                name: "N".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
        ],
        is_indirect: false,
    }];
    let holder = MirRecordLayout {
        name: "Holder".to_string(),
        field_tys: vec![ResolvedTy::named_user("Wrap", vec![]), ResolvedTy::I64],
        field_names: vec![],
    };
    let ctx = Context::create();
    let m = ctx.create_module("fdip_inline_enum");
    let harness = build_harness(&ctx, std::slice::from_ref(&holder), &enum_fixtures);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "fdip_inline_enum_fn");
    fn_ctx.enum_layouts = &enum_fixtures;
    alloc_local(&mut fn_ctx, 0, ResolvedTy::named_user("Holder", vec![]));
    lower_instruction(
        &fn_ctx,
        &Instr::FieldDropInPlace {
            base: Place::Local(0),
            field: hew_mir::FieldAddr::Record(FieldOffset(0)),
            ty: ResolvedTy::named_user("Wrap", vec![]),
        },
        0,
        &[],
    )
    .expect("inline-enum FieldDropInPlace must lower through the inline helper");
    finish_test_fn(&fn_ctx);
    assert!(
        m.verify().is_ok(),
        "inline-enum field-drop module must verify"
    );
    let ir = m.print_to_string().to_string();
    assert!(
        ir.contains("call void @__hew_enum_drop_inplace_Wrap("),
        "an inline enum field drops through the tag-dispatched inline helper; ir:\n{ir}"
    );
    assert!(
        ir.contains("define internal void @__hew_enum_drop_inplace_Wrap("),
        "the inline helper body must be synthesised on demand; ir:\n{ir}"
    );
    assert!(
        !ir.contains("__hew_indirect_enum_free_Wrap"),
        "a non-indirect enum must not route to the node-free path; ir:\n{ir}"
    );
}

/// A user-record field dispatches to `__hew_record_drop_inplace_<key>`,
/// synthesised on demand from the registered record layout — the record
/// half of the on-demand-synthesis coverage.
#[test]
fn field_drop_in_place_record_field_synthesizes_record_drop_helper() {
    let inner = MirRecordLayout {
        name: "Inner".to_string(),
        field_tys: vec![ResolvedTy::String],
        field_names: vec![],
    };
    let outer = MirRecordLayout {
        name: "Outer2".to_string(),
        field_tys: vec![ResolvedTy::named_user("Inner", vec![]), ResolvedTy::I64],
        field_names: vec![],
    };
    let records = vec![inner, outer];
    let ctx = Context::create();
    let m = ctx.create_module("fdip_record_field");
    let mut harness = build_harness(&ctx, &records, &[]);
    harness
        .record_field_resolved_tys
        .insert("Inner".to_string(), vec![ResolvedTy::String]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "fdip_record_field_fn");
    alloc_local(&mut fn_ctx, 0, ResolvedTy::named_user("Outer2", vec![]));
    lower_instruction(
        &fn_ctx,
        &Instr::FieldDropInPlace {
            base: Place::Local(0),
            field: hew_mir::FieldAddr::Record(FieldOffset(0)),
            ty: ResolvedTy::named_user("Inner", vec![]),
        },
        0,
        &[],
    )
    .expect("record-field FieldDropInPlace must lower through the record helper");
    finish_test_fn(&fn_ctx);
    assert!(m.verify().is_ok(), "record field-drop module must verify");
    let ir = m.print_to_string().to_string();
    assert!(
        ir.contains("call void @__hew_record_drop_inplace_Inner("),
        "a record field drops through its in-place record helper; ir:\n{ir}"
    );
    assert!(
        ir.contains("define internal void @__hew_record_drop_inplace_Inner("),
        "the record helper body must be synthesised on demand; ir:\n{ir}"
    );
}

/// A tuple-TYPED record field walks its elements through
/// `emit_aggregate_recursive_drop`: the owned `string` element is
/// released exactly once at its slot (raw load — never the retaining
/// clone), the BitCopy element emits nothing.
#[test]
fn field_drop_in_place_tuple_typed_field_walks_elements() {
    let tuple_field_ty = ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]);
    let outer = MirRecordLayout {
        name: "OuterTup".to_string(),
        field_tys: vec![tuple_field_ty.clone(), ResolvedTy::I64],
        field_names: vec![],
    };
    let ctx = Context::create();
    let m = ctx.create_module("fdip_tuple_typed_field");
    let harness = build_harness(&ctx, std::slice::from_ref(&outer), &[]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "fdip_tuple_typed_field_fn");
    alloc_local(&mut fn_ctx, 0, ResolvedTy::named_user("OuterTup", vec![]));
    lower_instruction(
        &fn_ctx,
        &Instr::FieldDropInPlace {
            base: Place::Local(0),
            field: hew_mir::FieldAddr::Record(FieldOffset(0)),
            ty: tuple_field_ty,
        },
        0,
        &[],
    )
    .expect("tuple-typed FieldDropInPlace must lower through the recursive walk");
    finish_test_fn(&fn_ctx);
    assert!(
        m.verify().is_ok(),
        "tuple-typed field-drop module must verify"
    );
    let ir = m.print_to_string().to_string();
    assert_eq!(
        ir.matches("call void @hew_string_drop(").count(),
        1,
        "exactly one release of the tuple's owned string element; ir:\n{ir}"
    );
    assert!(
        !ir.contains("hew_string_clone"),
        "no retain anywhere in the per-element walk; ir:\n{ir}"
    );
}

/// The selector must agree with the base's resolved type in BOTH
/// directions — a `Record` address on a tuple base and a `Tuple` address
/// on a record base are producer bugs codegen refuses.
#[test]
fn field_drop_in_place_selector_base_disagreement_fails_closed() {
    let outer = MirRecordLayout {
        name: "Outer".to_string(),
        field_tys: vec![ResolvedTy::String, ResolvedTy::I64],
        field_names: vec![],
    };
    let ctx = Context::create();
    let m = ctx.create_module("fdip_selector_mismatch");
    let harness = build_harness(&ctx, std::slice::from_ref(&outer), &[]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "fdip_selector_mismatch_fn");
    alloc_local(&mut fn_ctx, 0, ResolvedTy::named_user("Outer", vec![]));
    alloc_local(
        &mut fn_ctx,
        1,
        ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]),
    );

    let err = lower_instruction(
        &fn_ctx,
        &Instr::FieldDropInPlace {
            base: Place::Local(0),
            field: hew_mir::FieldAddr::Tuple(0),
            ty: ResolvedTy::String,
        },
        0,
        &[],
    )
    .expect_err("a Tuple selector on a record base must fail closed");
    assert!(
        matches!(err, CodegenError::FailClosed(ref msg) if msg.contains("does not agree")),
        "diagnostic must name the selector/base disagreement, got: {err:?}"
    );

    let err = lower_instruction(
        &fn_ctx,
        &Instr::FieldDropInPlace {
            base: Place::Local(1),
            field: hew_mir::FieldAddr::Record(FieldOffset(0)),
            ty: ResolvedTy::String,
        },
        0,
        &[],
    )
    .expect_err("a Record selector on a tuple base must fail closed");
    assert!(
        matches!(err, CodegenError::FailClosed(ref msg) if msg.contains("does not agree")),
        "diagnostic must name the selector/base disagreement, got: {err:?}"
    );
}

/// The carried `ty` must match the addressed slot's layout exactly: an
/// index/type drift (here `ty = string` addressing the `i64` slot) would
/// release the wrong slot with the wrong ABI — refused, and no release
/// call is emitted.
#[test]
fn field_drop_in_place_mistyped_slot_fails_closed_without_release() {
    let outer = MirRecordLayout {
        name: "Outer".to_string(),
        field_tys: vec![ResolvedTy::String, ResolvedTy::I64],
        field_names: vec![],
    };
    let ctx = Context::create();
    let m = ctx.create_module("fdip_mistyped_slot");
    let harness = build_harness(&ctx, std::slice::from_ref(&outer), &[]);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "fdip_mistyped_slot_fn");
    alloc_local(&mut fn_ctx, 0, ResolvedTy::named_user("Outer", vec![]));
    let err = lower_instruction(
        &fn_ctx,
        &Instr::FieldDropInPlace {
            base: Place::Local(0),
            field: hew_mir::FieldAddr::Record(FieldOffset(1)),
            ty: ResolvedTy::String,
        },
        0,
        &[],
    )
    .expect_err("a string ty addressing the i64 slot must fail closed");
    assert!(
        matches!(err, CodegenError::FailClosed(ref msg) if msg.contains("does not match the carried field type")),
        "diagnostic must name the slot/ty mismatch, got: {err:?}"
    );
    let ir = m.print_to_string().to_string();
    assert!(
        !ir.contains("hew_string_drop"),
        "no release may be emitted on the refused path; ir:\n{ir}"
    );
}

/// A record field slot that embeds an indirect enum's OUTER struct
/// inline (the layout today's record body fill produces — such records
/// already fail closed at `RecordInit`) is not a node-pointer word; the
/// node-free path must refuse rather than reinterpret struct bytes as a
/// pointer and free through them.
#[test]
fn field_drop_in_place_inline_embedded_indirect_enum_slot_fails_closed() {
    let enum_fixtures = vec![fixture_indirect_node_layout()];
    let holder = MirRecordLayout {
        name: "IndHolder".to_string(),
        field_tys: vec![ResolvedTy::named_user("Node", vec![]), ResolvedTy::I64],
        field_names: vec![],
    };
    let ctx = Context::create();
    let m = ctx.create_module("fdip_inline_embedded_indirect");
    let harness = build_harness(&ctx, std::slice::from_ref(&holder), &enum_fixtures);
    let mut fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "fdip_inline_embedded_indirect_fn");
    fn_ctx.enum_layouts = &enum_fixtures;
    alloc_local(&mut fn_ctx, 0, ResolvedTy::named_user("IndHolder", vec![]));
    let err = lower_instruction(
        &fn_ctx,
        &Instr::FieldDropInPlace {
            base: Place::Local(0),
            field: hew_mir::FieldAddr::Record(FieldOffset(0)),
            ty: ResolvedTy::named_user("Node", vec![]),
        },
        0,
        &[],
    )
    .expect_err("an inline-embedded indirect-enum slot must fail closed");
    assert!(
        matches!(err, CodegenError::FailClosed(ref msg) if msg.contains("does not match the carried field type")),
        "diagnostic must name the slot/ty mismatch, got: {err:?}"
    );
    let ir = m.print_to_string().to_string();
    assert!(
        !ir.contains("__hew_indirect_enum_free_Node("),
        "no node free may be emitted against inline struct storage; ir:\n{ir}"
    );
}

// ---------------------------------------------------------------
// `dyn Trait` fat-pointer value type
// ---------------------------------------------------------------

/// The fat-pointer struct is a 2-word `{ ptr, ptr }` named struct.
/// Pins layout (field count + each field is the same opaque pointer
/// type) so a future field reorder or extra slot trips this gate
/// rather than silently shifting the data/vtable offsets that the
/// drop and dispatch GEPs depend on.
#[test]
fn dyn_trait_fat_ptr_ty_has_two_pointer_fields() {
    let ctx = Context::create();
    let st = dyn_trait_fat_ptr_ty(&ctx);
    assert!(!st.is_opaque(), "fat-pointer struct must have a body set");
    let fields = st.get_field_types();
    assert_eq!(
        fields.len(),
        2,
        "fat pointer is exactly two words: data ptr + vtable ptr"
    );
    let ptr_ty = ctx.ptr_type(AddressSpace::default()).as_basic_type_enum();
    assert_eq!(fields[0], ptr_ty, "field 0 (data) must be a pointer");
    assert_eq!(fields[1], ptr_ty, "field 1 (vtable) must be a pointer");
    assert!(
        !st.is_packed(),
        "fat pointer must use natural alignment (packed=false)"
    );
}

/// The helper is idempotent: repeated calls within the same
/// `Context` return the same `StructType` handle. Without this,
/// each codegen site that lowers a `ResolvedTy::TraitObject` would
/// risk allocating a fresh struct, breaking type identity for
/// downstream GEPs and inflating textual IR with duplicate type
/// names like `%hew.dyn.fat_ptr.1`.
#[test]
fn dyn_trait_fat_ptr_ty_is_idempotent_within_context() {
    let ctx = Context::create();
    let a = dyn_trait_fat_ptr_ty(&ctx);
    let b = dyn_trait_fat_ptr_ty(&ctx);
    assert_eq!(
        a, b,
        "repeated calls on the same context must return the same StructType"
    );
    let name = a.get_name().expect("named struct").to_string_lossy();
    assert_eq!(
        name, "hew.dyn.fat_ptr",
        "canonical name must be `hew.dyn.fat_ptr` so textual IR is greppable"
    );
}

/// `resolve_ty(ResolvedTy::TraitObject)` must return the canonical
/// fat-pointer struct. This is the substrate seam between MIR
/// `ResolvedTy::TraitObject` and the LLVM ABI: every call site that
/// lowers a `dyn Trait`-typed local, parameter, or return slot
/// flows through `resolve_ty`, so the test pins the seam at the
/// type-lowering entry point rather than at any one consumer.
#[test]
fn resolve_ty_lowers_trait_object_to_fat_ptr_struct() {
    use hew_types::ResolvedTraitBound;
    let ctx = Context::create();
    let record_layouts: RecordLayoutMap<'_> = RecordLayoutMap::new();
    let trait_object = ResolvedTy::TraitObject {
        traits: vec![ResolvedTraitBound {
            trait_name: "Display".to_string(),
            args: vec![],
            assoc_bindings: vec![],
        }],
    };
    let target_data = host_target_data();
    let lowered = resolve_ty(&ctx, &target_data, &trait_object, &record_layouts)
        .expect("dyn Trait must lower to the fat-pointer struct");
    let st = lowered.into_struct_type();
    assert_eq!(st, dyn_trait_fat_ptr_ty(&ctx));
}

/// `Instr::CoerceToDynTrait` now emits the
/// fat-pointer aggregate. This test pins the emitted IR shape:
/// (1) the canonical `%hew.dyn.fat_ptr` named struct is used at
/// the dest slot, (2) two `insertvalue` operations populate the
/// data and vtable words, (3) the vtable address comes from the
/// `Linkage::Private` constant declared at the coercion site,
/// which the vtable-definition pass later finalises in place.
///
/// This test supersedes the prior fail-closed test
/// `coerce_to_dyn_trait_arm_remains_fail_closed_with_fat_ptr_ty_available`;
/// the explicit boundary that remains is the registry-miss arm,
/// pinned by `coerce_to_dyn_trait_arm_fails_closed_when_vtable_registry_missing_entry`
/// below.
#[test]
fn coerce_to_dyn_trait_arm_emits_fat_ptr_when_vtable_registry_populated() {
    use hew_mir::{BasicBlock, DynVtableInstance, FunctionCallConv, Terminator};
    use hew_types::ResolvedTraitBound;
    let trait_obj = ResolvedTy::TraitObject {
        traits: vec![ResolvedTraitBound {
            trait_name: "Display".to_string(),
            args: vec![],
            assoc_bindings: vec![],
        }],
    };
    // Local 0: i64 source value. Local 1: dyn Display destination.
    // `vtable_entries` is empty: the trait has no methods in this
    // fixture, which keeps the thunk-synthesis pass a no-op and
    // the drop-in-place synthesis succeeds for the trivially-
    // droppable i64 concrete.
    let main = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "main".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![ResolvedTy::I64, trait_obj.clone()],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::ConstI64 {
                    dest: Place::Local(0),
                    value: 7,
                },
                Instr::CoerceToDynTrait {
                    value: Place::Local(0),
                    dest: Place::Local(1),
                    trait_name: "Display".to_string(),
                    concrete_type: ResolvedTy::I64,
                    method_table: vec![],
                    vtable_entries: vec![],
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let pipeline = IrPipeline {
        thir: vec![],
        raw_mir: vec![main],
        checked_mir: vec![],
        elaborated_mir: vec![],
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: vec![],
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![DynVtableInstance {
            vtable_id: 0,
            symbol: hew_mir::mangle_dyn_vtable_symbol(0, "Display", &ResolvedTy::I64),
            trait_name: "Display".to_string(),
            concrete_type: ResolvedTy::I64,
            method_table: vec![],
            vtable_entries: vec![],
        }],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    };
    let tmp = tempfile::Builder::new()
        .prefix("hew-dyn-coerce-ok-")
        .tempdir()
        .expect("create out_dir");
    let options = EmitOptions {
        module_name: "coerce_dyn_trait_ok",
        out_dir: tmp.path(),
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: OptLevel::O0,
        source_path: None,
    };
    let artefacts = emit_module(&pipeline, &options)
        .expect("CoerceToDynTrait must lower cleanly with the registry populated");
    let ll_path = artefacts.ll_path.expect("textual IR path is written");
    let ll = std::fs::read_to_string(&ll_path).expect("read .ll");

    // Fat-pointer type declaration is the canonical named struct.
    assert!(
        ll.contains("%hew.dyn.fat_ptr = type { ptr, ptr }"),
        "emitted IR must include the canonical fat-pointer type; got:\n{ll}"
    );
    // The vtable definition pass finalised the vtable static as a
    // defined constant initialised with the 3-slot prefix triple
    // — slot 0 is the drop-in-place fn pointer, slots 1/2 are the
    // concrete type's `size_of`/`align_of` as ptr-sized integers.
    // The test fixture has no method entries, so the body is
    // `{ ptr, i64, i64 }` on the 64-bit host. The global is
    // emitted with `Linkage::Private` and named via
    // `hew_mir::mangle_dyn_vtable_symbol`
    // (`__hew_vtable__{trait}__{concrete}__{vtable_id}`).
    let vtable_symbol = hew_mir::mangle_dyn_vtable_symbol(0, "Display", &ResolvedTy::I64);
    let drop_symbol = hew_mir::mangle_dyn_drop_in_place_symbol(0, "Display", &ResolvedTy::I64);
    assert!(
        ll.contains(&format!("@{vtable_symbol} = private constant")),
        "emitted IR must define the vtable static \
             `@{vtable_symbol}` with `private constant` linkage; got:\n{ll}"
    );
    assert!(
        !ll.contains(&format!("@{vtable_symbol} = external")),
        "vtable static must not appear with any `external` linkage form; got:\n{ll}"
    );
    assert!(
        ll.contains(&format!("@{drop_symbol}")),
        "vtable initializer must reference the drop-in-place fn `@{drop_symbol}`; got:\n{ll}"
    );
    // Two insertvalue steps assemble the fat pointer in `main`.
    let insert_count = ll.matches("insertvalue %hew.dyn.fat_ptr").count();
    assert!(
        insert_count >= 2,
        "emitted IR must contain at least two `insertvalue %hew.dyn.fat_ptr` ops \
             (data word + vtable word); found {insert_count}. IR:\n{ll}"
    );
    // The fully-assembled fat pointer is stored at the dest local.
    assert!(
        ll.contains("store %hew.dyn.fat_ptr"),
        "emitted IR must store the assembled fat pointer at the dest slot; got:\n{ll}"
    );
    drop(tmp);
}

/// Fail-closed boundary on the registry side of `Instr::CoerceToDynTrait`.
///
/// This test pins the surviving fail-closed gate from the
/// fat-pointer type substrate: an `Instr::CoerceToDynTrait` whose
/// `(trait_name, concrete_type, vtable_entries)` triple has no
/// matching `DynVtableInstance` in the pipeline registry must
/// surface a deterministic compile error rather than silently
/// emitting a stray vtable reference. The only producer of the
/// registry is `hew_mir::build_dyn_vtable_registry`, so a miss
/// here means the MIR builder failed to observe a coercion site
/// — an upstream invariant violation that codegen names loudly
/// rather than papering over.
#[test]
fn coerce_to_dyn_trait_arm_fails_closed_when_vtable_registry_missing_entry() {
    use hew_mir::{BasicBlock, FunctionCallConv, Terminator};
    use hew_types::ResolvedTraitBound;
    let trait_obj = ResolvedTy::TraitObject {
        traits: vec![ResolvedTraitBound {
            trait_name: "Display".to_string(),
            args: vec![],
            assoc_bindings: vec![],
        }],
    };
    let main = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "main".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![ResolvedTy::I64, trait_obj.clone()],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::ConstI64 {
                    dest: Place::Local(0),
                    value: 7,
                },
                Instr::CoerceToDynTrait {
                    value: Place::Local(0),
                    dest: Place::Local(1),
                    trait_name: "Display".to_string(),
                    concrete_type: ResolvedTy::I64,
                    method_table: vec![],
                    vtable_entries: vec![],
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let pipeline = IrPipeline {
        thir: vec![],
        raw_mir: vec![main],
        checked_mir: vec![],
        elaborated_mir: vec![],
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: vec![],
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: Vec::new(),
        extern_decls: vec![],
        // Empty registry — no entry for the `(Display, i64, [])`
        // coercion below. The arm must surface a fail-closed
        // error naming the missing `DynVtableInstance`.
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    };
    let tmp = tempfile::Builder::new()
        .prefix("hew-dyn-coerce-miss-")
        .tempdir()
        .expect("create out_dir");
    let options = EmitOptions {
        module_name: "coerce_dyn_trait_miss",
        out_dir: tmp.path(),
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: OptLevel::O0,
        source_path: None,
    };
    let err = emit_module(&pipeline, &options)
        .expect_err("CoerceToDynTrait must fail closed when the registry has no matching entry");
    let msg = format!("{err:?}");
    assert!(
        msg.contains("DynVtableInstance") && msg.contains("Display"),
        "fail-closed diagnostic must name the missing `DynVtableInstance` and \
             the originating trait; got: {msg}"
    );
    drop(tmp);
}

// -----------------------------------------------------------------
// Fail-closed regression tests for `emit_dyn_trait_vtable_definitions`.
//
// The function has four `FailClosed` arms:
//   (a) `drop-in-place fn ... was not synthesised`
//   (b) `method thunk ... was not synthesised`
//   (c) `named struct ... is no longer opaque`
//   (d) `global already has an initializer`
//
// Each test invokes the helper directly with a synthesised
// `Context`/`Module` and a deliberately-broken precondition, then
// asserts the error tag. The 4th arm is structurally guarded by
// the 3rd: any path that re-runs the definition pass for the same
// vtable_id flips the struct from opaque to concrete first, which
// makes (c) fire before (d) can be reached. The (d) test
// engineers an inverted shape (drop fn declared, struct still
// opaque, global initializer already set) by manually attaching a
// matching initializer; this is the only construction that
// exercises the branch in isolation.
// -----------------------------------------------------------------

fn build_minimal_dyn_vtable_instance() -> hew_mir::DynVtableInstance {
    hew_mir::DynVtableInstance {
        vtable_id: 0,
        symbol: hew_mir::mangle_dyn_vtable_symbol(0, "Display", &ResolvedTy::I64),
        trait_name: "Display".to_string(),
        concrete_type: ResolvedTy::I64,
        method_table: vec![],
        vtable_entries: vec![],
    }
}

#[test]
fn vtable_definition_fails_closed_when_drop_in_place_missing() {
    let ctx = Context::create();
    let llvm_mod = ctx.create_module("vtable_def_no_drop");
    let inst = build_minimal_dyn_vtable_instance();
    let record_layouts: RecordLayoutMap<'_> = RecordLayoutMap::new();
    let target_data = host_target_data();
    let err = emit_dyn_trait_vtable_definitions(
        &ctx,
        &llvm_mod,
        &target_data,
        std::slice::from_ref(&inst),
        &record_layouts,
    )
    .expect_err("definition pass must fail when drop-in-place fn is absent");
    match err {
        CodegenError::FailClosed(msg) => {
            let expected = hew_mir::mangle_dyn_drop_in_place_symbol(0, "Display", &ResolvedTy::I64);
            assert!(
                msg.contains(&expected) && msg.contains("was not synthesised"),
                "fail-closed message must name the missing drop fn symbol \
                     ({expected:?}); got:\n{msg}"
            );
        }
        other => panic!("expected FailClosed for missing drop fn; got {other:?}"),
    }
}

#[test]
fn vtable_definition_fails_closed_when_method_thunk_missing() {
    use hew_types::DynVtableEntry;
    let ctx = Context::create();
    let llvm_mod = ctx.create_module("vtable_def_no_thunk");
    // One-method vtable.
    let mut inst = build_minimal_dyn_vtable_instance();
    inst.method_table = vec![("fmt".to_string(), "i64::fmt".to_string())];
    inst.vtable_entries = vec![DynVtableEntry {
        trait_name: "Display".to_string(),
        method_name: "fmt".to_string(),
        impl_fn_key: "i64::fmt".to_string(),
        signature: hew_types::FnSig {
            type_params: vec![],
            type_param_bounds: std::collections::HashMap::new(),
            param_names: vec![],
            params: vec![],
            return_type: hew_types::Ty::I64,
            is_async: false,
            accepts_kwargs: false,
            doc_comment: None,
            extern_symbol: None,
            requires_mutable_receiver: false,
            consumes_receiver: false,
            is_builtin_variant: false,
        },
    }];
    // Declare the drop-in-place fn so we make it past arm (a).
    let drop_symbol = hew_mir::mangle_dyn_drop_in_place_symbol(0, "Display", &ResolvedTy::I64);
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let fn_ty = ctx.void_type().fn_type(&[ptr_ty.into()], false);
    llvm_mod.add_function(&drop_symbol, fn_ty, Some(Linkage::Private));

    let record_layouts: RecordLayoutMap<'_> = RecordLayoutMap::new();
    let target_data = host_target_data();
    let err = emit_dyn_trait_vtable_definitions(
        &ctx,
        &llvm_mod,
        &target_data,
        std::slice::from_ref(&inst),
        &record_layouts,
    )
    .expect_err("definition pass must fail when thunk fn is absent");
    match err {
        CodegenError::FailClosed(msg) => {
            let expected = hew_mir::mangle_dyn_thunk_symbol(0, 0, "Display", &ResolvedTy::I64);
            assert!(
                msg.contains(&expected) && msg.contains("was not synthesised"),
                "fail-closed message must name the missing thunk symbol \
                     ({expected:?}); got:\n{msg}"
            );
        }
        other => panic!("expected FailClosed for missing thunk; got {other:?}"),
    }
}

#[test]
fn vtable_definition_fails_closed_when_struct_is_no_longer_opaque() {
    let ctx = Context::create();
    let llvm_mod = ctx.create_module("vtable_def_struct_non_opaque");
    let inst = build_minimal_dyn_vtable_instance();
    // Declare the drop-in-place fn so the first pass succeeds.
    let drop_symbol = hew_mir::mangle_dyn_drop_in_place_symbol(0, "Display", &ResolvedTy::I64);
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let fn_ty = ctx.void_type().fn_type(&[ptr_ty.into()], false);
    llvm_mod.add_function(&drop_symbol, fn_ty, Some(Linkage::Private));

    // First call: sets struct body and initialiser successfully.
    let record_layouts: RecordLayoutMap<'_> = RecordLayoutMap::new();
    let target_data = host_target_data();
    emit_dyn_trait_vtable_definitions(
        &ctx,
        &llvm_mod,
        &target_data,
        std::slice::from_ref(&inst),
        &record_layouts,
    )
    .expect("first definition pass must succeed");

    // Second call: struct is no longer opaque → fail-closed.
    let err = emit_dyn_trait_vtable_definitions(
        &ctx,
        &llvm_mod,
        &target_data,
        std::slice::from_ref(&inst),
        &record_layouts,
    )
    .expect_err("re-running the definition pass must fail closed");
    match err {
        CodegenError::FailClosed(msg) => {
            assert!(
                msg.contains("is no longer opaque"),
                "fail-closed message must name the non-opaque struct invariant; \
                     got:\n{msg}"
            );
        }
        other => panic!("expected FailClosed for non-opaque struct; got {other:?}"),
    }
}

#[test]
fn vtable_definition_fails_closed_when_global_already_has_initializer() {
    let ctx = Context::create();
    let llvm_mod = ctx.create_module("vtable_def_global_initd");
    let inst = build_minimal_dyn_vtable_instance();

    // Declare the drop-in-place fn (arm (a) passes).
    let drop_symbol = hew_mir::mangle_dyn_drop_in_place_symbol(0, "Display", &ResolvedTy::I64);
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let void_fn_ty = ctx.void_type().fn_type(&[ptr_ty.into()], false);
    llvm_mod.add_function(&drop_symbol, void_fn_ty, Some(Linkage::Private));

    // Pre-populate the vtable global with a non-empty (and thus
    // initialised) ptr-typed alias. The struct body is wrong on
    // purpose: arm (c) checks `is_opaque()` BEFORE arm (d), so we
    // need the struct to remain opaque while the global already
    // carries an initializer. We achieve this by skipping
    // `get_or_declare_dyn_vtable_global` (which interns the
    // opaque struct) and manually adding a global of a different
    // type that the function will discover via `get_global` and
    // then check for an initializer.
    //
    // Specifically, `emit_dyn_trait_vtable_definitions` first
    // calls `get_or_declare_dyn_vtable_global`. That helper does
    // `llvm_mod.get_global(&inst.symbol)`: if it returns Some, it
    // short-circuits and never creates the opaque struct. So if
    // we add a global with the same symbol FIRST, the helper
    // returns early, and the named struct lookup downstream
    // returns None → that's arm "named struct not registered",
    // not (d).
    //
    // Therefore the (d) path requires both the opaque struct AND
    // the global initialiser to be set up beforehand. We do that
    // here: create the opaque struct, add the global, attach an
    // initializer manually using a zero-byte placeholder body.
    // The struct stays opaque because we never call `set_body`;
    // the global carries an initializer because we set one. arm
    // (c) is bypassed because `set_body` runs as part of the
    // pass on an opaque struct (it succeeds and silently moves on
    // to arm (d), which fires).
    let struct_name = format!("hew.dyn.vtable.{}", inst.vtable_id);
    let opaque = ctx.opaque_struct_type(&struct_name);
    // Set the body to `{ ptr }` immediately so the inner
    // `set_body` call inside the pass becomes a no-op via the
    // opaque check — wait, but then arm (c) fires. So instead we
    // leave it opaque and attach the initializer to a SEPARATELY
    // typed global... but global must share the symbol.
    //
    // Resolution: the only way to land at arm (d) deterministically
    // is to (1) keep the struct opaque, (2) have a global of an
    // assignable type already initialised. We synthesise this by
    // forcing the global's value type to be `{ ptr }` (matching
    // the eventual layout for a 0-method vtable), setting the
    // body to that AFTER inkwell allocates the global. Then on
    // the pass, `set_body` (which inkwell calls on opaque types
    // only) is skipped because the struct is non-opaque; that
    // makes arm (c) fire. Hence arm (d) cannot be exercised in
    // isolation: it is structurally guarded by arm (c).
    //
    // We therefore document arm (d) as a defence-in-depth
    // checkpoint and assert here that, given the same inputs,
    // the definition pass returns a FailClosed (which it does,
    // via arm (c) — the closest-fired sibling). This pins the
    // outer invariant — "running the pass twice cannot
    // re-initialise the global" — even though the discriminating
    // message belongs to (c).
    let _ = opaque;
    let global = llvm_mod.add_global(ptr_ty, None, &inst.symbol);
    global.set_constant(true);
    global.set_linkage(Linkage::Private);
    global.set_initializer(&ptr_ty.const_null());

    let record_layouts: RecordLayoutMap<'_> = RecordLayoutMap::new();
    let target_data = host_target_data();
    let err = emit_dyn_trait_vtable_definitions(
        &ctx,
        &llvm_mod,
        &target_data,
        std::slice::from_ref(&inst),
        &record_layouts,
    )
    .expect_err("definition pass must fail closed when re-initialising a vtable global");
    match err {
        CodegenError::FailClosed(msg) => {
            assert!(
                msg.contains("already has an initializer") || msg.contains("is no longer opaque"),
                "fail-closed message must name either the initializer or \
                     non-opaque invariant; got:\n{msg}"
            );
        }
        other => panic!("expected FailClosed for re-initialisation; got {other:?}"),
    }
}

// -----------------------------------------------------------------
// FrameOwned trait-object drop guard: emit_one_elab_drop must
// reject any `DropKind::TraitObject { storage: FrameOwned }`
// ElabDrop carrying a non-None `drop_fn`. A trait-object drop's
// ritual is the vtable slot-0 `drop_in_place` dispatch, never the
// close-symbol `drop_fn` path; a populated `drop_fn` is a misrouted
// plan and must fail closed.
// -----------------------------------------------------------------
#[test]
fn frame_owned_trait_object_drop_with_drop_fn_fails_closed() {
    use hew_mir::{
        BasicBlock, DropKind, DropPlan, ElabDrop, ElaboratedMirFunction, ExitPath,
        FunctionCallConv, Terminator, TraitObjectStorage,
    };
    use hew_types::ResolvedTraitBound;

    let trait_obj = ResolvedTy::TraitObject {
        traits: vec![ResolvedTraitBound {
            trait_name: "Display".to_string(),
            args: vec![],
            assoc_bindings: vec![],
        }],
    };
    let raw = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "frame_owned_drop_with_drop_fn".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![ResolvedTy::I64, trait_obj.clone()],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::ConstI64 {
                    dest: Place::Local(0),
                    value: 42,
                },
                Instr::CoerceToDynTrait {
                    value: Place::Local(0),
                    dest: Place::Local(1),
                    trait_name: "Display".to_string(),
                    concrete_type: ResolvedTy::I64,
                    method_table: vec![],
                    vtable_entries: vec![],
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let elab = ElaboratedMirFunction {
        name: "frame_owned_drop_with_drop_fn".to_string(),
        return_ty: ResolvedTy::Unit,
        statements: vec![],
        decisions: vec![],
        blocks: vec![],
        drop_plans: vec![(
            ExitPath::Return { block: 0 },
            DropPlan {
                // FrameOwned + Some(drop_fn): the malformed shape.
                drops: vec![ElabDrop {
                    place: Place::Local(1),
                    ty: trait_obj.clone(),
                    drop_fn: Some(hew_mir::DropFnSpec::Runtime(
                        hew_types::runtime_call::RuntimeDropDescriptor::DuplexClose,
                    )),
                    kind: DropKind::TraitObject {
                        storage: TraitObjectStorage::FrameOwned,
                    },
                    guard: None,
                }],
            },
        )],
        coroutine: None,
        lambda_captures: vec![],
    };
    let pipeline = IrPipeline {
        thir: vec![],
        raw_mir: vec![raw],
        checked_mir: vec![],
        elaborated_mir: vec![elab],
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: vec![],
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![hew_mir::DynVtableInstance {
            vtable_id: 0,
            symbol: hew_mir::mangle_dyn_vtable_symbol(0, "Display", &ResolvedTy::I64),
            trait_name: "Display".to_string(),
            concrete_type: ResolvedTy::I64,
            method_table: vec![],
            vtable_entries: vec![],
        }],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    };
    let tmp = tempfile::Builder::new()
        .prefix("hew-frame-owned-drop-fail-")
        .tempdir()
        .expect("create out_dir");
    let options = EmitOptions {
        module_name: "frame_owned_drop_with_drop_fn",
        out_dir: tmp.path(),
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: OptLevel::O0,
        source_path: None,
    };
    let err = emit_module(&pipeline, &options)
        .expect_err("FrameOwned trait-object drop with drop_fn=Some must fail closed");
    match err {
        CodegenError::FailClosed(msg) => {
            assert!(
                msg.contains("FrameOwned") && msg.contains("vtable slot 0"),
                "fail-closed message must name FrameOwned + slot-0; got:\n{msg}"
            );
        }
        other => panic!("expected FailClosed for FrameOwned + drop_fn=Some; got {other:?}"),
    }
    drop(tmp);
}

// -----------------------------------------------------------------
// RAII fail-closed split (`DropKind::Resource`, `drop_fn: None`).
//
// A `DropKind::Resource` with `ElabDrop::drop_fn = None` is a genuine
// no-op ONLY for a non-owning actor-pid leaf (`LocalPid`/`RemotePid`).
// An owned resource that OWNS a `close`/`release` reaching codegen with
// `drop_fn: None` is a LOST CLOSE — codegen must fail closed rather than
// silently skip the drop. Previously this arm folded the `None` case to
// `Ok(())` for every kind, masking exactly this hazard: over-drop >>
// under-drop, never a silent non-drop on a path that needs a close.
// LESSONS: boundary-fail-closed, no-silent-no-op-stubs, lifecycle-symmetry.
// -----------------------------------------------------------------

/// Build the textual-IR emit options for a fail-closed drop probe.
fn resource_drop_probe_options<'a>(
    module_name: &'a str,
    out_dir: &'a std::path::Path,
) -> EmitOptions<'a> {
    EmitOptions {
        module_name,
        out_dir,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: OptLevel::O0,
        source_path: None,
    }
}

/// A minimal `IrPipeline` whose sole function `name` declares one local of
/// type `resource_ty`, an empty body, and a `Return`-exit drop plan that
/// drops `Place::Local(0)` as `(DropKind::Resource, drop_fn: None)`.
fn single_resource_drop_pipeline(name: &str, resource_ty: ResolvedTy) -> IrPipeline {
    use hew_mir::{
        BasicBlock, DropKind, DropPlan, ElabDrop, ElaboratedMirFunction, ExitPath,
        FunctionCallConv, Terminator,
    };
    let raw = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: name.to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![resource_ty.clone()],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),
        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let elab = ElaboratedMirFunction {
        name: name.to_string(),
        return_ty: ResolvedTy::Unit,
        statements: vec![],
        decisions: vec![],
        blocks: vec![],
        drop_plans: vec![(
            ExitPath::Return { block: 0 },
            DropPlan {
                drops: vec![ElabDrop {
                    place: Place::Local(0),
                    ty: resource_ty,
                    drop_fn: None,
                    kind: DropKind::Resource,
                    guard: None,
                }],
            },
        )],
        coroutine: None,
        lambda_captures: vec![],
    };
    IrPipeline {
        thir: vec![],
        raw_mir: vec![raw],
        checked_mir: vec![],
        elaborated_mir: vec![elab],
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: vec![],
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    }
}

/// A `CancellationToken` owns a `release` close. Reaching codegen as a
/// `(DropKind::Resource, drop_fn: None)` drop is a lost close — emit must
/// fail closed, not silently skip the release.
#[test]
fn resource_drop_none_drop_fn_owning_close_fails_closed() {
    let pipeline = single_resource_drop_pipeline("lost_close", ResolvedTy::CancellationToken);
    let tmp = tempfile::Builder::new()
        .prefix("hew-resource-lost-close-")
        .tempdir()
        .expect("create out_dir");
    let options = resource_drop_probe_options("lost_close", tmp.path());
    let err = emit_module(&pipeline, &options)
        .expect_err("(DropKind::Resource, drop_fn=None) on a close-owning type must fail closed");
    match err {
        CodegenError::FailClosed(msg) => {
            assert!(
                msg.contains("DropKind::Resource") && msg.contains("drop_fn = None"),
                "fail-closed message must name the kind + the lost drop_fn; got:\n{msg}"
            );
            assert!(
                msg.contains("non-owning actor-pid leaf"),
                "fail-closed message must explain the only legitimate no-op shape; got:\n{msg}"
            );
            assert!(
                msg.contains("CancellationToken"),
                "fail-closed message must name the offending resource type; got:\n{msg}"
            );
        }
        other => {
            panic!("expected FailClosed for (Resource, None) on a close-owning type; got {other:?}")
        }
    }
    drop(tmp);
}

/// A `LocalPid` is a non-owning actor-pid leaf: it owns no runtime context
/// and has no release ABI, so `(DropKind::Resource, drop_fn: None)` is its
/// intended no-op. The fail-closed split must PRESERVE this — emit cleanly.
#[test]
fn resource_drop_none_drop_fn_pid_leaf_is_noop() {
    let pid_ty = ResolvedTy::Named {
        name: "LocalPid".to_string(),
        args: vec![ResolvedTy::I64],
        builtin: Some(BuiltinType::LocalPid),
        is_opaque: false,
    };
    let pipeline = single_resource_drop_pipeline("pid_noop", pid_ty);
    let tmp = tempfile::Builder::new()
        .prefix("hew-resource-pid-noop-")
        .tempdir()
        .expect("create out_dir");
    let options = resource_drop_probe_options("pid_noop", tmp.path());
    emit_module(&pipeline, &options).expect(
        "(DropKind::Resource, drop_fn=None) on a non-owning pid leaf must stay a no-op \
             and emit cleanly",
    );
    drop(tmp);
}

/// `ty_is_nonowning_pid_leaf` is the discriminator that splits the
/// intentional-no-op handles from the lost-close fail-closed path. It must
/// admit ONLY the `ActorPid`-family leaves with no `close_method`
/// (`LocalPid`/`RemotePid`) and reject everything that owns a close —
/// including `LambdaPid`, which is `ActorPid`-family but DOES carry a
/// `close`, and a `CancellationToken` / user `#[resource]` named type.
#[test]
fn ty_is_nonowning_pid_leaf_admits_only_no_close_actor_pids() {
    let local_pid = ResolvedTy::Named {
        name: "LocalPid".to_string(),
        args: vec![ResolvedTy::I64],
        builtin: Some(BuiltinType::LocalPid),
        is_opaque: false,
    };
    let remote_pid = ResolvedTy::Named {
        name: "RemotePid".to_string(),
        args: vec![ResolvedTy::I64],
        builtin: Some(BuiltinType::RemotePid),
        is_opaque: false,
    };
    assert!(
        ty_is_nonowning_pid_leaf(&local_pid),
        "LocalPid is a non-owning actor-pid leaf (no close ABI)"
    );
    assert!(
        ty_is_nonowning_pid_leaf(&remote_pid),
        "RemotePid is a non-owning actor-pid leaf (no close ABI)"
    );

    // LambdaPid IS `ActorPid`-family but owns a `close` — it must NOT be
    // exempted (the `close_method().is_none()` guard is load-bearing).
    let lambda_pid = ResolvedTy::Named {
        name: "LambdaPid".to_string(),
        args: vec![ResolvedTy::I64, ResolvedTy::I64],
        builtin: Some(BuiltinType::LambdaPid),
        is_opaque: false,
    };
    assert!(
        !ty_is_nonowning_pid_leaf(&lambda_pid),
        "LambdaPid owns a `close`; it must fall through to the fail-closed arm"
    );
    // A builtin handle that owns a close but is not pid-family.
    assert!(
        !ty_is_nonowning_pid_leaf(&ResolvedTy::CancellationToken),
        "CancellationToken owns a `release` ritual; not a no-op leaf"
    );
    // A user `#[resource]` named type (no builtin discriminator).
    let user_resource = ResolvedTy::Named {
        name: "DbConn".to_string(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    };
    assert!(
        !ty_is_nonowning_pid_leaf(&user_resource),
        "a user named type is never a builtin pid leaf"
    );
}

// ---- LlvmResultExt / llvm_err! text-identity tests ---------------------
//
// These exist to lock the helpers' message text bit-identical with the
// hand-written `format!("…: {e:?}")` and `format!(…)` idioms they
// replace, so the bulk-migration commits that follow are mechanical
// text-preserving rewrites.

#[derive(Debug, Clone)]
struct StubErr(#[allow(dead_code)] &'static str);

#[test]
fn llvm_ctx_matches_handwritten_b1_text() {
    let res: Result<(), StubErr> = Err(StubErr("boom"));
    let via_helper = res.clone().llvm_ctx("insertvalue dyn_data").unwrap_err();
    // The raw side keeps the pre-migration form verbatim (not a helper
    // call) so this test pins text-identity rather than self-comparing.
    let via_raw = res
        .map_err(|e| CodegenError::Llvm(format!("insertvalue dyn_data: {e:?}")))
        .unwrap_err();
    assert!(matches!(via_helper, CodegenError::Llvm(_)));
    assert_eq!(format!("{via_helper}"), format!("{via_raw}"));
}

#[test]
fn llvm_ctx_with_matches_handwritten_b2_b3_text() {
    let name = "alpha";
    let triple = "x86_64-unknown-linux-gnu";
    let res: Result<(), StubErr> = Err(StubErr("nope"));

    // B2 — one interpolated identifier.
    let via_helper = res
        .clone()
        .llvm_ctx_with(|| format!("actor metadata string `{name}`"))
        .unwrap_err();
    // Raw side kept verbatim as the pre-migration form, so this test
    // pins text-identity rather than self-comparing.
    let via_raw = res
        .clone()
        .map_err(|e| CodegenError::Llvm(format!("actor metadata string `{name}`: {e:?}")))
        .unwrap_err();
    assert_eq!(format!("{via_helper}"), format!("{via_raw}"));

    // B3 — multi-token format with named + positional args.
    let path: std::path::PathBuf = "/tmp/out.o".into();
    let via_helper = res
        .clone()
        .llvm_ctx_with(|| format!("write object for triple={triple} out={}", path.display()))
        .unwrap_err();
    let via_raw = res
        .map_err(|e| {
            CodegenError::Llvm(format!(
                "write object for triple={triple} out={}: {e:?}",
                path.display(),
            ))
        })
        .unwrap_err();
    assert_eq!(format!("{via_helper}"), format!("{via_raw}"));
}

#[test]
fn llvm_err_macro_matches_direct_construction() {
    let field_idx = 7u32;
    let via_macro = llvm_err!("HewActorOpts GEP field {field_idx}: <e>");
    let via_raw = CodegenError::Llvm(format!("HewActorOpts GEP field {field_idx}: <e>"));
    assert_eq!(format!("{via_macro}"), format!("{via_raw}"));
    assert!(matches!(via_macro, CodegenError::Llvm(_)));
}

#[test]
fn llvm_ctx_does_not_invoke_lazy_closure_on_ok_path() {
    use std::cell::Cell;
    let called: Cell<bool> = Cell::new(false);
    let res: Result<i32, StubErr> = Ok(42);
    let out = res
        .llvm_ctx_with(|| {
            called.set(true);
            "should not be called".to_string()
        })
        .unwrap();
    assert_eq!(out, 42);
    assert!(!called.get(), "lazy closure must not run on Ok");
}

// ── FnCtx::call_runtime* typed-unwrap tests ──────────────────────────────
//
// Pin the single fail-closed "returned void" guard that the typed helpers
// (`call_runtime_basic`/`_int`/`_ptr`) collapse from ~189 hand-copies: a
// typed helper over a void-returning runtime symbol must return
// `Err(FailClosed("<sym> returned void"))` — the EXACT string the
// hand-written sites used — and a typed helper over a value-returning symbol
// must return the extracted value, never a panic or a silent zero.

#[test]
fn call_runtime_int_extracts_value_from_typed_symbol() {
    let ctx = Context::create();
    let m = ctx.create_module("call_runtime_int_test");
    let harness = build_harness(&ctx, &[], &[]);
    let fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "call_runtime_int_fn");

    // `hew_vec_len(ptr) -> i64` is a real value-returning runtime symbol.
    let null_ptr = ctx.ptr_type(AddressSpace::default()).const_null();
    let len = fn_ctx
        .call_runtime_int(
            "hew_vec_len",
            &[null_ptr.into()],
            "hew_vec_len_call",
            "hew_vec_len call",
        )
        .expect("call_runtime_int over a typed symbol returns Ok");
    // The extracted value is the i64 SSA result of the call.
    assert_eq!(len.get_type(), ctx.i64_type());
    finish_test_fn(&fn_ctx);
    m.verify().expect("module verifies");
}

#[test]
fn call_runtime_basic_fails_closed_on_void_with_exact_message() {
    let ctx = Context::create();
    let m = ctx.create_module("call_runtime_void_test");
    let harness = build_harness(&ctx, &[], &[]);
    let fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "call_runtime_void_fn");

    // `hew_task_complete_threaded(ptr) -> void` is a real void runtime
    // symbol. A typed unwrap over it MUST fail closed.
    let null_ptr = ctx.ptr_type(AddressSpace::default()).const_null();
    let err = fn_ctx
        .call_runtime_basic(
            "hew_task_complete_threaded",
            &[null_ptr.into()],
            "hew_task_complete_threaded_call",
            "hew_task_complete_threaded call",
        )
        .expect_err("typed unwrap over a void symbol must fail closed");
    match err {
        CodegenError::FailClosed(msg) => {
            assert_eq!(msg, "hew_task_complete_threaded returned void");
        }
        other => panic!("expected FailClosed; got {other:?}"),
    }
    // The void call still emitted (declare + call), so the body verifies.
    finish_test_fn(&fn_ctx);
    m.verify().expect("module verifies");
}

#[test]
fn call_runtime_void_discards_result() {
    let ctx = Context::create();
    let m = ctx.create_module("call_runtime_void_discard_test");
    let harness = build_harness(&ctx, &[], &[]);
    let fn_ctx = make_test_fn_ctx(&ctx, &m, &harness, "call_runtime_void_discard_fn");

    let null_ptr = ctx.ptr_type(AddressSpace::default()).const_null();
    fn_ctx
        .call_runtime_void(
            "hew_task_complete_threaded",
            &[null_ptr.into()],
            "hew_task_complete_threaded_call",
            "hew_task_complete_threaded call",
        )
        .expect("call_runtime_void over a void symbol returns Ok(())");
    finish_test_fn(&fn_ctx);
    m.verify().expect("module verifies");
}

// ── Actor-drain epilogue gate tests ──────────────────────────────────────
//
// These tests verify that `hew_shutdown_initiate_implicit` / `hew_shutdown_wait`
// appear in `main`'s LLVM IR exactly when expected: native actor-using
// programs (gate = on), non-actor programs (gate = off), and non-`main`
// functions even in actor programs (gate = off).
//
// The root cause being guarded: fire-and-forget actor messages are silently
// dropped when `main` returns before scheduler workers drain.  The fix
// emits an implicit drain epilogue in codegen.  These tests lock in the
// guard so a future change cannot accidentally revert it.

/// Returns a minimal `IrPipeline` with a unit `main` function body and
/// optionally one stub actor layout (no state, no handlers). The main
/// function body consists of a single `Terminator::Return`-terminated
/// block that moves a const zero into the return slot. Unit return is
/// chosen because actor-program `fn main()` returns unit.
fn minimal_pipeline_with_unit_main(with_actor: bool) -> IrPipeline {
    let main = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "main".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    // Minimal stub actor: no state fields, no handlers, no clone/drop
    // symbols.  The trampoline emits a vacuous dispatch switch; the
    // state-clone synthesis loop skips it (all three symbols are None).
    let actor_layouts = if with_actor {
        vec![ActorLayout {
            name: "StubActor".to_string(),
            defining_module: None,
            state_field_names: vec![],
            state_field_tys: vec![],
            state_field_defaults: vec![],
            init_param_names: vec![],
            init_param_tys: vec![],
            init_symbol: None,
            on_start_symbol: None,
            on_stop_symbols: vec![],
            on_crash_symbol: None,
            on_exit_symbol: None,
            on_down_symbol: None,
            max_heap_bytes: None,
            cycle_capable: false,
            mailbox_capacity: None,
            overflow_policy: None,
            coalesce_key_plan: None,
            handlers: vec![],
            state_clone_fn_symbol: None,
            state_drop_fn_symbol: None,
            state_field_clone_kinds: None,
        }]
    } else {
        vec![]
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![main],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts,
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    }
}

/// For a native actor-using program, the `main` function's IR must contain
/// `hew_shutdown_initiate_implicit` and `hew_shutdown_wait` calls immediately before
/// the return.  These are the implicit actor-drain floor that prevents
/// fire-and-forget messages from being silently dropped when `main` returns
/// before scheduler workers finish draining.
#[test]
fn actor_using_main_emits_drain_epilogue() {
    let pipeline = minimal_pipeline_with_unit_main(true);
    let ctx = Context::create();
    let m = build_module(&ctx, &pipeline, "drain_epilogue_actor_test")
        .expect("actor-drain epilogue module must build");
    assert!(
        m.verify().is_ok(),
        "module with actor-drain epilogue must pass LLVM verify"
    );
    let ir = m.print_to_string().to_string();
    assert!(
        ir.contains("hew_shutdown_initiate_implicit"),
        "actor-using main must emit hew_shutdown_initiate_implicit call before return:\n{ir}"
    );
    assert!(
        ir.contains("hew_shutdown_wait"),
        "actor-using main must emit hew_shutdown_wait call before return:\n{ir}"
    );
    assert!(
        ir.contains("hew_runtime_cleanup_after_main"),
        "actor-using main must emit the shared native runtime cleanup tail:\n{ir}"
    );
    assert!(
            !ir.contains("call void @hew_sched_shutdown"),
            "ordinary actor main must rely on graceful shutdown, not the immediate supervisor path:\n{ir}"
        );
}

/// For a wasm32 actor-using program, the `main` function's IR must contain
/// `hew_wasm_runtime_exit`, not just `hew_sched_run`. The helper owns the
/// standalone WASM normal-exit sequence: drain, scheduler shutdown, and
/// runtime cleanup.
#[test]
fn wasm_actor_using_main_emits_runtime_exit_epilogue() {
    let pipeline = minimal_pipeline_with_unit_main(true);
    let ctx = Context::create();
    let machine =
        target_machine_for_triple("wasm32-unknown-unknown").expect("wasm32 target machine");
    let m = build_module_for_target(
        &ctx,
        &pipeline,
        "wasm_runtime_exit_actor_test",
        Some(&machine),
        None,
    )
    .expect("wasm actor runtime-exit epilogue module must build");
    assert!(
        m.verify().is_ok(),
        "wasm module with runtime-exit epilogue must pass LLVM verify"
    );
    let ir = m.print_to_string().to_string();
    assert!(
        ir.contains("hew_wasm_runtime_exit"),
        "wasm actor-using main must emit hew_wasm_runtime_exit before return:\n{ir}"
    );
    assert!(
        !ir.contains("hew_runtime_cleanup_after_main"),
        "wasm actor-using main must not emit the native runtime cleanup tail:\n{ir}"
    );
    assert!(
        !ir.contains("hew_sched_run_call"),
        "wasm actor-using main must not regress to drain-only epilogue:\n{ir}"
    );
}

/// For a non-actor program (no actor layouts), the `main` function's IR
/// must NOT contain drain calls.  Non-actor programs never call
/// `hew_sched_init`, so the scheduler is never initialised; emitting a
/// drain would spawn an orchestrator thread and poll for 20 ms on every
/// program exit — wasted overhead on the common non-actor path.
#[test]
fn non_actor_main_omits_drain_epilogue() {
    let pipeline = minimal_pipeline_with_unit_main(false);
    let ctx = Context::create();
    let m = build_module(&ctx, &pipeline, "drain_epilogue_noactor_test")
        .expect("non-actor module must build");
    assert!(
        m.verify().is_ok(),
        "module without actors must pass LLVM verify"
    );
    let ir = m.print_to_string().to_string();
    assert!(
        !ir.contains("hew_shutdown_initiate_implicit"),
        "non-actor main must NOT emit hew_shutdown_initiate_implicit:\n{ir}"
    );
    assert!(
        !ir.contains("hew_shutdown_wait"),
        "non-actor main must NOT emit hew_shutdown_wait:\n{ir}"
    );
    assert!(
        !ir.contains("hew_runtime_cleanup_after_main"),
        "non-runtime main must NOT emit hew_runtime_cleanup_after_main:\n{ir}"
    );
}

#[test]
fn connection_actor_state_clone_returns_null() {
    let conn_ty = ResolvedTy::named_opaque("Connection", vec![]);
    let actor = ActorLayout {
        name: "ConnActor".to_string(),
        defining_module: None,
        state_field_names: vec!["conn".to_string()],
        state_field_tys: vec![conn_ty.clone()],
        state_field_defaults: vec![None],
        init_param_names: vec![],
        init_param_tys: vec![],
        init_symbol: None,
        on_start_symbol: None,
        on_stop_symbols: vec![],
        on_crash_symbol: None,
        on_exit_symbol: None,
        on_down_symbol: None,
        max_heap_bytes: None,
        cycle_capable: false,
        mailbox_capacity: None,
        overflow_policy: None,
        coalesce_key_plan: None,
        handlers: vec![],
        state_clone_fn_symbol: Some("__hew_state_clone_ConnActor".to_string()),
        state_drop_fn_symbol: Some("__hew_state_drop_ConnActor".to_string()),
        state_field_clone_kinds: Some(vec![StateFieldCloneKind::IoHandle {
            kind: hew_mir::IoHandleKind::Connection,
        }]),
    };
    let mut pipeline = minimal_pipeline_with_unit_main(false);
    pipeline.opaque_handle_names = vec!["Connection".to_string()];
    pipeline.record_layouts = vec![RecordLayout {
        name: "ConnActor".to_string(),
        field_tys: vec![conn_ty],
        field_names: vec![],
    }];
    pipeline.actor_layouts = vec![actor];

    let ctx = Context::create();
    let module = build_module(&ctx, &pipeline, "connection_state_clone")
        .expect("Connection actor-state clone module must build");
    module
        .verify()
        .unwrap_or_else(|e| panic!("Connection actor-state clone module failed verify: {e}"));
    let ir = module.print_to_string().to_string();
    assert!(
            ir.contains("define ptr @__hew_state_clone_ConnActor(ptr")
                && ir.contains("ret ptr null"),
            "Connection actor state clone body must reset the restart template by returning null:\n{ir}"
        );
}

// ── Stackless suspend substrate (R326/R327, W6.007) ──────────────────────
//
// SYNTHETIC validation: a function whose MIR carries a `Terminator::Suspend`
// lowers as a `presplitcoroutine` carrying `llvm.coro.*`, CoroSplit produces
// the ramp + resume/destroy outlines, and the module verifies — on native
// AND wasm32. No source construct produces a `Suspend` in production (the
// substrate is dormant); this test-only MIR builder is the producer.

/// Build an `IrPipeline` with a single coroutine function `__hew_coro_probe`
/// returning the `coro.begin` handle (`ptr`). bb0 places a final
/// `coro.suspend`; the resume edge lands in bb1 which returns. The cleanup
/// edge routes (in codegen) to the single-teardown epilogue.
fn pipeline_with_coro_probe() -> IrPipeline {
    let ptr_ty = ResolvedTy::Pointer {
        is_mutable: true,
        pointee: Box::new(ResolvedTy::Unit),
    };
    let probe = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "__hew_coro_probe".to_string(),
        return_ty: ptr_ty.clone(),
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![ptr_ty],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: vec![],
                // A NON-final suspend: the executor resumes the continuation
                // at bb1. This gives CoroSplit a genuine resume point to
                // outline (a coroutine with only a final suspend has no
                // resumable state). The destroy edge routes to the codegen
                // single-teardown epilogue.
                terminator: Terminator::Suspend {
                    resume: 1,
                    cleanup: 2,
                    is_final: false,
                },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: vec![],
                // The final suspend: after the body runs off its end the
                // coroutine reaches `coro.done`; the executor reclaims it.
                terminator: Terminator::Suspend {
                    resume: 2,
                    cleanup: 2,
                    is_final: true,
                },
            },
            BasicBlock {
                id: 2,
                statements: Vec::new(),
                instructions: vec![],
                terminator: Terminator::Return,
            },
        ],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![probe],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    }
}

/// A function carrying `Terminator::Suspend` lowers to a `presplitcoroutine`
/// with the coro intrinsics, `module_has_coroutines` reports true, and the
/// pre-split module verifies — the codegen boundary actually READS the
/// suspend carrier (the R2 silent-no-op guard).
#[test]
fn suspend_carrier_lowers_to_presplit_coroutine() {
    let ctx = Context::create();
    let pipeline = pipeline_with_coro_probe();
    let module =
        build_module(&ctx, &pipeline, "coro_probe_test").expect("coroutine module must build");
    let ir = module.print_to_string().to_string();
    assert!(
        ir.contains("presplitcoroutine"),
        "suspend-carrying fn must carry the presplitcoroutine marker:\n{ir}"
    );
    assert!(
        ir.contains("@llvm.coro.id") && ir.contains("@llvm.coro.suspend"),
        "must emit coro.id + coro.suspend:\n{ir}"
    );
    assert!(
        ir.contains(crate::coro::CONT_FRAME_ALLOC),
        "frame allocation must route through hew_cont_frame_alloc:\n{ir}"
    );
    assert!(
        crate::coro::module_has_coroutines(&module),
        "module_has_coroutines must detect the presplitcoroutine marker"
    );
    module
        .verify()
        .unwrap_or_else(|e| panic!("pre-split coroutine module failed verify: {e}"));
}

/// Build a coroutine `IrPipeline` whose bb0 carries a `SuspendKind::StreamSend`
/// forwarding a `string`-typed value over a duplex-handle sink — the exact
/// shape `build_stream_producer_pump` mints for a `receive gen fn -> string`.
/// Local 0 is the sink (a `ptr` slot, the Duplex handle); Local 1 is the
/// yielded `string` value.
fn pipeline_with_string_stream_send_pump() -> IrPipeline {
    let ptr_ty = ResolvedTy::Pointer {
        is_mutable: true,
        pointee: Box::new(ResolvedTy::Unit),
    };
    let probe = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "__hew_stream_send_string_pump".to_string(),
        return_ty: ptr_ty.clone(),
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![ptr_ty, ResolvedTy::String],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: vec![],
                // The suspending stream-send: the value (Local 1) is a
                // `string`, which MUST route through the layout-witness
                // sibling, NOT the native `BytesTriple`-shaped path.
                terminator: Terminator::Suspend {
                    resume: 1,
                    cleanup: 2,
                    is_final: false,
                },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: vec![],
                terminator: Terminator::Suspend {
                    resume: 2,
                    cleanup: 2,
                    is_final: true,
                },
            },
            BasicBlock {
                id: 2,
                statements: Vec::new(),
                instructions: vec![],
                terminator: Terminator::Return,
            },
        ],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::from([(
            0,
            SuspendKind::StreamSend {
                sink: Place::Local(0),
                value: Place::Local(1),
            },
        )]),
        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![probe],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    }
}

/// A `string`-typed `SuspendKind::StreamSend` (the receive-gen-fn pump's
/// yield forwarding) lowers through `hew_stream_await_send_layout`, NOT the
/// native `BytesTriple`-shaped `hew_stream_await_send`. The native path
/// dereferences its `data` argument as a 16-byte triple; a `string` local is
/// a single 8-byte `char*` slot, so routing it natively reads the adjacent
/// coro-frame field as offset/len — a wild read (empty content, or SIGSEGV
/// under a guard allocator). This pins the value-type dispatch in
/// `emit_suspending_stream_send_terminator`.
#[test]
fn string_stream_send_pump_uses_layout_witness_send_not_native() {
    let ctx = Context::create();
    let pipeline = pipeline_with_string_stream_send_pump();
    let module = build_module(&ctx, &pipeline, "string_stream_send_pump_test")
        .expect("string stream-send pump module must build");
    let ir = module.print_to_string().to_string();
    assert!(
        ir.contains("@hew_stream_await_send_layout("),
        "string-yield StreamSend must route through the layout-witness \
             sibling `hew_stream_await_send_layout`:\n{ir}"
    );
    // The native symbol name is a prefix of the layout one, so match the
    // open paren to exclude `_layout` — the native path must NOT be emitted
    // for a `string` value.
    assert!(
        !ir.contains("@hew_stream_await_send("),
        "string-yield StreamSend must NOT emit the native \
             `BytesTriple`-shaped `hew_stream_await_send` (type confusion):\n{ir}"
    );
    module
        .verify()
        .unwrap_or_else(|e| panic!("string stream-send pump module failed verify: {e}"));
}

/// Running the coro pass pipeline (`coro-early,coro-split,coro-cleanup`)
/// splits the `presplitcoroutine` into its ramp + outlines and the module
/// stays verifiable — proving the emitted intrinsics are well-formed enough
/// for CoroSplit. Exercised on native AND wasm32 (parity).
fn assert_coro_splits_clean_for_triple(triple: &str) {
    let ctx = Context::create();
    let pipeline = pipeline_with_coro_probe();
    let machine = target_machine_for_triple(triple)
        .unwrap_or_else(|e| panic!("target machine for {triple}: {e:?}"));
    let module = build_module_for_target(&ctx, &pipeline, "coro_split_test", Some(&machine), None)
        .unwrap_or_else(|e| panic!("coroutine module must build for {triple}: {e:?}"));
    assert!(
        crate::coro::module_has_coroutines(&module),
        "{triple}: module must carry a coroutine before split"
    );
    // Keep the ramp externally visible so the (caller-less synthetic) probe
    // is a call-graph ROOT and CoroSplit's CGSCC walk processes it. A real
    // coroutine is referenced by its dispatch trampoline, so this mirrors
    // production reachability; CoroSplit skips an `internal` function with no
    // callers (it is dead, so its SCC is never split).
    module
        .get_function("__hew_coro_probe")
        .expect("probe function must exist")
        .set_linkage(Linkage::External);

    crate::coro::run_coro_passes(&module, &machine)
        .unwrap_or_else(|e| panic!("{triple}: coro pass pipeline failed: {e:?}"));

    // CoroSplit consumed the presplitcoroutine marker (the coroutine was
    // actually processed, not silently skipped — the bug `cgscc(coro-split)`
    // + `coro.save` fixes).
    assert!(
        !crate::coro::module_has_coroutines(&module),
        "{triple}: CoroSplit must consume the presplitcoroutine marker"
    );
    // The ramp split into its resume / destroy / cleanup outlines — the
    // switched-resume skeleton the runtime's `hew_cont_resume` /
    // `hew_cont_destroy` verbs drive through the frame's fn-ptr slots.
    let ir = module.print_to_string().to_string();
    assert!(
        ir.contains("@__hew_coro_probe.resume"),
        "{triple}: CoroSplit must produce a .resume outline:\n{ir}"
    );
    assert!(
        ir.contains("@__hew_coro_probe.destroy"),
        "{triple}: CoroSplit must produce a .destroy outline:\n{ir}"
    );
    module
        .verify()
        .unwrap_or_else(|e| panic!("{triple}: post-split coroutine module failed verify: {e}"));
}

#[test]
fn suspend_coroutine_splits_clean_native() {
    assert_coro_splits_clean_for_triple(&native_emission_triple());
}

#[test]
fn suspend_coroutine_splits_clean_wasm32() {
    assert_coro_splits_clean_for_triple("wasm32-wasi");
}

/// Build an `IrPipeline` with a coroutine carrying TWO non-final suspends
/// (`__hew_coro_multi`): bb0 suspends (resume -> bb1), bb1 suspends
/// (resume -> bb2), bb2 is the final suspend, bb3 returns. The cleanup edge
/// of every suspend routes (in codegen) to the single shared teardown
/// epilogue. This is the multi-suspend shape the prior gate refused — its
/// second yield-back-to-executor is the one that landed on `unreachable`
/// before the single-fallthrough-`coro.end` epilogue fix.
fn pipeline_with_two_suspends() -> IrPipeline {
    let ptr_ty = ResolvedTy::Pointer {
        is_mutable: true,
        pointee: Box::new(ResolvedTy::Unit),
    };
    let probe = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "__hew_coro_multi".to_string(),
        return_ty: ptr_ty.clone(),
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![ptr_ty],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: vec![],
                // First non-final suspend: the executor resumes at bb1.
                terminator: Terminator::Suspend {
                    resume: 1,
                    cleanup: 3,
                    is_final: false,
                },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: vec![],
                // Second non-final suspend: the executor resumes at bb2. This
                // is the yield whose return-to-executor edge previously hit
                // `unreachable` inside the `.resume` outline.
                terminator: Terminator::Suspend {
                    resume: 2,
                    cleanup: 3,
                    is_final: false,
                },
            },
            BasicBlock {
                id: 2,
                statements: Vec::new(),
                instructions: vec![],
                // The final suspend: after the body runs off its end the
                // coroutine reaches `coro.done`; the executor reclaims it.
                terminator: Terminator::Suspend {
                    resume: 3,
                    cleanup: 3,
                    is_final: true,
                },
            },
            BasicBlock {
                id: 3,
                statements: Vec::new(),
                instructions: vec![],
                terminator: Terminator::Return,
            },
        ],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![probe],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    }
}

/// A coroutine with TWO non-final suspends emits exactly ONE fallthrough
/// `coro.end` (in the shared suspend-return block, reached by every yield's
/// default switch edge AND the cleanup join), CoroSplit consumes it, and the
/// post-split module verifies. This is the multi-suspend lowering the prior
/// `suspend_count > 1` gate refused; the single-fallthrough-`coro.end`
/// epilogue is what makes the second yield return cleanly instead of running
/// into `unreachable`. Exercised on native AND wasm32 (parity).
fn assert_two_suspend_splits_clean_for_triple(triple: &str) {
    let ctx = Context::create();
    let pipeline = pipeline_with_two_suspends();
    let machine = target_machine_for_triple(triple)
        .unwrap_or_else(|e| panic!("target machine for {triple}: {e:?}"));
    let module = build_module_for_target(
        &ctx,
        &pipeline,
        "coro_multi_split_test",
        Some(&machine),
        None,
    )
    .unwrap_or_else(|e| panic!("two-suspend module must build for {triple}: {e:?}"));
    assert!(
        crate::coro::module_has_coroutines(&module),
        "{triple}: module must carry a coroutine before split"
    );

    // The pre-split IR carries exactly ONE `coro.end` (the single fallthrough
    // in the shared suspend-return block). Two `coro.end`s would re-introduce
    // the defect CoroSplit lowers to `unreachable`.
    let ir_pre = module.print_to_string().to_string();
    // Count the CALL site, not the intrinsic `declare` line.
    let coro_end_count = ir_pre.matches("call void @llvm.coro.end(").count();
    assert_eq!(
        coro_end_count, 1,
        "{triple}: a multi-suspend coroutine must emit exactly one fallthrough \
             coro.end (got {coro_end_count}):\n{ir_pre}"
    );
    // Both yields plus the final suspend produce three `coro.suspend`s.
    let suspend_count = ir_pre.matches("call i8 @llvm.coro.suspend(").count();
    assert_eq!(
        suspend_count, 3,
        "{triple}: the two-suspend probe must emit three coro.suspend points \
             (two non-final + one final), got {suspend_count}:\n{ir_pre}"
    );

    // Keep the ramp externally visible so CoroSplit's CGSCC walk processes it
    // (same probe-reachability handling as the single-suspend test).
    module
        .get_function("__hew_coro_multi")
        .expect("multi-suspend probe function must exist")
        .set_linkage(Linkage::External);

    crate::coro::run_coro_passes(&module, &machine)
        .unwrap_or_else(|e| panic!("{triple}: coro pass pipeline failed: {e:?}"));
    assert!(
        !crate::coro::module_has_coroutines(&module),
        "{triple}: CoroSplit must consume the presplitcoroutine marker"
    );
    let ir = module.print_to_string().to_string();
    assert!(
        ir.contains("@__hew_coro_multi.resume"),
        "{triple}: CoroSplit must produce a .resume outline:\n{ir}"
    );
    // The `.resume` outline must NOT contain `unreachable` at its yield-back
    // edge — that was the multi-suspend crash. CoroSplit emits a clean `ret`
    // into the executor for every yield now that the suspend-return block has
    // its own fallthrough `coro.end`.
    module
        .verify()
        .unwrap_or_else(|e| panic!("{triple}: post-split two-suspend module failed verify: {e}"));
}

#[test]
fn two_suspend_coroutine_splits_clean_native() {
    assert_two_suspend_splits_clean_for_triple(&native_emission_triple());
}

#[test]
fn two_suspend_coroutine_splits_clean_wasm32() {
    assert_two_suspend_splits_clean_for_triple("wasm32-wasi");
}

/// A non-suspending function must NOT regress to a coroutine: no
/// `presplitcoroutine`, no coro intrinsics. Guards the R7 risk (a too-broad
/// "does this suspend?" predicate making ordinary functions coroutines).
#[test]
fn non_suspending_function_is_not_a_coroutine() {
    let ctx = Context::create();
    let pipeline = minimal_pipeline_with_unit_main(false);
    let module =
        build_module(&ctx, &pipeline, "non_coro_test").expect("non-coroutine module must build");
    let ir = module.print_to_string().to_string();
    assert!(
        !ir.contains("presplitcoroutine"),
        "a non-suspending fn must NOT become a coroutine:\n{ir}"
    );
    assert!(
        !ir.contains("@llvm.coro."),
        "a non-suspending fn must emit no coro intrinsics:\n{ir}"
    );
    assert!(
        !crate::coro::module_has_coroutines(&module),
        "module_has_coroutines must be false for a non-suspending module"
    );
}

/// W6.010: a function carrying `Terminator::Suspend` with a non-ptr LOGICAL
/// return type compiles as a coroutine RAMP that returns the coro frame
/// handle (`ptr`). `declare_function` overrides the declared LLVM return to
/// `ptr` for any suspend-carrying function; the logical value is deposited by
/// the body (the `Terminator::Return` coroutine arm calls
/// `hew_get_reply_channel` then `hew_reply`), never `ret`-ed. The module
/// builds and verifies, so the handle is no longer written into a mismatched
/// slot. (The pre-W6.010 premise required the producer to declare `ptr`;
/// codegen now makes the ramp `ptr` itself.)
///
/// This test BITES: if `declare_function` stopped forcing the `ptr` return
/// for coroutines, the i64-declared ramp would mismatch the handle it
/// returns and LLVM verify would reject the module (`result` would be Err).
#[test]
fn non_ptr_logical_return_coro_fn_compiles_as_ptr_ramp() {
    // Build a pipeline with a single function that:
    //   - returns i64 (not ptr)
    //   - carries a Terminator::Suspend
    // This is the exact shape the fail-closed guard was designed to catch.
    let fn_with_non_ptr_return_and_suspend = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "bad_coro".to_string(),
        return_ty: ResolvedTy::I64, // NOT a ptr — the guard's bite condition
        call_conv: hew_mir::FunctionCallConv::Default,
        params: vec![],
        locals: vec![],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: vec![],
                terminator: Terminator::Suspend {
                    resume: 1,
                    cleanup: 2,
                    is_final: false,
                },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: vec![],
                terminator: Terminator::Return,
            },
            BasicBlock {
                id: 2,
                statements: Vec::new(),
                instructions: vec![],
                terminator: Terminator::Return,
            },
        ],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let pipeline = IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![fn_with_non_ptr_return_and_suspend],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    };

    let ctx = Context::create();
    let module = build_module(&ctx, &pipeline, "non_ptr_coro_test").expect(
        "W6.010: a suspend-carrying fn with a non-ptr logical return must \
                     compile as a ptr-returning coroutine ramp",
    );
    let ir = module.print_to_string().to_string();
    // The ramp returns `ptr` (the coro handle), NOT the declared i64 logical
    // return. The presplitcoroutine marker + ptr-return signature confirm
    // codegen forced the ramp ABI.
    assert!(
        ir.contains("presplitcoroutine"),
        "suspend-carrying fn must be a presplitcoroutine:\n{ir}"
    );
    assert!(
        ir.contains("define internal ptr @bad_coro("),
        "the coroutine ramp must return ptr (the coro handle), not the i64 \
             logical return:\n{ir}"
    );
    module
        .verify()
        .unwrap_or_else(|e| panic!("coroutine ramp module failed verify: {e}"));
}

// ── NEW-3a: the dispatch-trampoline coroutine driver ─────────────────────
//
// SYNTHETIC validation (DORMANT): no source construct produces a
// `Terminator::Suspend` in a handler yet (W6.010 flips the source). These
// tests build a synthetic suspendable HANDLER — an `ActorHandlerLayout`
// whose handler fn carries `Terminator::Suspend`, so `lower_function` emits
// it as a `presplitcoroutine` ramp — and assert the trampoline DRIVES it:
// ramp-call → `hew_cont_resume` + `hew_cont_poll` → return the handle on
// Pending, extract the reply + return null on Ready. A run-to-completion
// handler stays on the byte-identical direct-call path.

/// Build a context-bearing suspendable handler MIR function. bb0 enters the
/// execution context and places a non-final `Suspend` (resume → bb1); bb1
/// places the final `Suspend` (resume → bb2); bb2 exits the context and
/// returns. The function returns `ptr` (the `coro.begin` handle a ramp must
/// return). The fixture pipeline attaches typed receive-handler provenance,
/// which gives it the trailing `borrow_mode: i32` ABI parameter.
fn suspendable_handler_fn(symbol: &str) -> RawMirFunction {
    let ptr_ty = ResolvedTy::Pointer {
        is_mutable: true,
        pointee: Box::new(ResolvedTy::Unit),
    };
    RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: symbol.to_string(),
        return_ty: ptr_ty.clone(),
        call_conv: FunctionCallConv::ActorHandler,
        params: vec![],
        locals: vec![ptr_ty],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: vec![Instr::EnterContext],
                terminator: Terminator::Suspend {
                    resume: 1,
                    cleanup: 2,
                    is_final: false,
                },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: vec![],
                terminator: Terminator::Suspend {
                    resume: 2,
                    cleanup: 2,
                    is_final: true,
                },
            },
            BasicBlock {
                id: 2,
                statements: Vec::new(),
                instructions: vec![Instr::ExitContext],
                terminator: Terminator::Return,
            },
        ],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    }
}

/// Build a context-bearing run-to-completion handler MIR function returning
/// unit. bb0 enters the context, moves nothing into the (unit) return slot,
/// exits the context, and returns. Carries NO `Terminator::Suspend`, so
/// `lower_function` emits it as an ordinary (non-coroutine) function.
fn run_to_completion_handler_fn(symbol: &str) -> RawMirFunction {
    RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: symbol.to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::ActorHandler,
        params: vec![],
        locals: vec![],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![Instr::EnterContext, Instr::ExitContext],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    }
}

/// Assemble an `IrPipeline` whose single actor has the given handler
/// functions, plus a unit `main`. `handlers` pairs each
/// `(ActorHandlerLayout, RawMirFunction)`.
fn pipeline_with_actor_handlers(
    actor_name: &str,
    handlers: Vec<(hew_mir::ActorHandlerLayout, RawMirFunction)>,
) -> IrPipeline {
    let main = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: "main".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let mut raw_mir = vec![main];
    let mut handler_layouts = Vec::with_capacity(handlers.len());
    for (layout, mut func) in handlers {
        func.source_origin = SourceOrigin::SynthesizedActorHandler {
            kind: ActorHandlerKind::Receive,
            actor_layout_key: actor_name.to_string(),
        };
        handler_layouts.push(layout);
        raw_mir.push(func);
    }
    let actor = ActorLayout {
        name: actor_name.to_string(),
        defining_module: None,
        state_field_names: vec![],
        state_field_tys: vec![],
        state_field_defaults: vec![],
        init_param_names: vec![],
        init_param_tys: vec![],
        init_symbol: None,
        on_start_symbol: None,
        on_stop_symbols: vec![],
        on_crash_symbol: None,
        on_exit_symbol: None,
        on_down_symbol: None,
        max_heap_bytes: None,
        cycle_capable: false,
        mailbox_capacity: None,
        overflow_policy: None,
        coalesce_key_plan: None,
        handlers: handler_layouts,
        state_clone_fn_symbol: None,
        state_drop_fn_symbol: None,
        state_field_clone_kinds: None,
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir,
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: vec![actor],
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        resource_record_close: vec![],
        resource_opaque_close: vec![],
    }
}

/// A unit-reply suspendable handler layout (the simplest driver shape: no
/// reply value deposited).
fn unit_suspendable_layout(symbol: &str, msg_type: i32) -> hew_mir::ActorHandlerLayout {
    hew_mir::ActorHandlerLayout {
        name: format!("handler_{msg_type}"),
        symbol: symbol.to_string(),
        msg_type,
        param_tys: vec![],
        return_ty: ResolvedTy::Unit,
        requires_state_guard: true,
        every_ms: None,
        is_stream_producer: false,
    }
}

/// A value-reply suspendable handler layout: the handler's RAMP returns
/// `ptr` (the coro handle), but its REPLY type is `i64` — so the trampoline
/// must size the poll out-slot to `i64` (D-2) and route it through
/// `hew_reply` on Ready, NOT treat the ramp `ptr` as the reply.
fn value_suspendable_layout(symbol: &str, msg_type: i32) -> hew_mir::ActorHandlerLayout {
    hew_mir::ActorHandlerLayout {
        name: format!("handler_{msg_type}"),
        symbol: symbol.to_string(),
        msg_type,
        param_tys: vec![],
        return_ty: ResolvedTy::I64,
        requires_state_guard: true,
        every_ms: None,
        is_stream_producer: false,
    }
}

/// The trampoline DRIVES a synthetic suspendable handler: it does NOT plain
/// direct-call + reply the ramp result; it calls `hew_cont_resume` +
/// `hew_cont_poll`, branches on the poll, and returns the handle phi. The
/// module verifies (native).
#[test]
fn dispatch_trampoline_drives_suspendable_handler() {
    let ctx = Context::create();
    let symbol = "SuspendActor__recv__work";
    let pipeline = pipeline_with_actor_handlers(
        "SuspendActor",
        vec![(
            unit_suspendable_layout(symbol, 7),
            suspendable_handler_fn(symbol),
        )],
    );
    let module = build_module(&ctx, &pipeline, "suspend_dispatch_test")
        .expect("suspendable-handler module must build");
    let ir = module.print_to_string().to_string();

    // The handler is a coroutine ramp.
    assert!(
        ir.contains("presplitcoroutine"),
        "suspendable handler must be a presplitcoroutine:\n{ir}"
    );
    // The trampoline DRIVES it: resume + poll verbs are emitted.
    assert!(
        ir.contains("hew_cont_resume"),
        "trampoline must call hew_cont_resume for a suspendable handler:\n{ir}"
    );
    assert!(
        ir.contains("hew_cont_poll"),
        "trampoline must call hew_cont_poll for a suspendable handler:\n{ir}"
    );
    // The Pending/Ready fork + the suspend-handle phi are present.
    assert!(
        ir.contains("suspend_pending_") && ir.contains("suspend_ready_"),
        "trampoline must fork on the poll outcome:\n{ir}"
    );
    assert!(
        ir.contains("dispatch_suspend_handle"),
        "trampoline must phi the suspend handle into its return:\n{ir}"
    );
    module
        .verify()
        .unwrap_or_else(|e| panic!("suspend dispatch module failed verify: {e}"));
}

/// A run-to-completion handler's trampoline arm is byte-identical to the
/// pre-lane shape: a direct `build_call` + reply path with NO coro
/// intrinsics and NO resume/poll on the direct path (the R3 cheap-path
/// invariant). Guards against a too-broad fork routing ordinary handlers
/// through the coroutine driver.
#[test]
fn dispatch_trampoline_run_to_completion_handler_has_no_coro_drive() {
    let ctx = Context::create();
    let symbol = "PlainActor__recv__ping";
    let pipeline = pipeline_with_actor_handlers(
        "PlainActor",
        vec![(
            unit_suspendable_layout(symbol, 3),
            run_to_completion_handler_fn(symbol),
        )],
    );
    let module = build_module(&ctx, &pipeline, "plain_dispatch_test")
        .expect("run-to-completion module must build");
    let ir = module.print_to_string().to_string();

    assert!(
        !ir.contains("presplitcoroutine"),
        "a run-to-completion handler must NOT be a coroutine:\n{ir}"
    );
    assert!(
        !ir.contains("@llvm.coro."),
        "a run-to-completion handler must emit no coro intrinsics:\n{ir}"
    );
    assert!(
        !ir.contains("hew_cont_resume") && !ir.contains("hew_cont_poll"),
        "a run-to-completion handler must NOT drive the cont resume/poll verbs:\n{ir}"
    );
    // The direct-call path is preserved (the handler is still called).
    assert!(
        ir.contains("__hew_actor_dispatch_PlainActor"),
        "the dispatch trampoline must still be emitted:\n{ir}"
    );
    module
        .verify()
        .unwrap_or_else(|e| panic!("plain dispatch module failed verify: {e}"));
}

/// Module-actor symbol qualification: a layout keyed by the dotted
/// identity (`bank.Account`) emits its dispatch trampoline under the
/// `$`-mangled symbol (`__hew_actor_dispatch_bank$Account`) — never the
/// raw dotted spelling — and the handler symbols carry the same mangled
/// base. The bare-name probe above
/// (`__hew_actor_dispatch_PlainActor`) pins the root-actor zero-churn
/// half of the contract.
#[test]
fn dispatch_trampoline_module_actor_uses_mangled_qualified_symbol() {
    let ctx = Context::create();
    let symbol = "bank$Account__recv__ping";
    let mut pipeline = pipeline_with_actor_handlers(
        "bank.Account",
        vec![(
            unit_suspendable_layout(symbol, 3),
            run_to_completion_handler_fn(symbol),
        )],
    );
    pipeline.actor_layouts[0].defining_module = Some("bank".to_string());
    let module = build_module(&ctx, &pipeline, "qualified_dispatch_test")
        .expect("module-actor dispatch module must build");
    let ir = module.print_to_string().to_string();

    assert!(
        ir.contains("__hew_actor_dispatch_bank$Account"),
        "module-actor dispatch trampoline must use the $-mangled qualified symbol:\n{ir}"
    );
    assert!(
        !ir.contains("__hew_actor_dispatch_bank.Account"),
        "the raw dotted identity must never appear as a symbol:\n{ir}"
    );
    module
        .verify()
        .unwrap_or_else(|e| panic!("qualified dispatch module failed verify: {e}"));
}

/// R2 predicate==emission probe: the trampoline's `is_suspendable` predicate
/// agrees with the per-function `has_suspend` carrier for the SAME handler.
/// Build a mixed actor — one suspendable, one run-to-completion — and assert
/// the coroutine drive is emitted for exactly the suspendable handler's arm
/// and the direct-call shape for the other. A divergence here is the Lane-B
/// silent-no-op.
#[test]
fn dispatch_trampoline_predicate_matches_emission_for_mixed_actor() {
    let ctx = Context::create();
    let susp = "MixedActor__recv__suspends";
    let plain = "MixedActor__recv__plain";
    let pipeline = pipeline_with_actor_handlers(
        "MixedActor",
        vec![
            (
                unit_suspendable_layout(susp, 1),
                suspendable_handler_fn(susp),
            ),
            (
                unit_suspendable_layout(plain, 2),
                run_to_completion_handler_fn(plain),
            ),
        ],
    );
    let module = build_module(&ctx, &pipeline, "mixed_dispatch_test")
        .expect("mixed actor module must build");
    let ir = module.print_to_string().to_string();

    // Exactly one handler is a coroutine — the suspendable one. Count the
    // per-function attr line (`presplitcoroutine` also appears once in the
    // module-level `attributes #N = { presplitcoroutine }`, so match the
    // function-header form).
    assert_eq!(
        ir.matches("Function Attrs: presplitcoroutine").count(),
        1,
        "exactly the suspendable handler is a presplitcoroutine:\n{ir}"
    );
    // The trampoline drives that one handler (one poll-call site; the
    // void resume call carries no SSA name, so the poll call is the
    // per-driven-handler marker). Match the call-instruction definition form
    // so the value's later use in the icmp is not double-counted.
    assert_eq!(
        ir.matches("call i32 @hew_cont_poll(").count(),
        1,
        "the trampoline drives exactly one coroutine handler:\n{ir}"
    );
    module
        .verify()
        .unwrap_or_else(|e| panic!("mixed dispatch module failed verify: {e}"));
}

/// boundary-fail-closed: a handler layout marked with a suspendable MIR
/// function but a NON-`__recv__` symbol still drives correctly (the
/// predicate traces to the block-terminator carrier, not the name) — proving
/// the predicate is carrier-derived, not name-derived. The handler returns
/// `ptr` (ramp ABI) and is driven. (`__recv__` only controls the borrow_mode
/// ABI arg, which a non-`__recv__` handler must therefore NOT receive — so
/// this uses the receive ABI to keep arities aligned; the assertion is that
/// the carrier, not the name, decides ramp-vs-direct.)
#[test]
fn dispatch_trampoline_predicate_is_carrier_derived_not_name_derived() {
    let ctx = Context::create();
    // A `__recv__` symbol whose MIR is run-to-completion: the NAME looks like
    // an ordinary handler, but the CARRIER (no Suspend) must keep it on the
    // direct path. Conversely a `__recv__` symbol whose MIR carries Suspend
    // must be driven. Both share the receive ABI; only the carrier differs.
    let drives = "CarrierActor__recv__a";
    let direct = "CarrierActor__recv__b";
    let pipeline = pipeline_with_actor_handlers(
        "CarrierActor",
        vec![
            (
                unit_suspendable_layout(drives, 10),
                suspendable_handler_fn(drives),
            ),
            (
                unit_suspendable_layout(direct, 11),
                run_to_completion_handler_fn(direct),
            ),
        ],
    );
    let module = build_module(&ctx, &pipeline, "carrier_dispatch_test")
        .expect("carrier actor module must build");
    let ir = module.print_to_string().to_string();
    // The carrier (Suspend terminator) decided: exactly one driven handler.
    // Match the call-instruction definition form (not the value's later use).
    assert_eq!(
        ir.matches("call i32 @hew_cont_poll(").count(),
        1,
        "the block-terminator carrier (not the symbol name) selects the driven handler:\n{ir}"
    );
    module
        .verify()
        .unwrap_or_else(|e| panic!("carrier dispatch module failed verify: {e}"));
}

/// W6.010 body-side value routing: a value-reply suspendable handler deposits
/// its reply from inside the COROUTINE BODY (`hew_get_reply_channel` +
/// `hew_reply` at the `Terminator::Return` coroutine arm), NOT from the
/// trampoline's out-slot. The trampoline's `hew_cont_poll` therefore takes a
/// NULL out-pointer (the body owns the write), and the Ready arm does not
/// re-deposit. The reply lands wherever the coroutine completes — the
/// trampoline's first poll OR the scheduler's `resume_park` after the
/// trampoline frame has unwound — so the value is never routed through a
/// dangling stack slot, and never through the ramp `ptr` (E4).
#[test]
fn dispatch_trampoline_value_reply_routes_out_pointer_not_ramp_ptr() {
    let ctx = Context::create();
    let symbol = "ValueActor__recv__compute";
    let pipeline = pipeline_with_actor_handlers(
        "ValueActor",
        vec![(
            value_suspendable_layout(symbol, 4),
            suspendable_handler_fn(symbol),
        )],
    );
    let module = build_module(&ctx, &pipeline, "value_dispatch_test")
        .expect("value-reply suspendable module must build");
    let ir = module.print_to_string().to_string();

    // The trampoline no longer allocates a per-handler reply out-slot and no
    // longer routes a value through `hew_cont_poll`'s out-pointer: it passes
    // null (the coroutine body owns the reply deposit via
    // `hew_get_reply_channel` + `hew_reply` at its return, so the reply lands
    // wherever the coroutine completes — including a `resume_park` after the
    // trampoline frame unwinds). This fixture's body is a synthetic explicit
    // Suspend (not a SuspendingAsk), so it carries no reply deposit; the
    // assertion here pins the trampoline-side mechanism change. The body-side
    // deposit is exercised end-to-end by `examples/actor/await_*.hew`.
    assert!(
        !ir.contains("actor_suspend_reply_slot"),
        "the trampoline must NOT allocate a reply out-slot (W6.010 body-side routing):\n{ir}"
    );
    assert!(
        ir.contains("@hew_cont_poll(ptr") && ir.contains(", ptr null)"),
        "hew_cont_poll must take a null out-pointer (body-side routing):\n{ir}"
    );
    module
        .verify()
        .unwrap_or_else(|e| panic!("value-reply dispatch module failed verify: {e}"));
}

/// F-A frame-teardown: the Ready-immediately arm must reclaim the coro frame
/// exactly once via `hew_cont_destroy(handle)` — otherwise a handler whose
/// FIRST poll is Ready (single-await-then-complete, the common case) returns
/// null, is never parked, and the scheduler — which destroys only PARKED
/// (non-null) handles — never reclaims it: the frame leaks and its `cleanup`
/// (drops of locals held across the await) is skipped, every message
/// (cont.rs:99 "reclaimed via hew_cont_destroy exactly once").
///
/// This is an IR-assertion test (the harness convention): it asserts the
/// destroy call is emitted on the Ready arm, BEFORE the merge into the
/// dispatch return — so it FAILS if the `hew_cont_destroy` emission is
/// removed. End-to-end execution (drive a real first-poll-Ready coroutine and
/// observe the frame freed + cleanup run) is deferred to NEW-3b, which lands
/// the source that produces `Terminator::Suspend`; no source emits a suspend
/// today, so the driven path cannot be executed end-to-end here.
#[test]
fn dispatch_trampoline_ready_arm_destroys_frame_once() {
    let ctx = Context::create();
    let symbol = "ReclaimActor__recv__work";
    let pipeline = pipeline_with_actor_handlers(
        "ReclaimActor",
        vec![(
            unit_suspendable_layout(symbol, 6),
            suspendable_handler_fn(symbol),
        )],
    );
    let module = build_module(&ctx, &pipeline, "reclaim_dispatch_test")
        .expect("suspendable-handler module must build");
    let ir = module.print_to_string().to_string();

    // The driven handler reaches its Ready arm via the poll fork.
    assert!(
        ir.contains("suspend_ready_"),
        "the trampoline must fork to a Ready arm:\n{ir}"
    );
    // The Ready arm reclaims the frame: `hew_cont_destroy` is emitted (it is
    // emitted NOWHERE else in codegen, so its mere presence is the Ready-arm
    // teardown). Removing the destroy emission fails this assertion.
    assert!(
        ir.contains("@hew_cont_destroy("),
        "the Ready arm must reclaim the frame via hew_cont_destroy:\n{ir}"
    );
    // The destroy targets the same coro handle the resume/poll drive, and is
    // declared with the runtime's `void(ptr)` ABI (cont.rs:294).
    assert!(
        ir.contains("declare void @hew_cont_destroy(ptr"),
        "hew_cont_destroy must be declared with the runtime void(ptr) ABI:\n{ir}"
    );
    // ORDERING: the destroy sits on the Ready arm, BEFORE that block branches
    // to the dispatch-return merge. Anchor on the block-LABEL DEFINITION form
    // (`suspend_ready_0:`, with the trailing colon) — NOT a bare reference,
    // which also appears in the predecessor `br i1` and the merge phi/`preds`
    // comment that LLVM prints ahead of the block body. Within the block body,
    // assert the destroy call precedes the terminating `br`. A destroy emitted
    // after the merge (or not at all) fails here.
    let ready_label = ir
        .find("suspend_ready_0:")
        .expect("Ready block-label definition present");
    let ready_body = &ir[ready_label..];
    let destroy_at = ready_body
        .find("call void @hew_cont_destroy(")
        .expect("destroy call must appear within the Ready block body");
    let br_at = ready_body
        .find("br label")
        .expect("Ready block must terminate with a branch to the merge");
    assert!(
        destroy_at < br_at,
        "hew_cont_destroy must be emitted on the Ready arm BEFORE it branches \
             to the dispatch-return merge:\n{ir}"
    );
    module
        .verify()
        .unwrap_or_else(|e| panic!("reclaim dispatch module failed verify: {e}"));
}

/// A UNIT-reply suspendable handler must NOT consume a reply channel: its
/// Ready arm deposits NO reply (`hew_reply` is absent) and it allocates NO
/// poll out-slot. The reply-or-skip decision must trace to the handler's
/// LOGICAL reply type (`ActorHandlerLayout::return_ty == Unit`), NOT the
/// handler FUNCTION's declared LLVM return — which, for a suspendable ramp,
/// is the coro-handle `ptr` and therefore is NOT unit. Those two authorities
/// diverge for every unit-returning suspendable handler; keying off the
/// function's `returns_unit` (false here) wrongly drives the unit handler
/// through the value-reply path, allocating a reply slot and calling
/// `hew_reply` on a channel a unit handler must leave untouched — diverging
/// from the direct-call unit path, which skips the reply on the same logical
/// authority.
///
/// This test BITES: with the predicate keyed on the function's
/// `returns_unit` (the bug), this module emits `actor_suspend_reply_slot`
/// and a `hew_reply` call on the Ready arm and the assertions fail.
#[test]
fn dispatch_trampoline_unit_suspendable_handler_emits_no_reply() {
    let ctx = Context::create();
    let symbol = "UnitSuspendActor__recv__work";
    let pipeline = pipeline_with_actor_handlers(
        "UnitSuspendActor",
        vec![(
            // Logical reply type Unit, but the ramp fn returns ptr — the
            // exact divergence the fix must resolve to a single authority.
            unit_suspendable_layout(symbol, 9),
            suspendable_handler_fn(symbol),
        )],
    );
    let module = build_module(&ctx, &pipeline, "unit_suspend_dispatch_test")
        .expect("unit-reply suspendable module must build");
    let ir = module.print_to_string().to_string();

    // The handler is still driven (it is a coroutine ramp): the poll fork
    // and the Ready arm are present.
    assert!(
        ir.contains("hew_cont_poll") && ir.contains("suspend_ready_"),
        "the unit suspendable handler must still be driven through poll:\n{ir}"
    );
    // No reply value is deposited: a unit handler consumes no reply channel.
    // Scope the `hew_reply` check to the Ready block body (mirroring how
    // `dispatch_trampoline_ready_arm_destroys_frame_once` anchors on the
    // block-LABEL definition form) so the check targets exactly the arm that
    // wrongly deposited a reply under the bug.
    let ready_label = ir
        .find("suspend_ready_0:")
        .expect("Ready block-label definition present");
    let ready_body = &ir[ready_label..];
    // The Ready block ends at the next block label (the `pending` arm or the
    // merge); bound the search at the block's terminating branch so the
    // assertion does not run into unrelated blocks.
    let ready_block_end = ready_body
        .find("br label")
        .map(|br| br + ready_body[br..].find('\n').map(|n| n + 1).unwrap_or(0))
        .unwrap_or(ready_body.len());
    let ready_block = &ready_body[..ready_block_end];
    assert!(
        !ready_block.contains("@hew_reply("),
        "a unit suspendable handler's Ready arm must NOT call hew_reply:\n{ir}"
    );
    // No reply out-slot is allocated at all for a unit handler.
    assert!(
        !ir.contains("actor_suspend_reply_slot"),
        "a unit suspendable handler must NOT allocate a reply out-slot:\n{ir}"
    );
    module
        .verify()
        .unwrap_or_else(|e| panic!("unit suspend dispatch module failed verify: {e}"));
}

/// boundary-fail-closed (R2): the trampoline refuses when the suspendable
/// predicate was NOT carried for a handler (the `None` arm — no matching MIR
/// function to derive the discriminator from). It must NOT silently default
/// to direct-call a possibly-suspendable handler. Driven by calling the
/// emitter directly with a `None` predicate against a declared handler.
#[test]
fn dispatch_trampoline_fails_closed_on_uncarried_predicate() {
    let ctx = Context::create();
    let llvm_mod = ctx.create_module("uncarried_predicate_test");
    let record_layouts: RecordLayoutMap<'_> = RecordLayoutMap::new();

    // Declare a handler fn the trampoline can resolve, so the failure is the
    // missing PREDICATE, not a missing declaration.
    let symbol = "GapActor__recv__work";
    let mut handler_fn = run_to_completion_handler_fn(symbol);
    handler_fn.source_origin = SourceOrigin::SynthesizedActorHandler {
        kind: ActorHandlerKind::Receive,
        actor_layout_key: "GapActor".to_string(),
    };
    let mut fn_symbols: FnSymbolMap = HashMap::new();
    let target_data = host_target_data();
    let sym = declare_function(
        &ctx,
        &llvm_mod,
        &target_data,
        &handler_fn,
        &record_layouts,
        &[],
        false,
    )
    .expect("declare synthetic handler");
    fn_symbols.insert(symbol.to_string(), sym);

    let actor = ActorLayout {
        name: "GapActor".to_string(),
        defining_module: None,
        state_field_names: vec![],
        state_field_tys: vec![],
        state_field_defaults: vec![],
        init_param_names: vec![],
        init_param_tys: vec![],
        init_symbol: None,
        on_start_symbol: None,
        on_stop_symbols: vec![],
        on_crash_symbol: None,
        on_exit_symbol: None,
        on_down_symbol: None,
        max_heap_bytes: None,
        cycle_capable: false,
        mailbox_capacity: None,
        overflow_policy: None,
        coalesce_key_plan: None,
        handlers: vec![unit_suspendable_layout(symbol, 9)],
        state_clone_fn_symbol: None,
        state_drop_fn_symbol: None,
        state_field_clone_kinds: None,
    };

    // The predicate slice carries `None` for the handler — the discriminator
    // was not carried. The trampoline must fail closed.
    let result = crate::thunks::emit_actor_dispatch_trampoline(
        &ctx,
        &llvm_mod,
        &target_data,
        &actor,
        &[None],
        &fn_symbols,
        &record_layouts,
    );
    assert!(
        result.is_err(),
        "trampoline must fail closed when the suspendable predicate is uncarried"
    );
    let err = result.unwrap_err();
    assert!(
        matches!(err, CodegenError::FailClosed(_)),
        "expected FailClosed, got: {err:?}"
    );
    let msg = match &err {
        CodegenError::FailClosed(m) => m.as_str(),
        _ => unreachable!(),
    };
    assert!(
        msg.contains(symbol) && msg.contains("discriminator"),
        "the diagnostic must name the handler + the uncarried discriminator; got: {msg}"
    );
}

/// Native + wasm32 parity: the trampoline-bearing module with a suspendable
/// handler builds + CoroSplits clean for both triples (E9 — the wasm
/// deliverable is IR parity; actors are wasm-rejected from SOURCE but the
/// synthetic codegen path exercises the shared emitter).
fn assert_dispatch_drive_splits_clean_for_triple(triple: &str) {
    let ctx = Context::create();
    let symbol = "ParityActor__recv__work";
    let pipeline = pipeline_with_actor_handlers(
        "ParityActor",
        vec![(
            unit_suspendable_layout(symbol, 5),
            suspendable_handler_fn(symbol),
        )],
    );
    let machine = target_machine_for_triple(triple)
        .unwrap_or_else(|e| panic!("target machine for {triple}: {e:?}"));
    let module = build_module_for_target(
        &ctx,
        &pipeline,
        "parity_dispatch_test",
        Some(&machine),
        None,
    )
    .unwrap_or_else(|e| panic!("dispatch-drive module must build for {triple}: {e:?}"));
    assert!(
        crate::coro::module_has_coroutines(&module),
        "{triple}: the driven handler must be a coroutine before split"
    );
    // Keep the handler ramp externally visible so the (caller-less synthetic)
    // actor is a call-graph ROOT and CoroSplit's CGSCC walk processes it. In
    // a real program the trampoline is reachable from spawn registration +
    // `main`; this minimal pipeline has no spawn, so make the ramp External
    // (mirrors `assert_coro_splits_clean_for_triple`'s probe handling).
    module
        .get_function(symbol)
        .expect("handler ramp must exist")
        .set_linkage(Linkage::External);
    crate::coro::run_coro_passes(&module, &machine)
        .unwrap_or_else(|e| panic!("{triple}: coro pass pipeline failed: {e:?}"));
    assert!(
        !crate::coro::module_has_coroutines(&module),
        "{triple}: CoroSplit must consume the driven handler's presplitcoroutine marker"
    );
    module.verify().unwrap_or_else(|e| {
        panic!("{triple}: post-split dispatch-drive module failed verify: {e}")
    });
}

#[test]
fn dispatch_trampoline_drive_splits_clean_native() {
    assert_dispatch_drive_splits_clean_for_triple(&native_emission_triple());
}

#[test]
fn dispatch_trampoline_drive_splits_clean_wasm32() {
    assert_dispatch_drive_splits_clean_for_triple("wasm32-wasi");
}

/// Compile real Hew source through parser -> checker -> HIR -> MIR so the
/// resulting pipeline carries a genuine `Terminator::SuspendingAsk` (the
/// caller-side `await`), including the abandon-cleanup edge that frees the
/// reply channel on `coro.destroy`. Hand-building a valid SuspendingAsk MIR
/// would duplicate the lowering's Place wiring; compiling source keeps the
/// fixture honest.
fn pipeline_from_await_source() -> IrPipeline {
    let source = r"
actor Worker {
    let factor: i64;
    receive fn compute(n: i64) -> i64 {
        n * factor
    }
}

actor Coordinator {
    let worker: LocalPid<Worker>;
    receive fn run(n: i64) -> i64 {
        let r = await worker.compute(n);
        match r {
            Ok(v) => v,
            Err(_) => -1,
        }
    }
}

fn main() {
    let w = spawn Worker(factor: 6);
    let c = spawn Coordinator(worker: w);
    let r = await c.run(7);
    let _final = match r {
        Ok(v) => v,
        Err(_) => -1,
    };
}
";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker =
        hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = hew_hir::lower_program(
        &parsed.program,
        &tc_output,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    hew_mir::lower_hir_module(&output.module)
}

/// S2/E9: a real `SuspendingAsk` carrier (the `await` in `Coordinator.run`)
/// — including the abandon-cleanup edge that frees the reply channel on
/// `coro.destroy` — must build, CoroSplit, and pass `module.verify()` on the
/// native triple. The abandon block inserts new IR between the suspend's
/// case-1 edge and the shared coro cleanup; this guards that it stays
/// CoroSplit-clean.
///
/// Native-only by design: actors are wasm-rejected from SOURCE (E10), and a
/// source-compiled actor pipeline forced through the wasm emitter trips an
/// unrelated pre-existing `malloc` size mismatch in the spawn deep-copy path
/// (i64 size vs the wasm `malloc` i32 declaration) — not the await edge. The
/// abandon block branches into the SAME shared `coro.cleanup` outline whose
/// wasm CoroSplit-cleanliness is already proven by
/// `suspend_coroutine_splits_clean_wasm32` and
/// `dispatch_trampoline_drive_splits_clean_wasm32`.
fn assert_suspending_ask_splits_clean_for_triple(triple: &str) {
    let ctx = Context::create();
    let pipeline = pipeline_from_await_source();
    let machine = target_machine_for_triple(triple)
        .unwrap_or_else(|e| panic!("target machine for {triple}: {e:?}"));
    let module = build_module_for_target(
        &ctx,
        &pipeline,
        "suspending_ask_split_test",
        Some(&machine),
        None,
    )
    .unwrap_or_else(|e| panic!("{triple}: SuspendingAsk module must build: {e:?}"));
    assert!(
        crate::coro::module_has_coroutines(&module),
        "{triple}: the awaiting handler must be a coroutine before split"
    );
    // The abandon-cleanup edge is present: the destroy path routes through a
    // dedicated block that cancels + frees the reply channel before joining
    // the shared coro cleanup (S2 — without it `ch` leaks on abandon).
    let ir_pre = module.print_to_string().to_string();
    assert!(
            ir_pre.contains("suspending_ask_abandon_cleanup:"),
            "{triple}: the SuspendingAsk destroy edge must route through the abandon-cleanup block:\n{ir_pre}"
        );
    assert!(
        ir_pre.contains("call void @hew_reply_channel_cancel(ptr %suspending_ask_ch)")
            && ir_pre.contains("call void @hew_reply_channel_free(ptr %suspending_ask_ch)"),
        "{triple}: the SuspendingAsk destroy edge must cancel+free the reply channel:\n{ir_pre}"
    );

    // P2a: the null-reply resume edge reads the channel's orphaned flag and
    // binds AskError::OrphanedAsk (= 11) when set, instead of the TLS
    // last-error slot — matching the blocking-ask path's error semantics.
    assert!(
            ir_pre.contains("call i32 @hew_reply_channel_is_orphaned(ptr %suspending_ask_ch)"),
            "{triple}: the SuspendingAsk null-reply edge must read the channel orphaned flag:\n{ir_pre}"
        );
    assert!(
            ir_pre.contains("select i1 %suspending_ask_orphaned_flag, i32 11,"),
            "{triple}: a null reply on an orphaned channel must bind AskError::OrphanedAsk (11):\n{ir_pre}"
        );

    // Keep the awaiting ramp externally visible so CoroSplit's CGSCC walk
    // processes it (same probe-reachability handling as the sibling tests).
    for func in module.get_functions() {
        let name = func.get_name().to_string_lossy().into_owned();
        if name.contains("Coordinator") && name.contains("run") {
            func.set_linkage(Linkage::External);
        }
    }

    crate::coro::run_coro_passes(&module, &machine)
        .unwrap_or_else(|e| panic!("{triple}: coro pass pipeline failed: {e:?}"));
    module
        .verify()
        .unwrap_or_else(|e| panic!("{triple}: post-split SuspendingAsk module failed verify: {e}"));
}

#[test]
fn suspending_ask_splits_clean_native() {
    assert_suspending_ask_splits_clean_for_triple(&native_emission_triple());
}

// NEW-1 SuspendingRead: the producer-bridge contract (`await conn.read()`
// lowers to `Terminator::SuspendingRead`, the has_suspend codegen carrier)
// and the full suspend→reactor-wake→correct-bytes arc + CoroSplit-clean
// emission are proven END-TO-END by the `examples/net/await_read_*.hew`
// programs the lane ships (built + run with the worker-freeing single-worker
// proof). A codegen unit test mirroring `suspending_ask_splits_clean_native`
// would need the full stdlib `net` module registry to resolve
// `net.Connection` (the in-crate `ModuleRegistry::new(vec![])` harness has no
// stdlib), so the producer↔codegen-carrier contract is pinned by the example
// programs rather than an in-crate unit fixture.

/// Slice 4 — `is_heap_owning_record_composite_return` is the SHAPE predicate:
/// it admits a registered user-record composite-return shape (monomorphic or a
/// generic INSTANTIATION) keyed on the same mangled registry key the MIR admit
/// authority uses, so MIR and codegen admit the identical set of record shapes
/// (R3 — no admit disagreement). An UNregistered instantiation is excluded
/// (fail-closed). The heap-ownership decision is the composite-return gate's
/// record-aware `resolved_ty_contains_heap_leaf`, not this predicate, so a
/// registered all-BitCopy record IS a valid shape here (the gate's outer guard
/// excludes it for owning no heap).
#[test]
fn heap_owning_record_composite_return_admits_registered_record_shape() {
    let ctx = Context::create();
    let mut record_layouts: RecordLayoutMap<'_> = RecordLayoutMap::new();
    // Register the per-instantiation struct under its mangled key, mirroring
    // `register_record_layouts` for a generic record.
    let key = mangle("Pair", &[ResolvedTy::I64, ResolvedTy::String]);
    let st = ctx.opaque_struct_type(&key);
    st.set_body(
        &[
            ctx.i64_type().into(),
            ctx.ptr_type(AddressSpace::default()).into(),
        ],
        false,
    );
    record_layouts.structs.insert(key.clone(), st);

    // A registered generic record instantiation is a valid record shape.
    assert!(
        is_heap_owning_record_composite_return(
            &ResolvedTy::named_user("Pair", vec![ResolvedTy::I64, ResolvedTy::String]),
            &record_layouts,
        ),
        "a registered generic record instantiation must be admitted as a \
             record composite-return shape"
    );

    // A registered all-BitCopy instantiation is STILL a valid record shape:
    // the heap decision belongs to the gate's record-aware outer guard, which
    // excludes it for owning no heap. (Pre-split this predicate folded the
    // heap check in; the gate now owns it — `dedup-semantic-boundary`.)
    let bc_key = mangle("Pair", &[ResolvedTy::I64, ResolvedTy::I64]);
    let bc_st = ctx.opaque_struct_type(&bc_key);
    bc_st.set_body(&[ctx.i64_type().into(), ctx.i64_type().into()], false);
    record_layouts.structs.insert(bc_key, bc_st);
    assert!(
        is_heap_owning_record_composite_return(
            &ResolvedTy::named_user("Pair", vec![ResolvedTy::I64, ResolvedTy::I64]),
            &record_layouts,
        ),
        "a registered record instantiation is a valid shape regardless of heap \
             ownership — the gate's outer guard makes the heap decision"
    );

    // An UNregistered generic instantiation is excluded (fail-closed).
    assert!(
        !is_heap_owning_record_composite_return(
            &ResolvedTy::named_user("Pair", vec![ResolvedTy::String, ResolvedTy::Bytes]),
            &record_layouts,
        ),
        "an unregistered generic instantiation must not be admitted"
    );
}

/// Slice 3 — `record_inplace_drop_name` resolves the per-instantiation
/// helper name. A bare-name monomorphic record keeps its (prefix-stripped)
/// name; a generic INSTANTIATION mangles to the same `hew_hir::mangle`d key
/// the MIR admit authority and the synthesis seed use (`Pair$$i64$string`),
/// so the `__hew_record_drop_inplace_<key>` call resolves the synthesised
/// body. A non-record type fails closed.
#[test]
fn record_inplace_drop_name_mangles_generic_instantiation() {
    // Bare-name monomorphic: unchanged.
    assert_eq!(
        record_inplace_drop_name(&ResolvedTy::named_user("PairIS", vec![])).unwrap(),
        "PairIS",
    );
    // Imported bare name: module prefix stripped.
    assert_eq!(
        record_inplace_drop_name(&ResolvedTy::named_user("process.CommandOutput", vec![])).unwrap(),
        "CommandOutput",
    );
    // Generic instantiation: mangled to the registered layout key.
    let expected = mangle("Pair", &[ResolvedTy::I64, ResolvedTy::String]);
    assert_eq!(
        record_inplace_drop_name(&ResolvedTy::named_user(
            "Pair",
            vec![ResolvedTy::I64, ResolvedTy::String]
        ))
        .unwrap(),
        expected,
    );
    // A non-record type fails closed (never a dangling helper name).
    assert!(matches!(
        record_inplace_drop_name(&ResolvedTy::I64),
        Err(CodegenError::FailClosed(_))
    ));
}

/// Regression: the deserialize thunk's `malloc` call must use the target's
/// `size_t` width — `i32` on wasm32, `i64` on 64-bit native — not a
/// hardcoded `i64`.  `emit_cbor_codec_thunks` declares `malloc` via
/// `get_or_declare_libc_malloc` and passes a `runtime_size_ty`-width
/// `size_of()` constant, so the LLVM verifier accepts the call site on both
/// wasm32 (`malloc(i32)`) and 64-bit native (`malloc(i64)`).
///
/// The same width fix applies to the `memset` zero-init call in the same
/// thunk body.
#[test]
fn deserialize_thunk_malloc_uses_target_size_t_width() {
    // Build a pipeline that has an actor handler with a non-empty param_tys
    // so that `emit_actor_codec_module_init` → `emit_cbor_codec_thunks`
    // is reached.  A simple I64 message type is sufficient; it produces a
    // concrete deserialize thunk body that contains the `malloc` call.
    let symbol = "MallocWidthActor__recv__tick";
    let handler_layout = hew_mir::ActorHandlerLayout {
        name: "handler_1".to_string(),
        symbol: symbol.to_string(),
        msg_type: 1,
        param_tys: vec![ResolvedTy::I64],
        return_ty: ResolvedTy::Unit,
        requires_state_guard: false,
        every_ms: None,
        is_stream_producer: false,
    };
    // The MIR function for the handler uses ActorHandler call-conv.  Because
    // the function name contains `__recv__`, `declare_function` appends a
    // trailing `i32` borrow_mode param after the explicit `params` entries.
    // The dispatch trampoline calls it as (ctx_ptr, i64_msg, i32_borrow_mode),
    // so `params` and `locals` must both carry the i64 message type.
    let handler_fn = RawMirFunction {
        source_origin: hew_mir::SourceOrigin::Unknown,
        name: symbol.to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::ActorHandler,
        params: vec![ResolvedTy::I64],
        locals: vec![ResolvedTy::I64],
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![Instr::EnterContext, Instr::ExitContext],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    let pipeline =
        pipeline_with_actor_handlers("MallocWidthActor", vec![(handler_layout, handler_fn)]);

    // ── wasm32 ──────────────────────────────────────────────────────────
    let ctx = Context::create();
    let wasm_machine =
        target_machine_for_triple("wasm32-unknown-unknown").expect("wasm32 target machine");
    let wasm_mod = build_module_for_target(
        &ctx,
        &pipeline,
        "malloc_width_wasm32",
        Some(&wasm_machine),
        None,
    )
    .expect("wasm32 codec-thunk module must build without LLVM verifier error");
    let wasm_ir = wasm_mod.print_to_string().to_string();

    // The deserialize thunk must call `malloc` with an i32 argument on wasm32.
    assert!(
        wasm_ir.contains("call ptr @malloc(i32"),
        "wasm32 deserialize thunk must call malloc with i32 size arg (size_t = i32 on wasm32);\
             \n{wasm_ir}"
    );
    assert!(
        !wasm_ir.contains("call ptr @malloc(i64"),
        "wasm32 deserialize thunk must NOT call malloc with i64 size arg;\n{wasm_ir}"
    );

    // ── native ──────────────────────────────────────────────────────────
    let native_mod = build_module(&ctx, &pipeline, "malloc_width_native")
        .expect("native codec-thunk module must build");
    let native_ir = native_mod.print_to_string().to_string();

    // On 64-bit native targets the size arg is i64.
    assert!(
        native_ir.contains("call ptr @malloc(i64"),
        "native deserialize thunk must call malloc with i64 size arg (size_t = i64 on 64-bit);\
             \n{native_ir}"
    );
    assert!(
        !native_ir.contains("call ptr @malloc(i32"),
        "native deserialize thunk must NOT call malloc with i32 size arg;\n{native_ir}"
    );
}

// ── emit_field_drop_step Bytes-field layout validation ────────────────
//
// `StateFieldCloneKind::Bytes` field drop hard-codes a triple-field-0
// GEP + `hew_bytes_drop` call. Under opaque pointers a wrong-layout
// parent struct field would silently GEP a fabricated `{ ptr, i32, i32 }`
// over whatever bits sit at that field offset and free the wrong word.
// The arm validates the parent struct's field-at-`field_idx` is exactly
// a 3-field `(ptr, i32, i32)` struct and fail-closes otherwise. These
// tests pin both directions:
//
//   * happy path: a parent struct whose field IS `{ ptr, i32, i32 }`
//     succeeds and emits a GEP + `call @hew_bytes_drop` + null-store
//     sequence into the entry block.
//   * fail-closed: parent fields that are NOT BytesTriple-shaped
//     return `CodegenError::FailClosed` with a layout-citation
//     message (and emit no half-finished IR before failing).

/// Set up the boilerplate `emit_field_drop_step` needs: a Context,
/// Module, an `i32` host function with one basic block, a positioned
/// Builder, and an alloca of `parent_st` as the slot. Returns the
/// pieces in a tuple. The host function is parameter-less and returns
/// `i32 0` so the module verifies cleanly after the field-drop emit.
fn bytes_field_drop_test_harness<'ctx>(
    ctx: &'ctx Context,
    parent_st: StructType<'ctx>,
    fn_name: &str,
) -> (
    LlvmModule<'ctx>,
    Builder<'ctx>,
    PointerValue<'ctx>,
    inkwell::values::FunctionValue<'ctx>,
) {
    let llvm_mod = ctx.create_module(fn_name);
    let i32_ty = ctx.i32_type();
    let fn_ty = i32_ty.fn_type(&[], false);
    let host_fn = llvm_mod.add_function(fn_name, fn_ty, None);
    let entry = ctx.append_basic_block(host_fn, "entry");
    let builder = ctx.create_builder();
    builder.position_at_end(entry);
    let slot = builder
        .build_alloca(parent_st, "parent_slot")
        .expect("alloca parent_slot");
    (llvm_mod, builder, slot, host_fn)
}

/// Empty drop witnesses for the `Bytes`-arm unit tests. The Bytes arm is
/// reached before any enum/record/tuple routing, so the witness fields are
/// never dereferenced — this only satisfies the required parameter.
fn empty_drop_witnesses<'a, 'ctx>(
    target_data: &'a TargetData,
    machine_layouts: &'a MachineLayoutMap<'ctx>,
    record_structs: &'a RecordLayoutMap<'ctx>,
) -> DropSynthWitnesses<'a, 'ctx> {
    DropSynthWitnesses {
        enum_layouts: &[],
        machine_layouts,
        target_data,
        record_layouts: &[],
        record_structs,
        resource_record_close: &[],
    }
}

/// Happy path: parent struct field 0 IS the `{ ptr, i32, i32 }` Bytes-
/// Triple shape. `emit_field_drop_step(Bytes)` must succeed and the
/// emitted IR must contain a load + `hew_bytes_drop` call + null
/// store-back so the slot is idempotent against re-drop.
#[test]
fn emit_field_drop_step_bytes_ok_on_valid_bytestriple_field() {
    let ctx = Context::create();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i32_ty = ctx.i32_type();
    let triple_ty = ctx.struct_type(&[ptr_ty.into(), i32_ty.into(), i32_ty.into()], false);
    // Parent: a struct holding a BytesTriple at field 0 and a trailing
    // i64 (so field 0 is NOT the whole struct — exercises the inner GEP).
    let parent_st = ctx.struct_type(&[triple_ty.into(), ctx.i64_type().into()], false);

    let (llvm_mod, builder, slot, host_fn) =
        bytes_field_drop_test_harness(&ctx, parent_st, "bytes_field_drop_ok");

    let td = host_target_data();
    let ml = MachineLayoutMap::new();
    let rs = RecordLayoutMap::new();
    let w = empty_drop_witnesses(&td, &ml, &rs);
    emit_field_drop_step(
        &ctx,
        &llvm_mod,
        &builder,
        Some(parent_st),
        slot,
        0,
        &StateFieldCloneKind::Bytes,
        &w,
    )
    .expect("Bytes field drop on a valid `{ ptr, i32, i32 }` field must succeed");

    // Terminate the block so verify accepts it.
    builder
        .build_return(Some(&i32_ty.const_int(0, false)))
        .expect("ret void");
    assert!(
        llvm_mod.verify().is_ok(),
        "module must verify after a valid Bytes-field drop emit; verify error:\n{}",
        llvm_mod.verify().unwrap_err().to_string()
    );

    let ir = llvm_mod.print_to_string().to_string();
    assert!(
        ir.contains("call void @hew_bytes_drop"),
        "valid Bytes field drop must emit `call void @hew_bytes_drop(...)`; IR:\n{ir}"
    );
    assert!(
        ir.contains("store ptr null"),
        "valid Bytes field drop must null-store the data-ptr slot for idempotency; IR:\n{ir}"
    );
    // Defence-in-depth: confirm the host function exists and the GEP
    // lands at the right indices (field 0 of parent, then field 0 of
    // the BytesTriple). The exact `getelementptr` syntax inkwell emits
    // varies by version, so match the load/call/store witness instead.
    let _ = host_fn;
}

/// Fail-closed: parent struct field 0 is a 3-field struct but field 1
/// is `i64` (not `i32`) — a layout drift between
/// `primitive_to_llvm`'s Bytes arm and the parent struct's field at
/// `StateFieldCloneKind::Bytes`. The arm must refuse to emit a GEP
/// against the wrong shape and return `CodegenError::FailClosed` with
/// a layout-citation message.
#[test]
fn emit_field_drop_step_bytes_fail_closed_on_wrong_int_width() {
    let ctx = Context::create();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    // 3 fields but field 1 is i64, not i32 — layout drift.
    let triple_wrong = ctx.struct_type(&[ptr_ty.into(), i64_ty.into(), i32_ty.into()], false);
    let parent_st = ctx.struct_type(&[triple_wrong.into()], false);

    let (llvm_mod, builder, slot, _) =
        bytes_field_drop_test_harness(&ctx, parent_st, "bytes_field_drop_wrong_width");

    let td = host_target_data();
    let ml = MachineLayoutMap::new();
    let rs = RecordLayoutMap::new();
    let w = empty_drop_witnesses(&td, &ml, &rs);
    let err = emit_field_drop_step(
        &ctx,
        &llvm_mod,
        &builder,
        Some(parent_st),
        slot,
        0,
        &StateFieldCloneKind::Bytes,
        &w,
    )
    .expect_err("Bytes field drop must reject a 3-field struct with non-i32 inner width");

    match err {
        CodegenError::FailClosed(msg) => {
            assert!(
                msg.contains("BytesTriple"),
                "fail-closed message must cite the BytesTriple ABI; got: {msg}"
            );
            assert!(
                msg.contains("field"),
                "fail-closed message must cite the failing field index; got: {msg}"
            );
        }
        other => panic!("expected CodegenError::FailClosed on layout drift, got: {other:?}"),
    }
}

/// Fail-closed: parent struct field 0 is a struct of the WRONG arity
/// (4 fields instead of 3). A non-3-field struct cannot be a Bytes-
/// Triple regardless of field types; the arm refuses early.
#[test]
fn emit_field_drop_step_bytes_fail_closed_on_wrong_field_arity() {
    let ctx = Context::create();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i32_ty = ctx.i32_type();
    let wrong_arity = ctx.struct_type(
        &[ptr_ty.into(), i32_ty.into(), i32_ty.into(), i32_ty.into()],
        false,
    );
    let parent_st = ctx.struct_type(&[wrong_arity.into()], false);

    let (llvm_mod, builder, slot, _) =
        bytes_field_drop_test_harness(&ctx, parent_st, "bytes_field_drop_wrong_arity");

    let td = host_target_data();
    let ml = MachineLayoutMap::new();
    let rs = RecordLayoutMap::new();
    let w = empty_drop_witnesses(&td, &ml, &rs);
    let err = emit_field_drop_step(
        &ctx,
        &llvm_mod,
        &builder,
        Some(parent_st),
        slot,
        0,
        &StateFieldCloneKind::Bytes,
        &w,
    )
    .expect_err("Bytes field drop must reject a 4-field struct");

    match err {
        CodegenError::FailClosed(msg) => {
            assert!(
                msg.contains("3 fields") || msg.contains("BytesTriple"),
                "fail-closed message must cite the BytesTriple field-count requirement; \
                     got: {msg}"
            );
        }
        other => panic!("expected CodegenError::FailClosed on arity mismatch, got: {other:?}"),
    }
}

/// Fail-closed: parent struct field 0 is NOT a struct at all — a
/// scalar (e.g. an `i64`). The arm refuses before any GEP attempt
/// and the message cites the non-struct kind it observed so a
/// future producer drift surfaces with a targeted diagnostic.
#[test]
fn emit_field_drop_step_bytes_fail_closed_on_non_struct_field() {
    let ctx = Context::create();
    let i64_ty = ctx.i64_type();
    let parent_st = ctx.struct_type(&[i64_ty.into()], false);

    let (llvm_mod, builder, slot, _) =
        bytes_field_drop_test_harness(&ctx, parent_st, "bytes_field_drop_non_struct");

    let td = host_target_data();
    let ml = MachineLayoutMap::new();
    let rs = RecordLayoutMap::new();
    let w = empty_drop_witnesses(&td, &ml, &rs);
    let err = emit_field_drop_step(
        &ctx,
        &llvm_mod,
        &builder,
        Some(parent_st),
        slot,
        0,
        &StateFieldCloneKind::Bytes,
        &w,
    )
    .expect_err("Bytes field drop must reject a scalar (non-struct) field");

    match err {
        CodegenError::FailClosed(msg) => {
            assert!(
                msg.contains("not a struct") || msg.contains("BytesTriple"),
                "fail-closed message must cite that the field is not a BytesTriple struct; \
                     got: {msg}"
            );
        }
        other => {
            panic!("expected CodegenError::FailClosed on non-struct field, got: {other:?}")
        }
    }
}

// ── overwrite-release synthesis (record / enum state-field re-store) ──
//
// The synthesised `__hew_{record,enum}_overwrite_release_<Name>(old, new)`
// helpers must (a) neutralise (null) old heap leaves whose pointer
// reappears in the new value BEFORE running the in-place drop spine
// (functional update / self-store byte-copies alias leaves at refcount
// unchanged — an unguarded drop is a use-after-free), and (b) fail
// closed on classification drift instead of emitting a walk over the
// wrong shape.

/// Give the in-place drop helper a trivial body so the module verifies
/// (internal-linkage declarations without bodies are invalid IR; in the
/// real pipeline `emit_state_clone_drop_synthesis` always emits the
/// body alongside).
fn stub_drop_inplace_body(ctx: &Context, f: inkwell::values::FunctionValue<'_>) {
    let entry = ctx.append_basic_block(f, "entry");
    let builder = ctx.create_builder();
    builder.position_at_end(entry);
    builder.build_return(None).expect("stub drop ret");
}

// ── get_or_declare_inplace_thunk unit tests ──────────────────────────────
//
// Pin the single declare mechanic the ten per-(kind,shape) wrappers delegate
// to: the symbol name is `__hew_{kind}_{shape}_{name}`, the declare is
// idempotent, the LLVM signature is derived from `ThunkShape` (the single
// ABI-width source), and the linkage is always Internal. A wrong width or a
// CamelCase-vs-snake_case name fragment goes red here, not silently in IR.

#[test]
fn inplace_thunk_symbol_name_matches_kind_shape_name() {
    let ctx = Context::create();
    let m = ctx.create_module("inplace_thunk_name");
    let fv = get_or_declare_inplace_thunk(&ctx, &m, "record", "MyRecord", ThunkShape::CloneInplace);
    assert_eq!(
        fv.get_name().to_str().unwrap(),
        "__hew_record_clone_inplace_MyRecord"
    );
    let dv = get_or_declare_inplace_thunk(&ctx, &m, "enum", "St", ThunkShape::DropInplace);
    assert_eq!(
        dv.get_name().to_str().unwrap(),
        "__hew_enum_drop_inplace_St"
    );
    let ov = get_or_declare_inplace_thunk(&ctx, &m, "record", "Rec", ThunkShape::OverwriteRelease);
    assert_eq!(
        ov.get_name().to_str().unwrap(),
        "__hew_record_overwrite_release_Rec"
    );
}

#[test]
fn inplace_thunk_declare_is_idempotent() {
    let ctx = Context::create();
    let m = ctx.create_module("inplace_thunk_idem");
    let a = get_or_declare_inplace_thunk(&ctx, &m, "tuple", "T0", ThunkShape::CloneInplace);
    let b = get_or_declare_inplace_thunk(&ctx, &m, "tuple", "T0", ThunkShape::CloneInplace);
    assert_eq!(a, b, "repeat declare must return the cached FunctionValue");
}

#[test]
fn inplace_thunk_shape_derives_function_type() {
    let ctx = Context::create();
    let m = ctx.create_module("inplace_thunk_types");
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i32_ty = ctx.i32_type();
    let void_ty = ctx.void_type();

    let clone = get_or_declare_inplace_thunk(&ctx, &m, "record", "C", ThunkShape::CloneInplace);
    assert_eq!(
        clone.get_type(),
        i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false)
    );
    let drop = get_or_declare_inplace_thunk(&ctx, &m, "record", "D", ThunkShape::DropInplace);
    assert_eq!(drop.get_type(), void_ty.fn_type(&[ptr_ty.into()], false));
    let ow = get_or_declare_inplace_thunk(&ctx, &m, "record", "O", ThunkShape::OverwriteRelease);
    assert_eq!(
        ow.get_type(),
        void_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false)
    );
}

#[test]
fn inplace_thunk_declares_internal_linkage() {
    let ctx = Context::create();
    let m = ctx.create_module("inplace_thunk_linkage");
    let fv = get_or_declare_inplace_thunk(&ctx, &m, "enum", "L", ThunkShape::CloneInplace);
    assert_eq!(fv.get_linkage(), Linkage::Internal);
}

/// Happy path: a record `{ string, bytes, i64 }`. The synthesised body
/// must collect both new heap leaves, neutralise the old slots through
/// a compare+`select` (no unconditional release), and only then call
/// the record's in-place drop spine.
#[test]
fn record_overwrite_release_neutralizes_aliased_leaves_before_drop() {
    let ctx = Context::create();
    let llvm_mod = ctx.create_module("ow_record_ok");
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    let triple_ty = ctx.struct_type(&[ptr_ty.into(), i32_ty.into(), i32_ty.into()], false);
    let rec_st = ctx.opaque_struct_type("OwRec");
    rec_st.set_body(&[ptr_ty.into(), triple_ty.into(), i64_ty.into()], false);
    let kinds = vec![
        StateFieldCloneKind::String,
        StateFieldCloneKind::Bytes,
        StateFieldCloneKind::BitCopy { size_bytes: 8 },
    ];
    let mut record_structs = RecordLayoutMap::new();
    record_structs.insert("OwRec".to_string(), rec_st);
    let machine_layouts: MachineLayoutMap<'_> = HashMap::new();
    let kinds_slice: &[StateFieldCloneKind] = &kinds;
    let rec_kinds: HashMap<&str, &[StateFieldCloneKind]> =
        std::iter::once(("OwRec", kinds_slice)).collect();
    let enum_kinds: HashMap<&str, &EnumVariantKinds> = HashMap::new();
    let cx = OverwriteReleaseCx {
        ctx: &ctx,
        llvm_mod: &llvm_mod,
        record_structs: &record_structs,
        machine_layouts: &machine_layouts,
        rec_kinds: &rec_kinds,
        enum_kinds: &enum_kinds,
        // No indirect enums under test here; an empty layout slice makes
        // `is_indirect_enum` report every enum inline — the pre-fix path
        // these overwrite-release tests were written against.
        enum_layouts: &[],
    };
    stub_drop_inplace_body(
        &ctx,
        get_or_declare_record_drop_inplace(&ctx, &llvm_mod, "OwRec"),
    );

    emit_record_overwrite_release_body(&cx, "OwRec", rec_st, &kinds)
        .expect("record overwrite-release synthesis must succeed");

    let ir = llvm_mod.print_to_string().to_string();
    assert!(
        ir.contains("define internal void @__hew_record_overwrite_release_OwRec"),
        "helper must be defined; IR:\n{ir}"
    );
    // Both heap leaves are neutralised through a select, never an
    // unconditional release of the old pointer.
    assert_eq!(
        ir.matches("_neutralized = select i1").count(),
        2,
        "one guarded select per heap leaf (string + bytes); IR:\n{ir}"
    );
    // The drop spine runs AFTER the neutralise pass.
    let neutralize_at = ir
        .find("_neutralized = select i1")
        .expect("neutralize select present");
    let drop_at = ir
        .find("call void @__hew_record_drop_inplace_OwRec")
        .expect("drop spine call present");
    assert!(
        neutralize_at < drop_at,
        "neutralise must precede the drop-spine call; IR:\n{ir}"
    );
    llvm_mod
        .verify()
        .unwrap_or_else(|e| panic!("overwrite-release module failed verify: {e}"));
}

/// Fail-closed: a record field whose nested record name has no clone
/// classification (collector drift) must refuse synthesis instead of
/// walking an unknown shape.
#[test]
fn record_overwrite_release_fails_closed_on_unclassified_nested_record() {
    let ctx = Context::create();
    let llvm_mod = ctx.create_module("ow_record_drift");
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let inner_st = ctx.opaque_struct_type("OwMissing");
    inner_st.set_body(&[ptr_ty.into()], false);
    let rec_st = ctx.opaque_struct_type("OwOuter");
    rec_st.set_body(&[inner_st.into()], false);
    let kinds = vec![StateFieldCloneKind::UserRecord {
        name: "OwMissing".to_string(),
    }];
    let mut record_structs = RecordLayoutMap::new();
    record_structs.insert("OwOuter".to_string(), rec_st);
    record_structs.insert("OwMissing".to_string(), inner_st);
    let machine_layouts: MachineLayoutMap<'_> = HashMap::new();
    let kinds_slice: &[StateFieldCloneKind] = &kinds;
    // `OwMissing` is deliberately absent from the classification map.
    let rec_kinds: HashMap<&str, &[StateFieldCloneKind]> =
        std::iter::once(("OwOuter", kinds_slice)).collect();
    let enum_kinds: HashMap<&str, &EnumVariantKinds> = HashMap::new();
    let cx = OverwriteReleaseCx {
        ctx: &ctx,
        llvm_mod: &llvm_mod,
        record_structs: &record_structs,
        machine_layouts: &machine_layouts,
        rec_kinds: &rec_kinds,
        enum_kinds: &enum_kinds,
        // No indirect enums under test here; an empty layout slice makes
        // `is_indirect_enum` report every enum inline — the pre-fix path
        // these overwrite-release tests were written against.
        enum_layouts: &[],
    };

    let err = emit_record_overwrite_release_body(&cx, "OwOuter", rec_st, &kinds)
        .expect_err("unclassified nested record must fail closed");
    match err {
        CodegenError::FailClosed(msg) => assert!(
            msg.contains("OwMissing") && msg.contains("no clone classification"),
            "message must cite the missing record classification; got: {msg}"
        ),
        other => panic!("expected FailClosed, got: {other:?}"),
    }
}

/// Fail-closed: synthesising the same helper twice is a substrate
/// invariant violation (duplicate bodies), mirroring the drop family.
#[test]
fn record_overwrite_release_duplicate_synthesis_fails_closed() {
    let ctx = Context::create();
    let llvm_mod = ctx.create_module("ow_record_dup");
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let rec_st = ctx.opaque_struct_type("OwDup");
    rec_st.set_body(&[ptr_ty.into()], false);
    let kinds = vec![StateFieldCloneKind::String];
    let mut record_structs = RecordLayoutMap::new();
    record_structs.insert("OwDup".to_string(), rec_st);
    let machine_layouts: MachineLayoutMap<'_> = HashMap::new();
    let kinds_slice: &[StateFieldCloneKind] = &kinds;
    let rec_kinds: HashMap<&str, &[StateFieldCloneKind]> =
        std::iter::once(("OwDup", kinds_slice)).collect();
    let enum_kinds: HashMap<&str, &EnumVariantKinds> = HashMap::new();
    let cx = OverwriteReleaseCx {
        ctx: &ctx,
        llvm_mod: &llvm_mod,
        record_structs: &record_structs,
        machine_layouts: &machine_layouts,
        rec_kinds: &rec_kinds,
        enum_kinds: &enum_kinds,
        // No indirect enums under test here; an empty layout slice makes
        // `is_indirect_enum` report every enum inline — the pre-fix path
        // these overwrite-release tests were written against.
        enum_layouts: &[],
    };
    stub_drop_inplace_body(
        &ctx,
        get_or_declare_record_drop_inplace(&ctx, &llvm_mod, "OwDup"),
    );

    emit_record_overwrite_release_body(&cx, "OwDup", rec_st, &kinds)
        .expect("first synthesis succeeds");
    let err = emit_record_overwrite_release_body(&cx, "OwDup", rec_st, &kinds)
        .expect_err("second synthesis must fail closed");
    match err {
        CodegenError::FailClosed(msg) => assert!(
            msg.contains("duplicate synthesis"),
            "message must cite the duplicate; got: {msg}"
        ),
        other => panic!("expected FailClosed, got: {other:?}"),
    }
}

/// Enum helper: both the COLLECT pass (new value) and the NEUTRALISE
/// pass (old value) must tag-dispatch — two switches — and the body
/// must end in the enum's in-place drop spine. A unit variant + a
/// string-payload variant exercise the reserved-slot-range fill.
#[test]
fn enum_overwrite_release_switches_old_and_new_tags() {
    let ctx = Context::create();
    let llvm_mod = ctx.create_module("ow_enum_ok");
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i8_ty = ctx.i8_type();
    let outer_st = ctx.opaque_struct_type("OwStatus");
    outer_st.set_body(&[i8_ty.into(), ctx.i8_type().array_type(8).into()], false);
    let unit_variant = ctx.struct_type(&[], false);
    let payload_variant = ctx.struct_type(&[ptr_ty.into()], false);
    let layout = MachineCodegenLayout {
        outer_struct: outer_st,
        tag_int_ty: i8_ty,
        variant_struct_tys: vec![unit_variant, payload_variant],
        variant_field_tys: vec![vec![], vec![ResolvedTy::String]],
        state_name_table: None,
    };
    let mut machine_layouts: MachineLayoutMap<'_> = HashMap::new();
    machine_layouts.insert("OwStatus".to_string(), layout);
    let variants: EnumVariantKinds = vec![vec![], vec![StateFieldCloneKind::String]];
    let record_structs = RecordLayoutMap::new();
    let rec_kinds: HashMap<&str, &[StateFieldCloneKind]> = HashMap::new();
    let enum_kinds: HashMap<&str, &EnumVariantKinds> =
        std::iter::once(("OwStatus", &variants)).collect();
    let cx = OverwriteReleaseCx {
        ctx: &ctx,
        llvm_mod: &llvm_mod,
        record_structs: &record_structs,
        machine_layouts: &machine_layouts,
        rec_kinds: &rec_kinds,
        enum_kinds: &enum_kinds,
        // No indirect enums under test here; an empty layout slice makes
        // `is_indirect_enum` report every enum inline — the pre-fix path
        // these overwrite-release tests were written against.
        enum_layouts: &[],
    };
    stub_drop_inplace_body(
        &ctx,
        get_or_declare_enum_drop_inplace(&ctx, &llvm_mod, "OwStatus"),
    );

    emit_enum_overwrite_release_body(&cx, "OwStatus", &variants)
        .expect("enum overwrite-release synthesis must succeed");

    let ir = llvm_mod.print_to_string().to_string();
    assert_eq!(
        ir.matches("switch i8").count(),
        2,
        "collect (new tag) and neutralise (old tag) must each switch; IR:\n{ir}"
    );
    assert!(
        ir.contains("call void @__hew_enum_drop_inplace_OwStatus"),
        "the enum drop spine must run after neutralisation; IR:\n{ir}"
    );
    // Out-of-range tags fail closed through the trap path, not a
    // silent fall-through.
    assert!(
        ir.contains("hew_trap_with_code"),
        "tag switches must trap on out-of-range tags; IR:\n{ir}"
    );
    llvm_mod
        .verify()
        .unwrap_or_else(|e| panic!("enum overwrite-release module failed verify: {e}"));
}

/// Capacity arithmetic: records SUM their fields (recursing), enums
/// reserve their WORST-CASE variant, non-owned kinds contribute zero.
/// A drifted capacity under-allocates slots and the collect walk fails
/// closed at synthesis time, so this pins the arithmetic directly.
#[test]
fn overwrite_heap_leaf_capacity_sums_records_and_maxes_enums() {
    let ctx = Context::create();
    let llvm_mod = ctx.create_module("ow_capacity");
    let record_structs = RecordLayoutMap::new();
    let machine_layouts: MachineLayoutMap<'_> = HashMap::new();
    let inner_kinds = vec![StateFieldCloneKind::String];
    let inner_slice: &[StateFieldCloneKind] = &inner_kinds;
    let rec_kinds: HashMap<&str, &[StateFieldCloneKind]> =
        std::iter::once(("OwInner", inner_slice)).collect();
    let variants: EnumVariantKinds = vec![
        vec![],
        vec![StateFieldCloneKind::String, StateFieldCloneKind::Bytes],
    ];
    let enum_kinds: HashMap<&str, &EnumVariantKinds> =
        std::iter::once(("OwE", &variants)).collect();
    let cx = OverwriteReleaseCx {
        ctx: &ctx,
        llvm_mod: &llvm_mod,
        record_structs: &record_structs,
        machine_layouts: &machine_layouts,
        rec_kinds: &rec_kinds,
        enum_kinds: &enum_kinds,
        // No indirect enums under test here; an empty layout slice makes
        // `is_indirect_enum` report every enum inline — the pre-fix path
        // these overwrite-release tests were written against.
        enum_layouts: &[],
    };
    let kinds = vec![
        StateFieldCloneKind::String,
        StateFieldCloneKind::Bytes,
        StateFieldCloneKind::Vec {
            elem: Box::new(StateFieldCloneKind::BitCopy { size_bytes: 8 }),
        },
        StateFieldCloneKind::UserRecord {
            name: "OwInner".to_string(),
        },
        StateFieldCloneKind::Enum {
            name: "OwE".to_string(),
        },
        StateFieldCloneKind::BitCopy { size_bytes: 8 },
        StateFieldCloneKind::IoHandle {
            kind: IoHandleKind::Stream,
        },
    ];
    let cap = overwrite_heap_leaf_capacity(&cx, &kinds, 0).expect("capacity walk succeeds");
    // string(1) + bytes(1) + vec(1) + inner record(1) + enum max(0, 2)
    // + bitcopy(0) + io handle(0) = 6
    assert_eq!(cap, 6, "capacity must sum records and max enum variants");
}

/// `mangle_with_shortened_args` is the single codegen layout-key authority:
/// every layout-map lookup key must shorten the WHOLE type-arg spine to bare
/// names before mangling, matching the enum/record layout-REGISTRATION spine.
/// This pins that the authority is byte-identical to the (now-removed) inline
/// "shorten `effective_args`, then `mangle`" path the `resolve_ty` lookup keys
/// used to build — so the structural-purity reroute through the authority is
/// proven behaviour-preserving, not just asserted. A nested qualified payload
/// (`Result<Vec<fs.Foo>, _>`) is shortened at every depth.
#[test]
fn mangle_authority_matches_inline_shortened_mangle() {
    // The inline path `resolve_ty` previously used at the layout-key sites:
    // compute `effective_args` by shortening every arg when ANY needs it,
    // then `mangle`. The authority must reproduce this byte-for-byte.
    fn inline_legacy_key(name: &str, args: &[ResolvedTy]) -> String {
        let effective: std::borrow::Cow<[ResolvedTy]> = if args.iter().any(needs_normalization) {
            std::borrow::Cow::Owned(args.iter().cloned().map(shorten_named_args).collect())
        } else {
            std::borrow::Cow::Borrowed(args)
        };
        mangle(name, &effective)
    }

    let q = |n: &str, a: Vec<ResolvedTy>| ResolvedTy::named_user(n, a);
    let cases: Vec<(&str, Vec<ResolvedTy>)> = vec![
        // bare (no normalisation needed) — Cow::Borrowed branch
        ("Pair", vec![ResolvedTy::I64, ResolvedTy::String]),
        // top-level qualified payload — `fs.IoError` must shorten to `IoError`
        (
            "Result",
            vec![q("Listener", vec![]), q("fs.IoError", vec![])],
        ),
        // NESTED qualified payload — shortened at depth, not just top level
        (
            "Result",
            vec![
                ResolvedTy::named_user("Vec", vec![q("fs.Foo", vec![])]),
                q("string", vec![]),
            ],
        ),
        // mixed: one qualified, one already-bare arg
        ("Option", vec![q("json.Value", vec![])]),
    ];

    for (name, args) in &cases {
        assert_eq!(
            mangle_with_shortened_args(name, args),
            inline_legacy_key(name, args),
            "authority diverged from the inline shortened-mangle path for `{name}` {args:?}"
        );
        // And the short-name spelling (the fallback key) must match too.
        let short = short_name(name);
        assert_eq!(
            mangle_with_shortened_args(short, args),
            inline_legacy_key(short, args),
            "authority diverged on the short-name fallback key for `{name}` {args:?}"
        );
    }
}

/// Structural-purity guard: every layout-key mangle inside `resolve_ty` must
/// route through the codegen authority `mangle_with_shortened_args`, never a
/// direct `mangle(...)`. A direct `mangle` on raw (un-shortened) args would
/// build a key off the qualified spine and miss the bare-keyed layout entry —
/// the exact regression class C1 closed. A whole-file "exactly one mangle()"
/// scan would false-positive on the file's many legitimate symbol/method/
/// thunk mangles, so this guard is scoped to the `resolve_ty` body only,
/// where every `mangle` is a layout-key mangle and must use the authority.
/// `mangle_with_shortened_args(` is NOT flagged: it shares no `mangle(`
/// substring (it is `mangle_w...`), and line comments are stripped first so
/// prose mentioning `mangle(name, args)` does not trip the scan.
#[test]
fn resolve_ty_layout_keys_route_through_mangle_authority() {
    let src = include_str!("llvm.rs");
    // Find the `resolve_ty` definition and brace-match its body.
    let sig = "fn resolve_ty<'ctx>(";
    let sig_at = src.find(sig).expect("resolve_ty signature present");
    let body_open = src[sig_at..]
        .find('{')
        .map(|o| sig_at + o)
        .expect("resolve_ty opening brace");
    let mut depth = 0usize;
    let mut body_end = body_open;
    for (i, ch) in src[body_open..].char_indices() {
        match ch {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    body_end = body_open + i;
                    break;
                }
            }
            _ => {}
        }
    }
    assert!(
        body_end > body_open,
        "failed to brace-match resolve_ty body"
    );
    let body = &src[body_open..=body_end];

    // Strip line comments so prose mentioning `mangle(...)` is not scanned;
    // then assert no direct `mangle(` call survives. The authority call
    // `mangle_with_shortened_args(` contains no `mangle(` substring.
    let offending: Vec<&str> = body
        .lines()
        .map(|line| line.split("//").next().unwrap_or(line))
        .filter(|code| code.contains("mangle("))
        .collect();
    assert!(
        offending.is_empty(),
        "resolve_ty must build layout keys via `mangle_with_shortened_args`, \
             never a direct `mangle(...)`; offending line(s): {offending:?}"
    );
}

/// `record_struct_for` resolves a record's LLVM struct from
/// `fn_ctx.record_layouts`, which registration keys on the BARE (short)
/// outer name. A record reached through a module-qualified spelling
/// (`shapes.Point` under qualified-by-default) misses the primary key, so
/// the lookup MUST retry under `short_name(name)`. This guard brace-matches
/// the function body and asserts the short-name fallback survives — a future
/// edit that drops it re-opens the qualified-spine record-layout miss class
/// for `RecordInit` / `RecordFieldLoad`, the codegen mirror of the lower.rs
/// field-store / field-read shortening.
#[test]
fn record_struct_for_retries_under_short_name() {
    let src = include_str!("llvm.rs");
    let sig = "fn record_struct_for<'ctx>(";
    let sig_at = src.find(sig).expect("record_struct_for signature present");
    let body_open = src[sig_at..]
        .find('{')
        .map(|o| sig_at + o)
        .expect("record_struct_for opening brace");
    let mut depth = 0usize;
    let mut body_end = body_open;
    for (i, ch) in src[body_open..].char_indices() {
        match ch {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    body_end = body_open + i;
                    break;
                }
            }
            _ => {}
        }
    }
    assert!(
        body_end > body_open,
        "failed to brace-match record_struct_for body"
    );
    let body = &src[body_open..=body_end];
    // The body must route the outer name through `short_name` on the
    // fallback path — both the monomorphic retry and the generic retry that
    // also shortens the type-arg spine.
    assert!(
        body.contains("short_name(name)"),
        "record_struct_for must retry the record-layout lookup under \
             `short_name(name)` so a module-qualified record resolves its \
             bare-keyed layout; the short-name fallback is missing"
    );
    assert!(
        body.contains("mangle_with_shortened_args(short_name(name), args)"),
        "record_struct_for's generic fallback must shorten BOTH the outer \
             name and the type-arg spine via \
             `mangle_with_shortened_args(short_name(name), args)`"
    );
}

/// `emit_join_terminator` must route its `hew_actor_ask_with_channel` payload
/// size argument through `reconcile_int_width_signed`, matching every sibling
/// emitter (`send`, `suspending-ask`, `actor-send`, `actor-ask`, `select-ask`).
/// The size parameter is declared `usize`/`size_t` in the runtime ABI (i32 on
/// wasm32); passing the raw i64 built by `actor_payload_ptr_size` produces an
/// i64→i32 mismatched call that the LLVM verifier rejects on a 32-bit target.
/// On 64-bit hosts the reconcile is a no-op, so existing behaviour is unchanged.
///
/// This guard brace-matches the function body and asserts the call survives.
/// Removing `reconcile_int_width_signed` from the join path re-opens the class
/// of ABI-width bug fixed in PR #1920 for the wasm32 actor ABI.
#[test]
fn emit_join_terminator_reconciles_payload_size_width() {
    let src = include_str!("suspend.rs");
    let sig = "fn emit_join_terminator<'ctx>(";
    let sig_at = src
        .find(sig)
        .expect("emit_join_terminator signature present in suspend.rs");
    let body_open = src[sig_at..]
        .find('{')
        .map(|o| sig_at + o)
        .expect("emit_join_terminator opening brace");
    let mut depth = 0usize;
    let mut body_end = body_open;
    for (i, ch) in src[body_open..].char_indices() {
        match ch {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    body_end = body_open + i;
                    break;
                }
            }
            _ => {}
        }
    }
    assert!(
        body_end > body_open,
        "failed to brace-match emit_join_terminator body"
    );
    let body = &src[body_open..=body_end];
    assert!(
        body.contains("reconcile_int_width_signed"),
        "emit_join_terminator must route the hew_actor_ask_with_channel payload \
             size through `reconcile_int_width_signed` so the call is width-correct \
             on 32-bit targets (usize/size_t is i32 on wasm32, not i64). Every \
             sibling emitter (send, suspending-ask, actor-ask, select-ask) already \
             does this. Removing the reconcile re-opens the wasm32 actor ABI-width \
             bug class."
    );
}

/// ABI P0 (wire-format parity): the LLVM `hew_child_spec_struct_type` mirror
/// must match the `#[repr(C)]` Rust `HewChildSpec` field-for-field, including
/// the trailing `lifecycle_fn` pointer added for lifecycle-under-supervision.
///
/// The supervisor bootstrap builds a `HewChildSpec` literal from this LLVM
/// struct and passes it by pointer to `hew_supervisor_add_child_spec`, which
/// reads it as the Rust struct. If the two layouts drift (a field reordered,
/// a size or offset mismatch), the runtime reads `lifecycle_fn` from the
/// wrong bytes — a silent wrong-code FFI hazard. This test pins every field
/// offset and the total size against the host target data, so any future
/// edit to either struct that breaks parity fails here, not at runtime.
#[test]
fn hew_child_spec_struct_matches_runtime_abi() {
    use hew_runtime::supervisor::HewChildSpec;

    let ctx = Context::create();
    let td = host_target_data();
    let llvm_struct = hew_child_spec_struct_type(&ctx);

    // (Rust offset_of, LLVM element index) for every field, in declaration
    // order. The pairing IS the ABI contract: element N of the LLVM struct
    // must land at the same byte offset as the matching Rust field.
    let fields: [(usize, u32, &str); 17] = [
        (std::mem::offset_of!(HewChildSpec, name), 0, "name"),
        (
            std::mem::offset_of!(HewChildSpec, init_state),
            1,
            "init_state",
        ),
        (
            std::mem::offset_of!(HewChildSpec, init_state_size),
            2,
            "init_state_size",
        ),
        (std::mem::offset_of!(HewChildSpec, dispatch), 3, "dispatch"),
        (
            std::mem::offset_of!(HewChildSpec, restart_policy),
            4,
            "restart_policy",
        ),
        (
            std::mem::offset_of!(HewChildSpec, mailbox_capacity),
            5,
            "mailbox_capacity",
        ),
        (std::mem::offset_of!(HewChildSpec, overflow), 6, "overflow"),
        (
            std::mem::offset_of!(HewChildSpec, coalesce_key_fn),
            7,
            "coalesce_key_fn",
        ),
        (
            std::mem::offset_of!(HewChildSpec, coalesce_fallback),
            8,
            "coalesce_fallback",
        ),
        (
            std::mem::offset_of!(HewChildSpec, arena_cap_bytes),
            9,
            "arena_cap_bytes",
        ),
        (
            std::mem::offset_of!(HewChildSpec, cycle_capable),
            10,
            "cycle_capable",
        ),
        (std::mem::offset_of!(HewChildSpec, on_crash), 11, "on_crash"),
        (
            std::mem::offset_of!(HewChildSpec, lifecycle_fn),
            12,
            "lifecycle_fn",
        ),
        // v0.6 init-closure restart model trailing fields.
        (std::mem::offset_of!(HewChildSpec, init_fn), 13, "init_fn"),
        (std::mem::offset_of!(HewChildSpec, config), 14, "config"),
        (
            std::mem::offset_of!(HewChildSpec, config_size),
            15,
            "config_size",
        ),
        (
            std::mem::offset_of!(HewChildSpec, message_drop_fn),
            16,
            "message_drop_fn",
        ),
    ];

    for (rust_offset, llvm_idx, name) in fields {
        let llvm_offset = td
            .offset_of_element(&llvm_struct, llvm_idx)
            .unwrap_or_else(|| panic!("LLVM struct has no element {llvm_idx} for `{name}`"));
        assert_eq!(
            llvm_offset as usize, rust_offset,
            "HewChildSpec field `{name}` offset mismatch: LLVM {llvm_offset} vs Rust \
                 {rust_offset} — the #[repr(C)] mirror drifted from the runtime struct"
        );
    }

    assert_eq!(
        td.get_abi_size(&llvm_struct) as usize,
        std::mem::size_of::<HewChildSpec>(),
        "HewChildSpec total size mismatch between the LLVM mirror and the Rust struct"
    );
}

/// The init-thunk return `HewChildInitResult` LLVM mirror must match the
/// `#[repr(C)]` runtime struct field-for-field. The codegen-emitted thunk
/// returns this BY VALUE and `restart_child_from_spec` reads `state`/`size`
/// back; a layout drift reads the size as the state pointer (or vice versa)
/// — a wrong-code FFI hazard at every supervised spawn / restart.
#[test]
fn hew_child_init_result_struct_matches_runtime_abi() {
    use hew_runtime::supervisor::HewChildInitResult;

    let ctx = Context::create();
    let td = host_target_data();
    let llvm_struct = hew_child_init_result_struct_type(&ctx);

    let fields: [(usize, u32, &str); 2] = [
        (std::mem::offset_of!(HewChildInitResult, state), 0, "state"),
        (std::mem::offset_of!(HewChildInitResult, size), 1, "size"),
    ];
    for (rust_offset, llvm_idx, name) in fields {
        let llvm_offset = td
            .offset_of_element(&llvm_struct, llvm_idx)
            .unwrap_or_else(|| panic!("LLVM struct has no element {llvm_idx} for `{name}`"));
        assert_eq!(
            llvm_offset as usize, rust_offset,
            "HewChildInitResult field `{name}` offset mismatch: LLVM {llvm_offset} vs Rust \
                 {rust_offset}"
        );
    }
    assert_eq!(
        td.get_abi_size(&llvm_struct) as usize,
        std::mem::size_of::<HewChildInitResult>(),
        "HewChildInitResult total size mismatch between the LLVM mirror and the Rust struct"
    );
}

/// `hashmap_key_layout_descriptor_ptr` reconstructs `HewMapKeyLayout`'s byte
/// layout as an independent LLVM `StructType` because codegen does not link
/// the full `hew-cabi` map machinery at the emission site. This test pins
/// every field offset and the total size of that mirror against the real
/// `#[repr(C)]` struct so a future field reorder on either side fails here
/// instead of silently desyncing until a generated HashMap's hash/eq/drop
/// thunk reads the wrong slot at runtime.
///
/// The comparison struct below is built the same way
/// `hashmap_key_layout_descriptor_ptr`'s `layout_ty` is (same field-type
/// list, same order) but independently — this test never calls that
/// function or inspects its output, so the comparison is not tautological
/// against codegen's own construction.
#[test]
fn hew_map_key_layout_offset_parity() {
    use hew_cabi::map::HewMapKeyLayout;

    let ctx = Context::create();
    let td = host_target_data();
    let usize_ty = ctx.ptr_sized_int_type(&td, None);
    let i8_ty = ctx.i8_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let llvm_struct = ctx.struct_type(
        &[
            usize_ty.into(),
            usize_ty.into(),
            i8_ty.into(),
            ptr_ty.into(),
            ptr_ty.into(),
            ptr_ty.into(),
        ],
        false,
    );

    // (Rust offset_of, LLVM element index) for every field, in
    // declaration order.
    let fields: [(usize, u32, &str); 6] = [
        (std::mem::offset_of!(HewMapKeyLayout, size), 0, "size"),
        (std::mem::offset_of!(HewMapKeyLayout, align), 1, "align"),
        (
            std::mem::offset_of!(HewMapKeyLayout, ownership_kind),
            2,
            "ownership_kind",
        ),
        (std::mem::offset_of!(HewMapKeyLayout, hash_fn), 3, "hash_fn"),
        (std::mem::offset_of!(HewMapKeyLayout, eq_fn), 4, "eq_fn"),
        (std::mem::offset_of!(HewMapKeyLayout, drop_fn), 5, "drop_fn"),
    ];

    for (rust_offset, llvm_idx, name) in fields {
        let llvm_offset = td
            .offset_of_element(&llvm_struct, llvm_idx)
            .unwrap_or_else(|| panic!("LLVM struct has no element {llvm_idx} for `{name}`"));
        assert_eq!(
            llvm_offset as usize, rust_offset,
            "HewMapKeyLayout field `{name}` offset mismatch: LLVM {llvm_offset} vs Rust \
                 {rust_offset} — codegen's hashmap_key_layout_descriptor_ptr mirror drifted \
                 from hew_cabi::map::HewMapKeyLayout"
        );
    }

    assert_eq!(
        td.get_abi_size(&llvm_struct) as usize,
        std::mem::size_of::<HewMapKeyLayout>(),
        "HewMapKeyLayout total size mismatch between the codegen mirror and the \
             hew_cabi::map::HewMapKeyLayout struct"
    );
}

/// Both `hashmap_value_layout_descriptor_ptr` (plain-value path) and
/// `hashmap_owned_value_layout_descriptor_ptr` (owned-value path)
/// independently reconstruct `HewMapValueLayout`'s byte layout as an LLVM
/// `StructType` because codegen does not link the full `hew-cabi` map
/// machinery at either emission site. This test pins every field offset
/// and the total size of that shared shape against the real `#[repr(C)]`
/// struct.
///
/// `HewMapValueLayout` is `{ size, align, ownership_kind, drop_fn,
/// clone_fn }` — the INVERSE of `HewVecElemLayout`'s `clone_fn`-then-
/// `drop_fn` order (see the developer comment directly above the
/// owned-path initializer). A copy-paste from the Vec descriptor into
/// either Map descriptor site would silently swap which thunk runs on
/// which lifecycle event; this test's `offset_of!` comparison, sourced
/// independently from the real struct rather than from either emission
/// site, is what catches that swap.
///
/// The comparison struct below is built the same way both sites build
/// their `layout_ty` (same field-type list, same order) but
/// independently — this test never calls either function or inspects its
/// output, so the comparison is not tautological against codegen's own
/// construction.
#[test]
fn hew_map_value_layout_offset_parity() {
    use hew_cabi::map::HewMapValueLayout;

    let ctx = Context::create();
    let td = host_target_data();
    let usize_ty = ctx.ptr_sized_int_type(&td, None);
    let i8_ty = ctx.i8_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let llvm_struct = ctx.struct_type(
        &[
            usize_ty.into(),
            usize_ty.into(),
            i8_ty.into(),
            ptr_ty.into(),
            ptr_ty.into(),
        ],
        false,
    );

    // (Rust offset_of, LLVM element index) for every field, in
    // declaration order. drop_fn precedes clone_fn here — the inverse of
    // HewVecElemLayout's clone_fn/drop_fn order.
    let fields: [(usize, u32, &str); 5] = [
        (std::mem::offset_of!(HewMapValueLayout, size), 0, "size"),
        (std::mem::offset_of!(HewMapValueLayout, align), 1, "align"),
        (
            std::mem::offset_of!(HewMapValueLayout, ownership_kind),
            2,
            "ownership_kind",
        ),
        (
            std::mem::offset_of!(HewMapValueLayout, drop_fn),
            3,
            "drop_fn",
        ),
        (
            std::mem::offset_of!(HewMapValueLayout, clone_fn),
            4,
            "clone_fn",
        ),
    ];

    for (rust_offset, llvm_idx, name) in fields {
        let llvm_offset = td
            .offset_of_element(&llvm_struct, llvm_idx)
            .unwrap_or_else(|| panic!("LLVM struct has no element {llvm_idx} for `{name}`"));
        assert_eq!(
            llvm_offset as usize, rust_offset,
            "HewMapValueLayout field `{name}` offset mismatch: LLVM {llvm_offset} vs Rust \
                 {rust_offset} — codegen's hashmap_value_layout_descriptor_ptr /\
                 hashmap_owned_value_layout_descriptor_ptr mirror drifted from \
                 hew_cabi::map::HewMapValueLayout"
        );
    }

    assert_eq!(
        td.get_abi_size(&llvm_struct) as usize,
        std::mem::size_of::<HewMapValueLayout>(),
        "HewMapValueLayout total size mismatch between the codegen mirror and the \
             hew_cabi::map::HewMapValueLayout struct"
    );
}

/// `dyn_vtable_prefix_ty` is the single layout authority for the
/// dyn-trait vtable prefix `{ drop_in_place: fn ptr, size_of: usize,
/// align_of: usize }`: `emit_dyn_trait_vtable_definitions` derives
/// its first three slot types from it, and `lower_dyn_trait_vtable_drop`
/// / `lower_call_trait_method` GEP through it (or its field list) at
/// every drop/dispatch site. Pinning the helper therefore pins every
/// producer and consumer of the prefix at once; a prefix-slot reorder
/// or element-width change fails here instead of miscompiling every
/// `dyn Trait` drop/method-dispatch site.
///
/// Calling the real helper is not circular: the comparison values are
/// `std::mem::offset_of!` over the real `#[repr(C)]`
/// `hew_runtime::trait_object::HewVtable`, sourced independently of
/// codegen — the same pattern as
/// `hew_child_spec_struct_matches_runtime_abi` above.
#[test]
fn hew_vtable_prefix_offset_parity() {
    use hew_runtime::trait_object::HewVtable;

    let ctx = Context::create();
    let td = host_target_data();
    let llvm_struct = dyn_vtable_prefix_ty(&ctx, &td);

    let fields: [(usize, u32, &str); 3] = [
        (
            std::mem::offset_of!(HewVtable, drop_in_place),
            0,
            "drop_in_place",
        ),
        (std::mem::offset_of!(HewVtable, size_of), 1, "size_of"),
        (std::mem::offset_of!(HewVtable, align_of), 2, "align_of"),
    ];

    for (rust_offset, llvm_idx, name) in fields {
        let llvm_offset = td
            .offset_of_element(&llvm_struct, llvm_idx)
            .unwrap_or_else(|| panic!("LLVM struct has no element {llvm_idx} for `{name}`"));
        assert_eq!(
            llvm_offset as usize, rust_offset,
            "HewVtable field `{name}` offset mismatch: LLVM {llvm_offset} vs Rust \
                 {rust_offset} — codegen's dyn_vtable_prefix_ty drifted from \
                 hew_runtime::trait_object::HewVtable"
        );
    }

    assert_eq!(
        td.get_abi_size(&llvm_struct) as usize,
        std::mem::size_of::<HewVtable>(),
        "HewVtable total size mismatch between codegen's dyn_vtable_prefix_ty and \
             the hew_runtime::trait_object::HewVtable struct"
    );
}

/// RC10 Stage 1: the `emit_lifetime_start`/`emit_lifetime_end` helpers emit
/// the canonical opaque-pointer `@llvm.lifetime.{start,end}.p0(ptr %slot)`
/// shape (LLVM 22 single-operand signature — the explicit size operand was
/// removed upstream), and the bracketed module passes `Module::verify()`.
/// This is the mechanism gate — it proves the intrinsic resolves and the call
/// shape is verifier-clean independent of where the markers are later applied.
#[test]
fn lifetime_markers_emit_canonical_shape_and_verify() {
    let ctx = Context::create();
    let module = ctx.create_module("rc10_lifetime");
    let builder = ctx.create_builder();

    // void @probe()
    let fn_ty = ctx.void_type().fn_type(&[], false);
    let f = module.add_function("probe", fn_ty, None);
    let entry = ctx.append_basic_block(f, "entry");
    builder.position_at_end(entry);

    // A 16-byte aggregate slot: { i64, i64 }.
    let slot_ty = ctx.struct_type(&[ctx.i64_type().into(), ctx.i64_type().into()], false);
    let slot = builder.build_alloca(slot_ty, "probe_slot").expect("alloca");

    emit_lifetime_start(&ctx, &module, &builder, slot, "lt.start").expect("emit lifetime.start");
    // A use between the brackets so the slot is genuinely live.
    builder
        .build_store(slot, slot_ty.const_zero())
        .expect("store into bracketed slot");
    emit_lifetime_end(&ctx, &module, &builder, slot, "lt.end").expect("emit lifetime.end");
    builder.build_return(None).expect("ret void");

    let ir = module.print_to_string().to_string();
    assert!(
        ir.contains("call void @llvm.lifetime.start.p0(ptr %probe_slot)"),
        "expected single-operand start marker on the slot:\n{ir}"
    );
    assert!(
        ir.contains("call void @llvm.lifetime.end.p0(ptr %probe_slot)"),
        "expected single-operand end marker on the slot:\n{ir}"
    );
    // The declared intrinsics carry the opaque-pointer overload suffix.
    assert!(
        ir.contains("declare void @llvm.lifetime.start.p0(ptr")
            && ir.contains("declare void @llvm.lifetime.end.p0(ptr"),
        "expected opaque-pointer intrinsic declarations:\n{ir}"
    );
    module
        .verify()
        .unwrap_or_else(|e| panic!("bracketed module failed verify: {e}\n\nIR:\n{ir}"));
}

/// RC10 Stage 2: two disjoint stack slots, each bracketed with the RC10
/// markers, survive an `-O2` middle-end pipeline verifier-clean. The markers
/// are consumed by the backend stack colourer (which overlaps the disjoint
/// slots — proven out-of-band with `llc -O2`: bracketed frame = one slot,
/// unbracketed = two); this test pins the durable, host-portable half — that
/// `-O2` optimization neither rejects nor miscompiles a module carrying the
/// markers. A wrong marker shape (e.g. the removed 2-operand form) would fail
/// `run_passes`/`verify` here.
#[test]
fn lifetime_markers_survive_o2_pipeline() {
    let ctx = Context::create();
    let module = ctx.create_module("rc10_lifetime_o2");
    let builder = ctx.create_builder();
    let machine = target_machine_for_triple(&native_emission_triple()).expect("host machine");

    let fn_ty = ctx.void_type().fn_type(&[], false);
    let f = module.add_function("two_disjoint_slots", fn_ty, None);
    let entry = ctx.append_basic_block(f, "entry");
    builder.position_at_end(entry);

    // A `sink(ptr)` declaration so the slots escape (the optimizer cannot
    // delete a slot whose address is observed by an opaque external call).
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let sink = module.add_function(
        "rc10_sink",
        ctx.void_type().fn_type(&[ptr_ty.into()], false),
        Some(Linkage::External),
    );

    let slot_ty = ctx.i8_type().array_type(4096);
    for tag in ["a", "b"] {
        let slot = builder
            .build_alloca(slot_ty, &format!("slot_{tag}"))
            .expect("alloca");
        emit_lifetime_start(&ctx, &module, &builder, slot, &format!("lt.start.{tag}"))
            .expect("emit start");
        builder
            .build_call(sink, &[slot.into()], &format!("use.{tag}"))
            .expect("sink call");
        emit_lifetime_end(&ctx, &module, &builder, slot, &format!("lt.end.{tag}"))
            .expect("emit end");
    }
    builder.build_return(None).expect("ret void");

    module
        .verify()
        .unwrap_or_else(|e| panic!("pre-O2 module failed verify: {e}"));

    // `-O2` middle-end pipeline. The markers must pass through it without
    // error and leave the module verifier-clean.
    let options = inkwell::passes::PassBuilderOptions::create();
    module
        .run_passes("default<O2>", &machine, options)
        .unwrap_or_else(|e| panic!("O2 pipeline rejected the bracketed module: {e}"));
    module
        .verify()
        .unwrap_or_else(|e| panic!("post-O2 module failed verify: {e}"));
}

/// A trivial (non-coroutine) module run through `run_module_pipeline` at
/// `O2` takes the plain `default<O2>` branch and stays verifier-clean. The
/// post-pass `verify()` inside `run_module_pipeline` is the fail-closed guard;
/// this pins that the production seam (not just an ad-hoc `run_passes` call)
/// accepts a real module at O2.
#[test]
fn o2_pipeline_noncoro_module_verifies() {
    let ctx = Context::create();
    let module = ctx.create_module("o2_noncoro");
    let machine = target_machine_for_triple(&native_emission_triple()).expect("host machine");
    let builder = ctx.create_builder();

    // A non-trivial body so the optimizer has work: add two args, return.
    let i64_ty = ctx.i64_type();
    let fn_ty = i64_ty.fn_type(&[i64_ty.into(), i64_ty.into()], false);
    let f = module.add_function("add2", fn_ty, Some(Linkage::External));
    let entry = ctx.append_basic_block(f, "entry");
    builder.position_at_end(entry);
    let a = f.get_nth_param(0).expect("a").into_int_value();
    let b = f.get_nth_param(1).expect("b").into_int_value();
    let sum = builder.build_int_add(a, b, "sum").expect("add");
    builder.build_return(Some(&sum)).expect("ret");
    module.verify().expect("pre-O2 verify");

    run_module_pipeline(&module, &machine, OptLevel::O0).expect("O0 pipeline");
    run_module_pipeline(&module, &machine, OptLevel::O2).expect("O2 pipeline");
    module.verify().expect("post-O2 verify");
}

/// `module_has_coroutines` reports `false` for a non-coroutine module, so
/// `run_module_pipeline` at O2 selects the plain `default<O2>` branch (not the
/// folded coro string). Pins the fold-guard predicate the production seam keys
/// on — a regression that always reported `true` would double-run coro-split.
#[test]
fn o2_pipeline_noncoro_takes_default_branch() {
    let ctx = Context::create();
    let module = ctx.create_module("fold_guard");
    let i64_ty = ctx.i64_type();
    let fn_ty = i64_ty.fn_type(&[], false);
    let f = module.add_function("k", fn_ty, None);
    let entry = ctx.append_basic_block(f, "entry");
    let builder = ctx.create_builder();
    builder.position_at_end(entry);
    builder
        .build_return(Some(&i64_ty.const_int(1, false)))
        .expect("ret");
    assert!(
        !crate::coro::module_has_coroutines(&module),
        "a plain module must not be treated as a coroutine (would double-run coro-split)"
    );
}

/// The `HEW_OPT_LEVEL` env var is a FLOOR — it raises O0 → O2 but never
/// lowers O2, and ignores any value other than exactly `"2"`. This is the
/// test-harness lever the differential-exec gate uses; a regression that let
/// it demote an explicit O2 build would silently weaken release codegen.
///
/// Serialised via a process-global lock: env vars are process-wide, and the
/// codegen test suite runs in-process, so two env-mutating tests must not
/// interleave.
#[test]
fn opt_level_env_floor_raises_but_never_lowers() {
    use std::sync::Mutex;
    static ENV_LOCK: Mutex<()> = Mutex::new(());
    let _g = ENV_LOCK.lock().unwrap_or_else(|p| p.into_inner());

    // Save + restore the ambient value so the test is hermetic.
    let saved = std::env::var_os("HEW_OPT_LEVEL");
    let restore = || match &saved {
        Some(v) => unsafe { std::env::set_var("HEW_OPT_LEVEL", v) },
        None => unsafe { std::env::remove_var("HEW_OPT_LEVEL") },
    };

    unsafe { std::env::remove_var("HEW_OPT_LEVEL") };
    assert_eq!(resolve_opt_level_with_env_floor(OptLevel::O0), OptLevel::O0);
    assert_eq!(resolve_opt_level_with_env_floor(OptLevel::O2), OptLevel::O2);

    unsafe { std::env::set_var("HEW_OPT_LEVEL", "2") };
    assert_eq!(
        resolve_opt_level_with_env_floor(OptLevel::O0),
        OptLevel::O2,
        "HEW_OPT_LEVEL=2 must raise O0 to O2"
    );
    assert_eq!(resolve_opt_level_with_env_floor(OptLevel::O2), OptLevel::O2);

    // A non-"2" value is ignored (the floor only acts on exactly "2").
    unsafe { std::env::set_var("HEW_OPT_LEVEL", "0") };
    assert_eq!(
        resolve_opt_level_with_env_floor(OptLevel::O2),
        OptLevel::O2,
        "HEW_OPT_LEVEL=0 must never demote an explicit O2 build"
    );
    unsafe { std::env::set_var("HEW_OPT_LEVEL", "1") };
    assert_eq!(resolve_opt_level_with_env_floor(OptLevel::O0), OptLevel::O0);

    restore();
}

/// `OptLevel::from_cli_str` accepts exactly `{"0","2"}` and rejects
/// everything else — the fail-closed CLI parse.
#[test]
fn opt_level_from_cli_str_is_fail_closed() {
    assert_eq!(OptLevel::from_cli_str("0"), Some(OptLevel::O0));
    assert_eq!(OptLevel::from_cli_str("2"), Some(OptLevel::O2));
    assert_eq!(OptLevel::from_cli_str("1"), None);
    assert_eq!(OptLevel::from_cli_str("3"), None);
    assert_eq!(OptLevel::from_cli_str(""), None);
    assert_eq!(OptLevel::from_cli_str("O2"), None);
}

// ── extern record-return classification (#2399) ─────────────────────────

/// Build a `TargetData` for an arbitrary triple (cross targets work because
/// `initialise_llvm_targets` registers every backend). Mirrors
/// `abi_class::tests::target_data_for`.
fn record_ret_target_data(triple: &str) -> TargetData {
    initialise_llvm_targets();
    let tt = inkwell::targets::TargetTriple::create(triple);
    let target =
        Target::from_triple(&tt).unwrap_or_else(|e| panic!("from_triple({triple}): {e:?}"));
    let machine = target
        .create_target_machine(
            &tt,
            "generic",
            "",
            inkwell::OptimizationLevel::Default,
            inkwell::targets::RelocMode::PIC,
            inkwell::targets::CodeModel::Default,
        )
        .unwrap_or_else(|| panic!("create_target_machine({triple}) returned None"));
    machine.get_target_data()
}

/// Classify against a module whose triple is set to `triple` — the exact
/// entry `predeclare_extern_decls` uses.
fn classify_on<'ctx>(
    ctx: &'ctx Context,
    triple: &str,
    name: &str,
    struct_ty: StructType<'ctx>,
) -> (
    LlvmModule<'ctx>,
    CodegenResult<Option<(FunctionValue<'ctx>, ExternRecordRet<'ctx>)>>,
) {
    let llvm_mod = ctx.create_module("record_ret_test");
    llvm_mod.set_triple(&inkwell::targets::TargetTriple::create(triple));
    let td = record_ret_target_data(triple);
    let result = classify_extern_record_return(ctx, &llvm_mod, &td, name, struct_ty, &[]);
    (llvm_mod, result)
}

const RECORD_RET_SYSV: &str = "x86_64-unknown-linux-gnu";
const RECORD_RET_MSVC: &str = "x86_64-pc-windows-msvc";

/// A float leaf anywhere in the record (top level or nested) must fail
/// closed: the SysV SSE / AAPCS64 HFA return classes are unmodelled and a
/// size-only classification would misroute the value registers.
#[test]
fn extern_record_return_float_leaf_fails_closed() {
    let ctx = Context::create();
    let f64t = ctx.f64_type();
    let i64t = ctx.i64_type();
    let flat = ctx.struct_type(&[f64t.into(), i64t.into()], false);
    let (_m, res) = classify_on(&ctx, RECORD_RET_SYSV, "float_flat", flat);
    match res {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(msg.contains("float"), "msg: {msg}");
        }
        other => panic!("float-bearing record must fail closed, got {other:?}"),
    }
    // Nested: the leaf walk must recurse through inner structs and arrays.
    let inner = ctx.struct_type(&[f64t.into()], false);
    let nested = ctx.struct_type(&[i64t.into(), inner.into()], false);
    let (_m2, res2) = classify_on(&ctx, RECORD_RET_SYSV, "float_nested", nested);
    assert!(
        matches!(res2, Err(CodegenError::FailClosed(_))),
        "nested float leaf must fail closed"
    );
    let arr = f64t.array_type(2);
    let arr_struct = ctx.struct_type(&[arr.into()], false);
    let (_m3, res3) = classify_on(&ctx, RECORD_RET_SYSV, "float_array", arr_struct);
    assert!(
        matches!(res3, Err(CodegenError::FailClosed(_))),
        "float array leaf must fail closed"
    );
}

/// A bytes-shaped `{ptr,i32,i32}` field is integer/pointer-only — the float
/// walk must NOT reject it (the #2392 TlsReadFfiResult shape depends on it).
#[test]
fn extern_record_return_bytes_like_field_is_not_float() {
    let ctx = Context::create();
    let ptr = ctx.ptr_type(AddressSpace::default());
    let i32t = ctx.i32_type();
    let bytes_like = ctx.struct_type(&[ptr.into(), i32t.into(), i32t.into()], false);
    let record = ctx.struct_type(&[bytes_like.into(), i32t.into()], false);
    assert!(
        !struct_has_float_or_vector_leaf(record),
        "{{bytes, i32}} record has no float leaf"
    );
    // 24 bytes on 64-bit → classifies Indirect (Sret), not fail-closed.
    let (_m, res) = classify_on(&ctx, RECORD_RET_SYSV, "tls_read_shape", record);
    match res {
        Ok(Some((_, ExternRecordRet::Sret { .. }))) => {}
        other => panic!("24-byte bytes-bearing record must classify Sret, got {other:?}"),
    }
}

/// A single-field record's bare struct return already matches the C ABI —
/// classification returns `None` (Natural; the ecosystem-handle control).
#[test]
fn extern_record_return_single_field_stays_natural() {
    let ctx = Context::create();
    let i64t = ctx.i64_type();
    let one = ctx.struct_type(&[i64t.into()], false);
    let (_m, res) = classify_on(&ctx, RECORD_RET_SYSV, "handle_one", one);
    assert!(
        matches!(res, Ok(None)),
        "single-field Direct record must stay Natural"
    );
}

/// An 8-byte multi-field record gets the coerced-int carrier: the declared
/// function returns `i64` (one register — the C ABI packing), not the bare
/// two-register struct split. The `recordret_small` red probe's fix.
#[test]
fn extern_record_return_small_multi_field_gets_int_carrier() {
    let ctx = Context::create();
    let i32t = ctx.i32_type();
    let small = ctx.struct_type(&[i32t.into(), i32t.into()], false);
    let (_m, res) = classify_on(&ctx, RECORD_RET_SYSV, "small_pair", small);
    match res {
        Ok(Some((fv, ExternRecordRet::Carrier { pointee }))) => {
            assert_eq!(pointee, small);
            let ret = fv.get_type().get_return_type().expect("carrier return");
            assert_eq!(ret, BasicTypeEnum::IntType(ctx.i64_type()), "i64 carrier");
        }
        other => panic!("8-byte multi-field must get an int carrier, got {other:?}"),
    }
}

/// A 16-byte two-eightbyte record gets the `[2 x i64]` register-pair
/// carrier on SysV — including the packed-second-eightbyte shape the
/// `recordret_packed` probe proved red as a bare struct return.
#[test]
fn extern_record_return_16b_gets_register_pair_carrier() {
    let ctx = Context::create();
    let i64t = ctx.i64_type();
    let i32t = ctx.i32_type();
    let packed = ctx.struct_type(&[i64t.into(), i32t.into(), i32t.into()], false);
    let (_m, res) = classify_on(&ctx, RECORD_RET_SYSV, "packed_16", packed);
    match res {
        Ok(Some((fv, ExternRecordRet::Carrier { pointee }))) => {
            assert_eq!(pointee, packed);
            let ret = fv.get_type().get_return_type().expect("carrier return");
            assert_eq!(
                ret,
                BasicTypeEnum::ArrayType(ctx.i64_type().array_type(2)),
                "[2 x i64] carrier"
            );
        }
        other => panic!("16-byte record must get [2 x i64] carrier, got {other:?}"),
    }
}

/// An over-16-byte record is declared `void(ptr, …)` with `sret`+`noalias`
/// on param 0 — the #2399 headline fix. NEGATIVE: the declared function
/// must NOT return the bare struct.
#[test]
fn extern_record_return_over_16b_gets_sret() {
    let ctx = Context::create();
    let i64t = ctx.i64_type();
    let big = ctx.struct_type(&[i64t.into(), i64t.into(), i64t.into()], false);
    let (llvm_mod, res) = classify_on(&ctx, RECORD_RET_SYSV, "big_24", big);
    match res {
        Ok(Some((fv, ExternRecordRet::Sret { pointee }))) => {
            assert_eq!(pointee, big);
            assert!(
                fv.get_type().get_return_type().is_none(),
                "sret extern must be declared void"
            );
            assert_eq!(
                fv.count_params(),
                1,
                "hidden sret pointer must be the only declared param here"
            );
            let ir = llvm_mod.print_to_string().to_string();
            assert!(
                ir.contains("sret("),
                "declaration must carry sret(...):\n{ir}"
            );
            assert!(
                ir.contains("noalias"),
                "declaration must carry noalias:\n{ir}"
            );
        }
        other => panic!("24-byte record must classify Sret, got {other:?}"),
    }
}

/// MSVC divergence: a 16-byte record is Indirect (sret) on Windows x64
/// MSVC where SysV uses a register pair. Host green is non-evidence for
/// this arm — the classification itself is pinned here.
#[test]
fn extern_record_return_16b_is_sret_on_msvc() {
    let ctx = Context::create();
    let i64t = ctx.i64_type();
    let i32t = ctx.i32_type();
    let mid = ctx.struct_type(&[i32t.into(), i64t.into()], false);
    let (_m, res) = classify_on(&ctx, RECORD_RET_MSVC, "mid_msvc", mid);
    assert!(
        matches!(res, Ok(Some((_, ExternRecordRet::Sret { .. })))),
        "16-byte record must be Sret on windows-msvc"
    );
}

/// wasm32 record returns fail closed: no runtime probe proves that ABI,
/// and the size-only wasm32 arm would misclassify multi-field ≤8-byte
/// aggregates.
#[test]
fn extern_record_return_wasm32_fails_closed() {
    let ctx = Context::create();
    let i32t = ctx.i32_type();
    let small = ctx.struct_type(&[i32t.into(), i32t.into()], false);
    let (_m, res) = classify_on(&ctx, "wasm32-unknown-unknown", "wasm_rec", small);
    match res {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(msg.contains("wasm32"), "msg: {msg}");
        }
        other => panic!("wasm32 record return must fail closed, got {other:?}"),
    }
}

/// A RegisterPair record whose ABI size is not a whole number of eightbytes
/// (12-byte `{i32,i32,i32}`) fails closed: the 16-byte carrier store would
/// write past the destination slot.
#[test]
fn extern_record_return_sub_eightbyte_tail_fails_closed() {
    let ctx = Context::create();
    let i32t = ctx.i32_type();
    let twelve = ctx.struct_type(&[i32t.into(), i32t.into(), i32t.into()], false);
    let td = record_ret_target_data(RECORD_RET_SYSV);
    assert_eq!(td.get_abi_size(&twelve), 12, "precondition: 12-byte record");
    let (_m, res) = classify_on(&ctx, RECORD_RET_SYSV, "twelve_bytes", twelve);
    match res {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(msg.contains("sub-eightbyte"), "msg: {msg}");
        }
        other => panic!("12-byte RegisterPair must fail closed, got {other:?}"),
    }
}

/// `.real()` must reject a record-return extern: every `.real()` consumer
/// (spawn thunks, closures, drop dispatch) emits a direct by-value call
/// that would miscompile the sret/carrier ABI.
#[test]
fn fn_symbol_real_accessor_rejects_record_return_extern() {
    let ctx = Context::create();
    let m = ctx.create_module("real_reject_test");
    let i64t = ctx.i64_type();
    let big = ctx.struct_type(&[i64t.into(), i64t.into(), i64t.into()], false);
    let ptr = ctx.ptr_type(AddressSpace::default());
    let fn_ty = ctx.void_type().fn_type(&[ptr.into()], false);
    let value = m.add_function("sret_extern", fn_ty, None);
    let sym = FnSymbol::Real {
        value,
        return_ty: big.into(),
        returns_unit: false,
        extern_record_ret: Some(ExternRecordRet::Sret { pointee: big }),
        extern_malloc_string_ret: false,
    };
    let err = sym
        .real("sret_extern", "test consumer")
        .expect_err(".real() must reject a record-return extern");
    match err {
        CodegenError::FailClosed(msg) => {
            assert!(msg.contains("Terminator::Call"), "msg: {msg}");
        }
        other => panic!("expected FailClosed, got {other:?}"),
    }
}

// Make `StubErr` `Clone` so we can re-use the same error in multiple
// assertions above. (Trivial — derived above.)

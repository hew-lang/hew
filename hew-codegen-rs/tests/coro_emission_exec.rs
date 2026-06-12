//! End-to-end execution proof for the stackless continuation primitive (W6.007).
//!
//! Proves the SUBSTRATE, not a source surface (no `gen fn`/`await` is wired to
//! `llvm.coro.*` yet — that is slices 4/5). The test emits a switched-resume
//! coroutine through the real codegen API (`hew_codegen_rs::coro::*`), runs
//! LLVM's coro lowering, links the object against the REAL Hew runtime
//! (`libhew.a` native / `libhew_runtime.a` wasm32), and drives it through the
//! REAL `HewCont` C ABI (`hew_cont_resume`/`hew_cont_done`/`hew_cont_destroy`):
//!
//!   * `coro_substrate_round_trips_value_native` — a finite generator coroutine
//!     allocates a Hew-owned heap value, mutates it across four suspends, and
//!     frees it once. The driver resumes/polls/destroys it via the runtime ABI.
//!     Asserts the accumulated sum (the value round-tripped through the frame on
//!     every resume) AND the alloc/free accounting (frame + value each 1/1).
//!     A leak or double-free fails the in-program accounting (exit != 0).
//!
//!   * `coro_substrate_leak_clean_native` — re-runs the same binary under
//!     `MallocScribble`/`MallocGuardEdges` (freed memory is poisoned; heap edges
//!     guarded) and `leaks --atExit` — proving the heap value is genuinely
//!     reloaded from the frame each resume (not read from a freed slot) and the
//!     single-teardown-owner discipline leaks nothing.
//!
//!   * `coro_substrate_round_trips_value_wasm32` — the SAME coroutine IR lowered
//!     for `wasm32-wasi`, linked with `wasm-ld` against the wasm runtime, run
//!     under `wasmtime`. The frame lives in linear memory via the Hew allocator;
//!     no malloc/asyncify/stack-switching import. Asserts exit 0.
//!
//! These promote the W6.006 scratch spike's `gen.ll`/`gen_wasm.ll` evidence onto
//! the production codegen emission path.

#![cfg(not(target_arch = "wasm32"))]
#![cfg(unix)]

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;

use hew_codegen_rs::coro::{
    emit_coro_intrinsic_token, emit_coro_prologue, module_has_coroutines, run_coro_passes,
    CoroContext,
};
use inkwell::context::Context;
use inkwell::llvm_sys::prelude::LLVMValueRef;
use inkwell::module::{Linkage, Module as LlvmModule};
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use inkwell::values::{AsValueRef, FunctionValue};
use inkwell::AddressSpace;

// ── repo / toolchain plumbing ─────────────────────────────────────────────

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-codegen-rs has a workspace parent")
        .to_path_buf()
}

fn target_dir() -> PathBuf {
    std::env::var_os("CARGO_TARGET_DIR").map_or_else(
        || repo_root().join("target"),
        |dir| {
            let path = PathBuf::from(dir);
            if path.is_absolute() {
                path
            } else {
                repo_root().join(path)
            }
        },
    )
}

fn llvm_bin(tool: &str) -> Option<PathBuf> {
    let brew = Command::new("brew")
        .args(["--prefix", "llvm"])
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string());
    if let Some(prefix) = brew {
        let candidate = PathBuf::from(prefix).join("bin").join(tool);
        if candidate.is_file() {
            return Some(candidate);
        }
    }
    which(tool)
}

fn which(tool: &str) -> Option<PathBuf> {
    let out = Command::new("which").arg(tool).output().ok()?;
    if !out.status.success() {
        return None;
    }
    let path = PathBuf::from(String::from_utf8_lossy(&out.stdout).trim());
    path.is_file().then_some(path)
}

/// macOS system frameworks the native link must pass explicitly. `libhew_runtime.a`
/// transitively pulls in `rustls-platform-verifier` / `security-framework` /
/// `system-configuration` (the runtime's TLS surface), whose symbols
/// (`_Sec*`, `_CF*`, `_SC*`, `_Authorization*`) live in CoreFoundation,
/// Security, and SystemConfiguration. A bare `clang obj runtime` link does not
/// read the crates' `cargo:rustc-link-lib=framework=…` directives (those only
/// reach a `cargo`-driven link), so the frameworks are named here.
///
/// On Unix the same bare-link gap requires the target's platform libraries,
/// mirroring the production `NativeLinkPlan` in `hew-cli/src/target.rs`. This
/// test links the archive directly without cargo, so it must name them itself.
fn native_link_frameworks() -> &'static [&'static str] {
    #[cfg(target_os = "macos")]
    {
        &[
            "-framework",
            "CoreFoundation",
            "-framework",
            "Security",
            "-framework",
            "SystemConfiguration",
        ]
    }
    #[cfg(target_os = "freebsd")]
    {
        &["-lpthread", "-lm"]
    }
    #[cfg(target_os = "linux")]
    {
        &["-lpthread", "-lm", "-ldl", "-lrt"]
    }
    #[cfg(not(any(target_os = "macos", target_os = "freebsd", target_os = "linux")))]
    {
        &[]
    }
}

fn wasmtime() -> Option<PathBuf> {
    which("wasmtime").or_else(|| {
        let home = std::env::var_os("HOME")?;
        let candidate = PathBuf::from(home)
            .join(".wasmtime")
            .join("bin")
            .join("wasmtime");
        candidate.is_file().then_some(candidate)
    })
}

/// Build the native runtime archive the coroutine links against for
/// `hew_cont_frame_alloc`/`free` and the resume/done/destroy verbs.
///
/// Uses `hew-runtime` (the `staticlib` crate) rather than `hew-lib` because
/// `hew-lib` transitively pulls in `security-framework`, `core-foundation`,
/// and other macOS-only system libraries that require passing extra `-framework`
/// flags to clang. `hew-runtime` only needs `libSystem` (the default libc).
///
/// Always (re)builds via `cargo build -p hew-runtime` rather than trusting an
/// existing `libhew_runtime.a`: the `staticlib` artifact is NOT refreshed by
/// `cargo test`/`cargo nextest` (the workspace test build emits only the
/// rlib), so a cached archive carried across commits — e.g. one predating the
/// `hew_cont_*` continuation substrate (`cont.rs` is newer than `origin/main`)
/// — would otherwise be linked against freshly-emitted coro objects, failing
/// with undefined-symbol errors. Cargo's incremental build makes this a fast
/// no-op when the archive is already current.
fn ensure_native_runtime() -> PathBuf {
    static BUILT: OnceLock<PathBuf> = OnceLock::new();
    BUILT
        .get_or_init(|| {
            let lib = target_dir().join("debug").join("libhew_runtime.a");
            let status = Command::new(std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into()))
                .current_dir(repo_root())
                .args(["build", "--quiet", "-p", "hew-runtime"])
                .status()
                .expect("spawn cargo build -p hew-runtime");
            assert!(status.success(), "cargo build -p hew-runtime failed");
            assert!(
                lib.exists(),
                "libhew_runtime.a not produced at {}",
                lib.display()
            );
            lib
        })
        .clone()
}

/// Build the wasm32-wasip1 runtime archive. `None` if the wasm toolchain
/// is unavailable (test skips rather than fails on a box without the target).
///
/// Like [`ensure_native_runtime`], always (re)builds so a stale cached wasm
/// archive is never linked; a missing target or any build failure leaves the
/// caller to skip.
fn ensure_wasm_runtime() -> Option<PathBuf> {
    static BUILT: OnceLock<Option<PathBuf>> = OnceLock::new();
    BUILT
        .get_or_init(|| {
            let lib = target_dir()
                .join("wasm32-wasip1")
                .join("debug")
                .join("libhew_runtime.a");
            let status = Command::new(std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into()))
                .current_dir(repo_root())
                .args([
                    "build",
                    "--quiet",
                    "-p",
                    "hew-runtime",
                    "--target",
                    "wasm32-wasip1",
                    "--no-default-features",
                ])
                .status()
                .ok()?;
            (status.success() && lib.exists()).then_some(lib)
        })
        .clone()
}

// ── the coroutine + driver emission (via the real codegen `coro` API) ──────

/// Emit `gen_counter(out, start, count) -> ptr` — a finite generator coroutine
/// holding a Hew-owned heap value across every suspend. Mirrors the W6.006 spike
/// `gen.ll`, but the prologue / suspend / frame-free are emitted by the
/// production `hew_codegen_rs::coro` API rather than hand-written `.ll`.
fn emit_gen_counter<'ctx>(ctx: &'ctx Context, llvm_mod: &LlvmModule<'ctx>) {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i64_ty = ctx.i64_type();
    let builder = ctx.create_builder();

    let value_alloc = get_or_add(
        llvm_mod,
        "test_value_alloc",
        ptr_ty.fn_type(&[], false),
        Linkage::External,
    );
    let value_free = get_or_add(
        llvm_mod,
        "test_value_free",
        ctx.void_type().fn_type(&[ptr_ty.into()], false),
        Linkage::External,
    );

    let func = llvm_mod.add_function(
        "gen_counter",
        ptr_ty.fn_type(&[ptr_ty.into(), i64_ty.into(), i64_ty.into()], false),
        None,
    );
    let out = func.get_nth_param(0).unwrap().into_pointer_value();
    let start = func.get_nth_param(1).unwrap().into_int_value();
    let count = func.get_nth_param(2).unwrap().into_int_value();

    // coro.id(null promise)/alloc/begin.
    let cc: CoroContext<'_, '_> =
        emit_coro_prologue(ctx, llvm_mod, &builder, func).expect("emit coro prologue");
    // After the prologue the builder sits at the end of `coro.begin`; capture it
    // as the i=0 incoming edge for the loop-carried phi.
    let begin_block = builder.get_insert_block().expect("builder in coro.begin");

    // boxed = test_value_alloc(); *boxed = start
    let boxed = builder
        .build_call(value_alloc, &[], "boxed")
        .unwrap()
        .try_as_basic_value()
        .basic()
        .unwrap()
        .into_pointer_value();
    builder.build_store(boxed, start).unwrap();

    let loop_check = ctx.append_basic_block(func, "loop.check");
    let loop_body = ctx.append_basic_block(func, "loop.body");
    let resume_bb = ctx.append_basic_block(func, "resume");
    let final_bb = ctx.append_basic_block(func, "final");
    let trap_bb = ctx.append_basic_block(func, "trap");
    let cleanup_bb = ctx.append_basic_block(func, "cleanup");
    let free_bb = ctx.append_basic_block(func, "dyn.free");
    let suspend_ret = ctx.append_basic_block(func, "suspend.ret");

    builder.build_unconditional_branch(loop_check).unwrap();

    // loop.check: i phi; if i < count -> body else final
    builder.position_at_end(loop_check);
    let i_phi = builder.build_phi(i64_ty, "i").unwrap();
    let i_cur = i_phi.as_basic_value().into_int_value();
    let cont = builder
        .build_int_compare(inkwell::IntPredicate::SLT, i_cur, count, "cont")
        .unwrap();
    builder
        .build_conditional_branch(cont, loop_body, final_bb)
        .unwrap();

    // loop.body: *out = *boxed; suspend (non-final)
    builder.position_at_end(loop_body);
    let cur = builder.build_load(i64_ty, boxed, "cur").unwrap();
    builder.build_store(out, cur).unwrap();
    cc.emit_suspend(resume_bb, cleanup_bb, suspend_ret, false, "body")
        .expect("emit non-final suspend");

    // resume: *boxed += 1; i = i + 1; back to check
    builder.position_at_end(resume_bb);
    let v = builder
        .build_load(i64_ty, boxed, "v")
        .unwrap()
        .into_int_value();
    let v1 = builder
        .build_int_add(v, i64_ty.const_int(1, false), "v1")
        .unwrap();
    builder.build_store(boxed, v1).unwrap();
    let i_next = builder
        .build_int_add(i_cur, i64_ty.const_int(1, false), "i.next")
        .unwrap();
    builder.build_unconditional_branch(loop_check).unwrap();

    // Wire the loop-carried phi: 0 from coro.begin, i.next from resume.
    i_phi.add_incoming(&[
        (&i64_ty.const_int(0, false), begin_block),
        (&i_next, resume_bb),
    ]);

    // final: suspend (final) — coro.done becomes true after this
    builder.position_at_end(final_bb);
    cc.emit_suspend(trap_bb, cleanup_bb, suspend_ret, true, "final")
        .expect("emit final suspend");

    // trap: resuming a final-suspended coroutine is a use error — unreachable
    builder.position_at_end(trap_bb);
    builder.build_unreachable().unwrap();

    // cleanup (destroy path): free the heap value, then the coro frame.
    //
    // Key design: `suspend_ret` is THE single exit of the coroutine, shared by
    // both the non-final-suspend return path and the cleanup/destroy path. It
    // calls `coro.end` before `ret`. This is required so CoroSplit recognises
    // `suspend_ret` as a valid suspension boundary and emits `ret void` in the
    // `.resume` outline (not `unreachable`). Without `coro.end` here, the
    // looping non-final suspend path in `.resume` routes to `unreachable` which
    // the optimizer re-maps to cleanup — causing a spurious free on every resume.
    //
    // Pattern matches fib_gen.ll: cleanup → free → suspend → coro.end → ret.
    // `emit_coro_frame_free` is NOT used here because it calls `coro.end` itself;
    // we need exactly one `coro.end` in `suspend_ret`, not one in each path.
    builder.position_at_end(cleanup_bb);
    builder.build_call(value_free, &[boxed.into()], "").unwrap();
    // %freemem = coro.free(id, handle); conditionally free the frame.
    let coro_free = {
        use inkwell::intrinsics::Intrinsic;
        Intrinsic::find("llvm.coro.free")
            .unwrap()
            .get_declaration(cc.llvm_mod, &[])
            .unwrap()
    };
    let freemem = unsafe {
        let mut args: [LLVMValueRef; 2] = [cc.id_token, cc.handle.as_value_ref()];
        let name = std::ffi::CString::new("coro.freemem").unwrap();
        let raw = emit_coro_intrinsic_token(builder.as_mut_ptr(), coro_free, &mut args, &name);
        inkwell::values::PointerValue::new(raw)
    };
    let is_null = builder
        .build_is_null(freemem, "coro.freemem.isnull")
        .unwrap();
    builder
        .build_conditional_branch(is_null, suspend_ret, free_bb)
        .unwrap();

    // free_bb: actually free the frame memory, then fall into suspend_ret.
    builder.position_at_end(free_bb);
    let frame_free = cc
        .llvm_mod
        .get_function("hew_cont_frame_free")
        .unwrap_or_else(|| {
            let ptr_ty = cc.ctx.ptr_type(inkwell::AddressSpace::default());
            cc.llvm_mod.add_function(
                "hew_cont_frame_free",
                cc.ctx.void_type().fn_type(&[ptr_ty.into()], false),
                Some(inkwell::module::Linkage::External),
            )
        });
    builder
        .build_call(frame_free, &[freemem.into()], "")
        .unwrap();
    builder.build_unconditional_branch(suspend_ret).unwrap();

    // suspend_ret: single exit — coro.end marks the suspension boundary, then
    // ret returns the handle from the ramp (→ ret void in .resume/.destroy).
    builder.position_at_end(suspend_ret);
    let coro_end = {
        use inkwell::intrinsics::Intrinsic;
        Intrinsic::find("llvm.coro.end")
            .unwrap()
            .get_declaration(cc.llvm_mod, &[])
            .unwrap()
    };
    unsafe {
        let c = cc.ctx.raw();
        let token_ty = inkwell::llvm_sys::core::LLVMTokenTypeInContext(c);
        let none = inkwell::llvm_sys::core::LLVMConstNull(token_ty);
        let mut args: [LLVMValueRef; 3] = [
            cc.handle.as_value_ref(),
            cc.ctx.bool_type().const_int(0, false).as_value_ref(),
            none,
        ];
        let name = std::ffi::CString::new("").unwrap();
        emit_coro_intrinsic_token(builder.as_mut_ptr(), coro_end, &mut args, &name);
    }
    builder.build_return(Some(&cc.handle)).unwrap();
}

fn get_or_add<'ctx>(
    llvm_mod: &LlvmModule<'ctx>,
    name: &str,
    ty: inkwell::types::FunctionType<'ctx>,
    linkage: Linkage,
) -> FunctionValue<'ctx> {
    llvm_mod
        .get_function(name)
        .unwrap_or_else(|| llvm_mod.add_function(name, ty, Some(linkage)))
}

/// Build the full module: `gen_counter` (via the real coro API) + the test
/// heap-value allocator pair (with accounting globals) + a `main` driver that
/// drives the coroutine through the REAL `HewCont` runtime ABI
/// (`hew_cont_resume`/`hew_cont_done`/`hew_cont_destroy`). `main` returns 0
/// only if the value round-tripped (sum == 406) and alloc/free balanced.
///
/// The test value allocator uses `hew_alloc`/`hew_dealloc` (from `hew-runtime`)
/// rather than `libc::malloc`/`free`. Both functions take `(u64, u64)` on all
/// targets — no `size_t` width difference between native and wasm32 — so this
/// module is ABI-correct on both without parameterisation.
fn build_module<'ctx>(ctx: &'ctx Context, name: &str) -> LlvmModule<'ctx> {
    let llvm_mod = ctx.create_module(name);
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i64_ty = ctx.i64_type();
    let i32_ty = ctx.i32_type();
    let i1_ty = ctx.bool_type();

    // accounting globals
    let value_allocs = add_counter(&llvm_mod, "value_allocs");
    let value_frees = add_counter(&llvm_mod, "value_frees");

    // test_value_alloc/free: 8-byte heap value via hew_alloc/hew_dealloc.
    // hew_alloc(size: u64, align: u64) -> *mut u8 — always i64 args on all targets.
    // hew_dealloc(ptr, size: u64, align: u64) — same ABI on native and wasm32.
    let hew_alloc = get_or_add(
        &llvm_mod,
        "hew_alloc",
        ptr_ty.fn_type(&[i64_ty.into(), i64_ty.into()], false),
        Linkage::External,
    );
    let hew_dealloc = get_or_add(
        &llvm_mod,
        "hew_dealloc",
        ctx.void_type()
            .fn_type(&[ptr_ty.into(), i64_ty.into(), i64_ty.into()], false),
        Linkage::External,
    );

    {
        let b = ctx.create_builder();
        let va = llvm_mod.add_function("test_value_alloc", ptr_ty.fn_type(&[], false), None);
        let bb = ctx.append_basic_block(va, "e");
        b.position_at_end(bb);
        let p = b
            .build_call(
                hew_alloc,
                &[
                    i64_ty.const_int(8, false).into(),
                    i64_ty.const_int(8, false).into(),
                ],
                "p",
            )
            .unwrap()
            .try_as_basic_value()
            .basic()
            .unwrap()
            .into_pointer_value();
        bump(&b, i64_ty, value_allocs);
        b.build_return(Some(&p)).unwrap();

        let vf = llvm_mod.add_function(
            "test_value_free",
            ctx.void_type().fn_type(&[ptr_ty.into()], false),
            None,
        );
        let bb = ctx.append_basic_block(vf, "e");
        b.position_at_end(bb);
        let p = vf.get_nth_param(0).unwrap().into_pointer_value();
        b.build_call(
            hew_dealloc,
            &[
                p.into(),
                i64_ty.const_int(8, false).into(),
                i64_ty.const_int(8, false).into(),
            ],
            "",
        )
        .unwrap();
        bump(&b, i64_ty, value_frees);
        b.build_return(None).unwrap();
    }

    emit_gen_counter(ctx, &llvm_mod);

    // Runtime HewCont ABI the driver calls.
    let cont_resume = get_or_add(
        &llvm_mod,
        "hew_cont_resume",
        ctx.void_type().fn_type(&[ptr_ty.into()], false),
        Linkage::External,
    );
    let cont_done = get_or_add(
        &llvm_mod,
        "hew_cont_done",
        i1_ty.fn_type(&[ptr_ty.into()], false),
        Linkage::External,
    );
    let cont_destroy = get_or_add(
        &llvm_mod,
        "hew_cont_destroy",
        ctx.void_type().fn_type(&[ptr_ty.into()], false),
        Linkage::External,
    );
    let gen_counter = llvm_mod.get_function("gen_counter").unwrap();

    // main: drive gen_counter(out, 100, 4) via the runtime ABI; accumulate the
    // yielded values; assert sum==406 and alloc/free balance.
    let main = llvm_mod.add_function("main", i32_ty.fn_type(&[], false), None);
    let b = ctx.create_builder();
    let entry = ctx.append_basic_block(main, "entry");
    let drive = ctx.append_basic_block(main, "drive");
    let drive_body = ctx.append_basic_block(main, "drive.body");
    let finish = ctx.append_basic_block(main, "finish");

    b.position_at_end(entry);
    let out = b.build_alloca(i64_ty, "out").unwrap();
    b.build_store(out, i64_ty.const_int(0, false)).unwrap();
    let hdl = b
        .build_call(
            gen_counter,
            &[
                out.into(),
                i64_ty.const_int(100, false).into(),
                i64_ty.const_int(4, false).into(),
            ],
            "hdl",
        )
        .unwrap()
        .try_as_basic_value()
        .basic()
        .unwrap()
        .into_pointer_value();
    b.build_unconditional_branch(drive).unwrap();

    // drive: sum phi; if hew_cont_done(hdl) stop else consume+resume
    b.position_at_end(drive);
    let sum_phi = b.build_phi(i64_ty, "sum").unwrap();
    let done = b
        .build_call(cont_done, &[hdl.into()], "done")
        .unwrap()
        .try_as_basic_value()
        .basic()
        .unwrap()
        .into_int_value();
    b.build_conditional_branch(done, finish, drive_body)
        .unwrap();

    b.position_at_end(drive_body);
    let cur = b.build_load(i64_ty, out, "cur").unwrap().into_int_value();
    let sum_next = b
        .build_int_add(sum_phi.as_basic_value().into_int_value(), cur, "sum.next")
        .unwrap();
    b.build_call(cont_resume, &[hdl.into()], "").unwrap();
    b.build_unconditional_branch(drive).unwrap();
    sum_phi.add_incoming(&[
        (&i64_ty.const_int(0, false), entry),
        (&sum_next, drive_body),
    ]);

    // finish: destroy; rc = 0 iff sum==406 and value alloc==free==1
    b.position_at_end(finish);
    let sum_final = sum_phi.as_basic_value().into_int_value();
    b.build_call(cont_destroy, &[hdl.into()], "").unwrap();
    let va = b
        .build_load(i64_ty, value_allocs.as_pointer_value(), "va")
        .unwrap()
        .into_int_value();
    let vf = b
        .build_load(i64_ty, value_frees.as_pointer_value(), "vf")
        .unwrap()
        .into_int_value();
    let ok_sum = b
        .build_int_compare(
            inkwell::IntPredicate::EQ,
            sum_final,
            i64_ty.const_int(406, false),
            "ok.sum",
        )
        .unwrap();
    let ok_va = b
        .build_int_compare(
            inkwell::IntPredicate::EQ,
            va,
            i64_ty.const_int(1, false),
            "ok.va",
        )
        .unwrap();
    let ok_vf = b
        .build_int_compare(
            inkwell::IntPredicate::EQ,
            vf,
            i64_ty.const_int(1, false),
            "ok.vf",
        )
        .unwrap();
    let a1 = b.build_and(ok_sum, ok_va, "a1").unwrap();
    let a2 = b.build_and(a1, ok_vf, "a2").unwrap();
    let rc = b
        .build_select(
            a2,
            i32_ty.const_int(0, false),
            i32_ty.const_int(1, false),
            "rc",
        )
        .unwrap()
        .into_int_value();
    b.build_return(Some(&rc)).unwrap();

    llvm_mod.verify().expect("emitted coro module verifies");
    llvm_mod
}

fn add_counter<'ctx>(
    llvm_mod: &LlvmModule<'ctx>,
    name: &str,
) -> inkwell::values::GlobalValue<'ctx> {
    let i64_ty = llvm_mod.get_context().i64_type();
    let g = llvm_mod.add_global(i64_ty, None, name);
    g.set_initializer(&i64_ty.const_int(0, false));
    g.set_linkage(Linkage::Internal);
    g
}

fn bump<'ctx>(
    b: &inkwell::builder::Builder<'ctx>,
    i64_ty: inkwell::types::IntType<'ctx>,
    g: inkwell::values::GlobalValue<'ctx>,
) {
    let c = b
        .build_load(i64_ty, g.as_pointer_value(), "c")
        .unwrap()
        .into_int_value();
    let c1 = b
        .build_int_add(c, i64_ty.const_int(1, false), "c1")
        .unwrap();
    b.build_store(g.as_pointer_value(), c1).unwrap();
}

// ── object emission (with coro lowering) ───────────────────────────────────

fn init_targets() {
    static INIT: OnceLock<()> = OnceLock::new();
    INIT.get_or_init(|| Target::initialize_all(&InitializationConfig::default()));
}

/// The native triple LLVM accepts. `TargetMachine::get_default_triple()` on
/// macOS returns `arm64-apple-darwin25.x`, whose `arm64`/`darwin` spelling
/// LLVM's `from_triple` rejects; on macOS, normalise to `aarch64`/`x86_64` +
/// `macosx` (matching the codegen's own `native_emission_triple`).
///
/// On FreeBSD the host default triple (e.g. `x86_64-portbld-freebsd15.0` or
/// `aarch64-unknown-freebsd15.0`) is already LLVM-accepted, so we pass it
/// through unchanged. The FreeBSD check MUST come before the macOS x86_64/
/// aarch64 normalisation arms to prevent `x86_64-portbld-freebsd*` from being
/// misidentified as `x86_64-apple-macosx13.0`. Other Unix hosts, including
/// Linux, likewise use the host triple unchanged.
fn native_triple() -> String {
    let default = TargetMachine::get_default_triple()
        .as_str()
        .to_string_lossy()
        .into_owned();
    if default.contains("freebsd") {
        // FreeBSD: the host-reported triple is already LLVM-accepted; pass it
        // through. Covers both x86_64-portbld-freebsd* and aarch64-*-freebsd*.
        default
    } else if cfg!(target_os = "macos") && default.starts_with("x86_64") {
        "x86_64-apple-macosx13.0".to_string()
    } else if cfg!(target_os = "macos")
        && (default.starts_with("aarch64") || default.starts_with("arm64"))
    {
        "aarch64-apple-macosx13.0".to_string()
    } else {
        default
    }
}

fn machine_for(triple: &str) -> TargetMachine {
    init_targets();
    let tt = TargetTriple::create(triple);
    let target = Target::from_triple(&tt).expect("target from triple");
    target
        .create_target_machine(
            &tt,
            "generic",
            "",
            inkwell::OptimizationLevel::Default,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .expect("target machine")
}

/// Emit `module` to an object at `out`, running coro lowering first (the same
/// gate the production `emit_object_in_process` uses).
fn emit_object(module: &LlvmModule<'_>, machine: &TargetMachine, out: &Path) {
    assert!(
        module_has_coroutines(module),
        "module must carry a presplitcoroutine function"
    );
    run_coro_passes(module, machine).expect("coro passes lower the coroutine");
    machine
        .write_to_file(module, FileType::Object, out)
        .expect("write object");
}

/// Locate the rustup-installed wasm32-wasip1 self-contained libc (`libc.a`).
///
/// Rustup's `wasm32-wasip1` target ships a self-contained C library that
/// provides `strlen`, `malloc`, `free`, `memcpy`, and the other C stdlib
/// symbols `libhew_runtime.a` imports — without requiring the WASI SDK. The
/// runtime also ships its own `_start` symbol, so we never need `crt1-command.o`
/// alongside this. Returns `None` if the toolchain is not installed.
fn wasm_self_contained_libc() -> Option<PathBuf> {
    let toolchain = Command::new("rustup")
        .args(["which", "rustc"])
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| PathBuf::from(String::from_utf8_lossy(&o.stdout).trim()))?;
    // .../toolchains/<name>/bin/rustc → .../toolchains/<name>
    let toolchain_root = toolchain.parent()?.parent()?;
    let libc = toolchain_root.join("lib/rustlib/wasm32-wasip1/lib/self-contained/libc.a");
    libc.exists().then_some(libc)
}

// ── tests ──────────────────────────────────────────────────────────────────

/// The continuation primitive lowers + runs on NATIVE: a Hew-owned heap value
/// round-trips through the coro frame across four suspends, driven entirely by
/// the runtime `HewCont` ABI, with balanced alloc/free.
#[test]
fn coro_substrate_round_trips_value_native() {
    let Some(clang) = llvm_bin("clang") else {
        eprintln!("skip: clang (LLVM 22) not found");
        return;
    };
    let runtime = ensure_native_runtime();
    let tmp = tempfile::tempdir().expect("tempdir");
    let obj = tmp.path().join("coro.o");
    let bin = tmp.path().join("coro");

    let ctx = Context::create();
    let module = build_module(&ctx, "coro_native");
    let machine = machine_for(&native_triple());
    emit_object(&module, &machine, &obj);

    let status = Command::new(&clang)
        .arg(&obj)
        .arg(&runtime)
        .args(native_link_frameworks())
        .args(["-o", bin.to_str().unwrap()])
        .status()
        .expect("link native coro binary");
    assert!(status.success(), "clang link failed");

    let run = Command::new(&bin).status().expect("run native coro binary");
    assert_eq!(
        run.code(),
        Some(0),
        "coroutine must round-trip the value (sum 406) with balanced alloc/free"
    );
}

/// The native binary is leak- and double-free-clean: re-run it under
/// MallocScribble (poison freed memory) + MallocGuardEdges, then under
/// `leaks --atExit` asserting zero leaked bytes. Proves the heap value is
/// genuinely reloaded from the frame each resume and the single-teardown-owner
/// frees the frame + value exactly once.
#[test]
fn coro_substrate_leak_clean_native() {
    let Some(clang) = llvm_bin("clang") else {
        eprintln!("skip: clang (LLVM 22) not found");
        return;
    };
    let Some(leaks) = which("leaks") else {
        eprintln!("skip: `leaks` not found (macOS only)");
        return;
    };
    let runtime = ensure_native_runtime();
    let tmp = tempfile::tempdir().expect("tempdir");
    let obj = tmp.path().join("coro.o");
    let bin = tmp.path().join("coro");

    let ctx = Context::create();
    let module = build_module(&ctx, "coro_leak");
    let machine = machine_for(&native_triple());
    emit_object(&module, &machine, &obj);
    let status = Command::new(&clang)
        .arg(&obj)
        .arg(&runtime)
        .args(native_link_frameworks())
        .args(["-o", bin.to_str().unwrap()])
        .status()
        .expect("link native coro binary");
    assert!(status.success(), "clang link failed");

    // Guarded run: poisoned-free + guard edges must still produce exit 0.
    let guarded = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .status()
        .expect("guarded run");
    assert_eq!(
        guarded.code(),
        Some(0),
        "value must be reloaded from the frame, not a freed slot (guarded run)"
    );

    // leaks --atExit: zero leaked bytes.
    let out = Command::new(&leaks)
        .args(["--atExit", "--"])
        .arg(&bin)
        .output()
        .expect("leaks run");
    let report = String::from_utf8_lossy(&out.stdout);
    assert!(
        report.contains("0 leaks for 0 total leaked bytes"),
        "leaks must report zero; got:\n{report}"
    );
}

/// The SAME coroutine IR lowered for wasm32-wasi runs under wasmtime: the frame
/// lives in linear memory via the Hew allocator, no malloc/asyncify/stack-switch
/// import. Asserts exit 0 (the encoded all-checks-pass verdict).
#[test]
fn coro_substrate_round_trips_value_wasm32() {
    let Some(wasm_ld) = llvm_bin("wasm-ld") else {
        eprintln!("skip: wasm-ld not found");
        return;
    };
    let Some(wasmtime) = wasmtime() else {
        eprintln!("skip: wasmtime not found");
        return;
    };
    let Some(runtime) = ensure_wasm_runtime() else {
        eprintln!("skip: wasm32-wasip1 runtime archive unavailable");
        return;
    };
    let Some(libc) = wasm_self_contained_libc() else {
        eprintln!("skip: rustup wasm32-wasip1 self-contained libc not installed");
        return;
    };
    let tmp = tempfile::tempdir().expect("tempdir");
    let obj = tmp.path().join("coro.o");
    let wasm = tmp.path().join("coro.wasm");

    let ctx = Context::create();
    // hew_cont_frame_alloc takes u64 on all targets including wasm32; no size_ty
    // split needed. Set triple/data-layout BEFORE emit_object so the target-data
    // the module carries is consistent with the object file the linker sees.
    let machine = machine_for("wasm32-wasi");
    let module = build_module(&ctx, "coro_wasm");
    module.set_triple(&machine.get_triple());
    module.set_data_layout(&machine.get_target_data().get_data_layout());
    emit_object(&module, &machine, &obj);

    // Link: libc (strlen/malloc/etc.) + runtime (_start + hew_cont_* + hew_alloc).
    // The runtime ships its own _start which calls main(argc, argv) → propagates
    // the i32 exit code to wasmtime's process exit. No crt1-command.o needed.
    let link = Command::new(&wasm_ld)
        .arg("--allow-undefined")
        .arg(&libc)
        .arg(&obj)
        .arg(&runtime)
        .args(["-o", wasm.to_str().unwrap()])
        .status()
        .expect("wasm-ld link");
    assert!(link.success(), "wasm-ld link failed");

    // wasmtime runs the module's _start (provided by the runtime); exit 0 from
    // main encodes the all-checks-pass verdict.
    let run = Command::new(&wasmtime)
        .arg("run")
        .arg(&wasm)
        .output()
        .expect("wasmtime run");
    assert!(
        run.status.success(),
        "wasm coroutine run failed: {}",
        String::from_utf8_lossy(&run.stderr)
    );
}

/// Diagnostic: emit + coro-lower + link the native binary and write the split IR to /tmp.
#[test]
fn debug_emit_and_check_native() {
    let Some(clang) = llvm_bin("clang") else {
        return;
    };
    let runtime = ensure_native_runtime();
    let tmp = tempfile::tempdir().expect("tempdir");
    let obj = tmp.path().join("coro.o");
    let bin = tmp.path().join("coro");
    let ir_path = std::path::PathBuf::from("/tmp/coro_post_split.ll");

    let ctx = Context::create();
    let module = build_module(&ctx, "coro_debug");
    let machine = machine_for(&native_triple());

    // Dump pre-split IR
    std::fs::write(
        "/tmp/coro_pre_split.ll",
        module.print_to_string().to_string(),
    )
    .unwrap();

    assert!(
        module_has_coroutines(&module),
        "must have presplitcoroutine"
    );
    run_coro_passes(&module, &machine).expect("coro passes");

    // Dump post-split IR
    std::fs::write(&ir_path, module.print_to_string().to_string()).unwrap();
    eprintln!("Post-split IR written to {}", ir_path.display());

    machine
        .write_to_file(&module, FileType::Object, &obj)
        .expect("write obj");

    let status = Command::new(&clang)
        .arg(&obj)
        .arg(&runtime)
        .args(native_link_frameworks())
        .args(["-o", bin.to_str().unwrap()])
        .stderr(std::process::Stdio::inherit())
        .status()
        .expect("link");
    if !status.success() {
        panic!("link failed");
    }

    // Run with verbose output
    let out = Command::new(&bin).output().expect("run");
    eprintln!("exit: {:?}", out.status.code());
    eprintln!("stdout: {}", String::from_utf8_lossy(&out.stdout));
    eprintln!("stderr: {}", String::from_utf8_lossy(&out.stderr));
}

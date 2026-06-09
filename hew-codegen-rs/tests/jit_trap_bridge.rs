//! JIT execution tests: verify that compiled Hew IR actually fires the
//! `hew_trap_with_code` supervisor seam at runtime, not just in the
//! emitted IR text.
//!
//! The existing `trap_kind_emission.rs` and `overflow_trap_emission.rs`
//! tests verify that the emitted LLVM IR *contains* the correct
//! `@hew_trap_with_code(i32 N)` call sites. This file verifies the
//! complementary runtime property: that executing the compiled module
//! through inkwell's MCJIT actually invokes the function with the
//! expected trap code.
//!
//! ## Method
//!
//! 1. Compile a Hew source through the full pipeline (HIR → MIR → codegen)
//!    to a textual `.ll` file via `emit_module`.
//! 2. Parse the `.ll` back into an inkwell `Module` with
//!    `Context::create_module_from_ir`, matching the diagnostic roundtrip
//!    used by these JIT tests.
//! 3. Create an MCJIT `ExecutionEngine` on the module.
//! 4. Register a test-local `extern "C"` trap recorder via
//!    `ExecutionEngine::add_global_mapping`. The recorder stores the
//!    received code in a `thread_local!` and uses `siglongjmp` to jump
//!    back to the `sigsetjmp` guard in `jit_run_main`, so `@llvm.trap`
//!    never executes.
//! 5. After the `siglongjmp` returns to the guard, assert the recorded
//!    code matches the expected constant.
//!
//! ## Why `sigsetjmp`/`siglongjmp` instead of `catch_unwind`
//!
//! `extern "C-unwind"` + `catch_unwind` does not prevent `@llvm.trap`
//! from executing. MCJIT does not register EH landing pads for the call
//! to `hew_trap_with_code` (the declaration is marked `void` and treated
//! as non-throwing by LLVM), so the unwind from the recorder reaches the
//! JIT frame boundary without the JIT recognising it as a Rust unwind;
//! the `llvm.trap` instruction in the same BB runs and produces SIGILL,
//! which starts a second panic — a double-panic aborts the process.
//!
//! `siglongjmp` is an unconditional non-local jump: the recorder stores
//! the code and immediately restores the CPU to the guard frame, before
//! `@llvm.trap` can execute. This is the same escape mechanism that
//! `hew_trap_with_code` uses in production via
//! `try_direct_longjmp_with_code` (see `hew-runtime/src/signal.rs`),
//! so the test mock is semantically accurate.
//!
//! The `SigJmpBuf` layout and the `sigsetjmp`/`siglongjmp` FFI
//! declarations are borrowed from `hew-runtime`'s identical pattern.
//!
//! ## Trap kinds covered
//!
//! - `IntegerOverflow` (201): `i64::MAX + 1` — overflow flag set by
//!   `llvm.sadd.with.overflow.i64`.
//! - `DivideByZero` (202): `a / 0` — zero-divisor check fires.
//! - `ShiftOutOfRange` (204): `a << 64` — `icmp uge shift_amount, 64`
//!   is true for `shift_amount = 64`.
//!
//! `IndexOutOfBounds` (205) is not included because Vec literal
//! construction is not yet in the Cluster 1 source-level pipeline; the
//! C-2 emission tests use hand-crafted `IrPipeline` fixtures instead.
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): the trap path must fire through the
//!   supervisor seam, not via process abort from `llvm.trap` alone.
//! - `exhaustive-coverage` (P0): one test per trap kind reachable from
//!   source-level compilation in the current spine.

#![cfg(not(target_arch = "wasm32"))]
#![cfg(unix)]

use std::cell::Cell;
use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_types::TypeCheckOutput;
use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::OptimizationLevel;

// ---------------------------------------------------------------------------
// sigsetjmp / siglongjmp FFI
//
// The libc crate does not expose sigjmp_buf as a usable Rust type, so we
// declare the buffer ourselves (over-allocated for portability, matching
// the pattern in hew-runtime/src/signal.rs) and bind the POSIX functions
// directly.
// ---------------------------------------------------------------------------

/// Over-allocated save buffer for `sigsetjmp`. The conservative sizes match
/// `hew-runtime`'s `SigJmpBuf`:
/// - x86_64: 256 bytes (glibc `__jmp_buf_tag` is 200 bytes)
/// - aarch64/other: 512 bytes
#[repr(C, align(16))]
struct SigJmpBuf {
    #[cfg(target_arch = "x86_64")]
    _buf: [u8; 256],
    #[cfg(not(target_arch = "x86_64"))]
    _buf: [u8; 512],
}

impl SigJmpBuf {
    const fn zeroed() -> Self {
        Self {
            _buf: [0u8; {
                #[cfg(target_arch = "x86_64")]
                {
                    256
                }
                #[cfg(not(target_arch = "x86_64"))]
                {
                    512
                }
            }],
        }
    }
}

extern "C" {
    // On Linux/glibc, `sigsetjmp` is a macro that expands to `__sigsetjmp`.
    // On macOS, `sigsetjmp` is the actual symbol name.
    #[cfg_attr(target_os = "linux", link_name = "__sigsetjmp")]
    fn sigsetjmp(env: *mut SigJmpBuf, savemask: libc::c_int) -> libc::c_int;
    fn siglongjmp(env: *mut SigJmpBuf, val: libc::c_int) -> !;
}

// ---------------------------------------------------------------------------
// Trap recorder
// ---------------------------------------------------------------------------

thread_local! {
    /// The last trap code received by `test_trap_recorder`.
    static LAST_TRAP_CODE: Cell<Option<i32>> = const { Cell::new(None) };

    /// The `sigsetjmp` guard buffer for the current JIT call.
    ///
    /// Set to `Some(buf_ptr)` immediately before `jit_main.call()` and
    /// cleared after the guard frame exits. The recorder jumps to this
    /// buffer on trap.
    ///
    /// # Safety invariant
    ///
    /// The pointer is only valid while the corresponding `SigJmpBuf` is
    /// live on the `jit_run_main` stack. The test recorder and
    /// `jit_run_main` run on the same thread, so there is no cross-thread
    /// pointer race.
    static JMP_TARGET: Cell<*mut SigJmpBuf> = const { Cell::new(std::ptr::null_mut()) };
}

/// Mock `hew_trap_with_code` registered into the JIT engine.
///
/// Stores `code` in `LAST_TRAP_CODE`, then `siglongjmp`s back to the
/// guard in `jit_run_main` before `@llvm.trap` can execute. The
/// longjmp return value `1` distinguishes the trap path from the normal
/// path (where `sigsetjmp` returns 0).
///
/// # Safety
///
/// - Must only be called from a JIT-compiled function that was invoked
///   from `jit_run_main` on the same thread.
/// - `JMP_TARGET` must be non-null (set by `jit_run_main`).
/// - The `SigJmpBuf` pointed to by `JMP_TARGET` must still be live.
extern "C" fn test_trap_recorder(code: i32) {
    LAST_TRAP_CODE.with(|c| c.set(Some(code)));
    let buf_ptr = JMP_TARGET.with(|p| p.get());
    // SAFETY: buf_ptr is live on jit_run_main's stack frame (same thread).
    unsafe { siglongjmp(buf_ptr, 1) };
}

// ---------------------------------------------------------------------------
// Pipeline helper
// ---------------------------------------------------------------------------

/// Compile `source` through the full Hew pipeline and return the path of
/// the emitted `.ll` file. Panics with a descriptive message at the first
/// stage that fails.
fn compile_to_ll(source: &str, module_name: &str) -> std::path::PathBuf {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "parse: {:?}", parsed.errors);

    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let verify = verify_hir(&output.module);
    assert!(
        output.diagnostics.is_empty() && verify.is_empty(),
        "hir/verify: {:?} {:?}",
        output.diagnostics,
        verify
    );

    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "mir: {:?}",
        pipeline.diagnostics
    );

    let tmp = std::env::temp_dir().join(format!("hew-jit-trap-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let artefacts = emit_module(&pipeline, &options)
        .unwrap_or_else(|e| panic!("emit_module for {module_name}: {e}"));
    artefacts
        .ll_path
        .expect("emit_module must populate ll_path")
}

/// JIT-execute the `main` function from `ll_path`, with `test_trap_recorder`
/// registered as `hew_trap_with_code`.
///
/// Returns `Err(code)` if the trap recorder fired (the expected path for
/// programs that always trap), `Ok(val)` if `main` returned cleanly.
///
/// Initializes the native target; redundant inits are safe.
fn jit_run_main(ll_path: &Path) -> Result<i64, i32> {
    Target::initialize_native(&InitializationConfig::default())
        .expect("initialize_native must succeed on the host platform");

    let ctx = Context::create();
    let buf = MemoryBuffer::create_from_file(ll_path)
        .unwrap_or_else(|e| panic!("read {}: {e}", ll_path.display()));
    let module = ctx
        .create_module_from_ir(buf)
        .unwrap_or_else(|e| panic!("parse {}: {e}", ll_path.display()));

    // Look up the hew_trap_with_code declaration before the JIT takes
    // ownership of the module.
    let trap_fn = module
        .get_function("hew_trap_with_code")
        .expect("emitted module must declare hew_trap_with_code");

    let ee = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .expect("create_jit_execution_engine must succeed; ensure native target is initialised");

    // Point the JIT's external symbol resolver at our recorder.
    ee.add_global_mapping(&trap_fn, test_trap_recorder as *const () as usize);

    // Reset the thread-locals before calling so stale state from earlier
    // tests doesn't pollute assertions.
    LAST_TRAP_CODE.with(|c| c.set(None));

    // SAFETY: jit_main is `fn() -> i64` matching the HIR function signature;
    // the JIT-compiled code is for the host triple.
    let jit_main = unsafe {
        ee.get_function::<unsafe extern "C" fn() -> i64>("main")
            .expect("main must be present in the JIT module")
    };

    // Guard frame: sigsetjmp saves the CPU state here. If test_trap_recorder
    // fires siglongjmp, execution returns here with val=1.
    let mut jmp_buf = SigJmpBuf::zeroed();
    JMP_TARGET.with(|p| p.set(&raw mut jmp_buf));

    // SAFETY: jmp_buf lives for the duration of this function, and
    // JMP_TARGET is read only by test_trap_recorder on the same thread.
    let setjmp_val = unsafe { sigsetjmp(&raw mut jmp_buf, 0) };

    let result = if setjmp_val == 0 {
        // Normal path: call the JIT entry point.
        // SAFETY: jit_main is a valid JIT-compiled function.
        let val = unsafe { jit_main.call() };
        Ok(val)
    } else {
        // Trap path: recorder siglongjmp'd back here.
        let code = LAST_TRAP_CODE
            .with(|c| c.get())
            .expect("trap recorder must store a code before siglongjmp");
        Err(code)
    };

    // Clear the pointer before jmp_buf drops off the stack.
    JMP_TARGET.with(|p| p.set(std::ptr::null_mut()));

    result
}

// ---------------------------------------------------------------------------
// IntegerOverflow — code 201
// ---------------------------------------------------------------------------

/// `i64::MAX + 1` overflows. `llvm.sadd.with.overflow.i64` sets the
/// overflow flag, the branch takes the trap path, and the JIT calls
/// `hew_trap_with_code(201)`.
#[test]
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
fn integer_overflow_fires_supervisor_seam_code_201() {
    // 9223372036854775807 is i64::MAX; adding 1 overflows a signed i64.
    let ll = compile_to_ll(
        "fn main() -> i64 { 9223372036854775807 + 1 }",
        "jit_integer_overflow",
    );
    let result = jit_run_main(&ll);
    assert_eq!(
        result,
        Err(201),
        "i64::MAX + 1 must fire hew_trap_with_code(201); got: {result:?}"
    );
}

// ---------------------------------------------------------------------------
// DivideByZero — code 202
// ---------------------------------------------------------------------------

/// Dividing by a zero divisor fires the zero check. The emitted IR
/// checks `divisor == 0` and branches to a trap block that calls
/// `hew_trap_with_code(202)`.
#[test]
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
fn divide_by_zero_fires_supervisor_seam_code_202() {
    let ll = compile_to_ll(
        "fn main() -> i64 { let a: i64 = 10; let b: i64 = 0; a / b }",
        "jit_divide_by_zero",
    );
    let result = jit_run_main(&ll);
    assert_eq!(
        result,
        Err(202),
        "a / 0 must fire hew_trap_with_code(202); got: {result:?}"
    );
}

// ---------------------------------------------------------------------------
// ShiftOutOfRange — code 204
// ---------------------------------------------------------------------------

/// Shifting left by 64 (>= the 64-bit width) fires the range check.
/// The emitted IR checks `shift_amount >= 64` with `icmp uge` and
/// branches to a trap block that calls `hew_trap_with_code(204)`.
#[test]
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
fn shift_out_of_range_fires_supervisor_seam_code_204() {
    let ll = compile_to_ll(
        "fn main() -> i64 { let a: i64 = 1; let b: i64 = 64; a << b }",
        "jit_shift_out_of_range",
    );
    let result = jit_run_main(&ll);
    assert_eq!(
        result,
        Err(204),
        "a << 64 must fire hew_trap_with_code(204); got: {result:?}"
    );
}

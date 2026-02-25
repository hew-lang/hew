//! Stackful micro-coroutine infrastructure: mmap'd stack pool + context switch
//! primitives.
//!
//! This is the foundation layer only — not yet wired to the scheduler or actor
//! dispatch.

use std::cell::RefCell;

// ── Windows virtual memory API ──────────────────────────────────────────────

#[cfg(windows)]
#[link(name = "kernel32")]
unsafe extern "system" {
    fn VirtualAlloc(
        addr: *mut std::ffi::c_void,
        size: usize,
        alloc_type: u32,
        protect: u32,
    ) -> *mut std::ffi::c_void;
    fn VirtualProtect(
        addr: *mut std::ffi::c_void,
        size: usize,
        new_protect: u32,
        old_protect: *mut u32,
    ) -> i32;
    fn VirtualFree(addr: *mut std::ffi::c_void, size: usize, free_type: u32) -> i32;
}

#[cfg(windows)]
const MEM_COMMIT: u32 = 0x1000;
#[cfg(windows)]
const MEM_RESERVE: u32 = 0x2000;
#[cfg(windows)]
const MEM_RELEASE: u32 = 0x8000;
#[cfg(windows)]
const PAGE_READWRITE: u32 = 0x04;
#[cfg(windows)]
const PAGE_NOACCESS: u32 = 0x01;

// ── CoroStack ────────────────────────────────────────────────────────────────

/// Usable stack size (8 KiB).
const STACK_SIZE: usize = 8 * 1024;
/// Guard page size (4 KiB, mapped `PROT_NONE` at the bottom).
const GUARD_SIZE: usize = 4 * 1024;

/// A coroutine stack: 8 KiB usable + 4 KiB guard page (`PROT_NONE` at bottom).
pub struct CoroStack {
    /// Base of the allocation (guard page starts here).
    base: *mut u8,
    /// Total allocation size (guard + usable).
    alloc_size: usize,
}

// SAFETY: The stack memory is exclusively owned by the `CoroStack` and is not
// shared across threads. Sending ownership to another thread is safe because
// only one thread accesses the memory at a time.
unsafe impl Send for CoroStack {}

impl CoroStack {
    /// Allocate a new stack with a guard page at the bottom.
    ///
    /// On Unix, uses `mmap`/`mprotect`. On Windows, uses `VirtualAlloc`/`VirtualProtect`.
    #[must_use]
    pub fn new() -> Option<Self> {
        let alloc_size = GUARD_SIZE + STACK_SIZE;

        #[cfg(unix)]
        let base_ptr = {
            // SAFETY: We request an anonymous, private mapping with read/write
            // permissions. `MAP_ANONYMOUS` means no file backing, and `fd = -1`
            // with `offset = 0` is the standard incantation for anonymous maps.
            let base = unsafe {
                libc::mmap(
                    std::ptr::null_mut(),
                    alloc_size,
                    libc::PROT_READ | libc::PROT_WRITE,
                    libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                    -1,
                    0,
                )
            };

            if base == libc::MAP_FAILED {
                return None;
            }

            // SAFETY: `base` is a valid pointer returned by `mmap` and
            // `GUARD_SIZE` is within the allocation. We mark the bottom page as
            // inaccessible so that stack overflow triggers a segfault instead of
            // silent corruption.
            let ret = unsafe { libc::mprotect(base, GUARD_SIZE, libc::PROT_NONE) };
            if ret != 0 {
                // SAFETY: `base` / `alloc_size` match the preceding `mmap`.
                unsafe { libc::munmap(base, alloc_size) };
                return None;
            }

            base.cast::<u8>()
        };

        #[cfg(windows)]
        let base_ptr = {
            // SAFETY: VirtualAlloc with MEM_COMMIT | MEM_RESERVE allocates
            // and commits a new region of virtual memory.
            let base = unsafe {
                VirtualAlloc(
                    std::ptr::null_mut(),
                    alloc_size,
                    MEM_COMMIT | MEM_RESERVE,
                    PAGE_READWRITE,
                )
            };

            if base.is_null() {
                return None;
            }

            // Set the guard page at the bottom to PAGE_NOACCESS.
            let mut old_protect: u32 = 0;
            let ret = unsafe { VirtualProtect(base, GUARD_SIZE, PAGE_NOACCESS, &mut old_protect) };
            if ret == 0 {
                unsafe { VirtualFree(base, 0, MEM_RELEASE) };
                return None;
            }

            base.cast::<u8>()
        };

        Some(CoroStack {
            base: base_ptr,
            alloc_size,
        })
    }

    /// Top of usable stack (stacks grow downward on x86-64 / aarch64).
    #[must_use]
    pub fn top(&self) -> *mut u8 {
        // SAFETY: `base + alloc_size` is one byte past the allocation, which
        // is a valid address for pointer arithmetic (not dereferenced). The
        // stack pointer will be set to an aligned value below this.
        unsafe { self.base.add(self.alloc_size) }
    }
}

impl Drop for CoroStack {
    fn drop(&mut self) {
        #[cfg(unix)]
        {
            // SAFETY: `base` and `alloc_size` correspond to a live `mmap`
            // allocation that has not yet been unmapped.
            unsafe {
                libc::munmap(self.base.cast::<libc::c_void>(), self.alloc_size);
            }
        }
        #[cfg(windows)]
        {
            // SAFETY: `base` was allocated by VirtualAlloc with MEM_COMMIT | MEM_RESERVE.
            unsafe {
                VirtualFree(self.base.cast(), 0, MEM_RELEASE);
            }
        }
    }
}

impl std::fmt::Debug for CoroStack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CoroStack")
            .field("base", &self.base)
            .field("alloc_size", &self.alloc_size)
            .finish()
    }
}

// ── CoroContext ───────────────────────────────────────────────────────────────

/// Saved CPU registers for a coroutine context switch.
///
/// On x86-64 we save the callee-saved registers plus `rsp` and `rip`:
/// `rbx, rbp, r12, r13, r14, r15, rsp, rip` (8 × 8 bytes = 64 bytes).
#[repr(C)]
#[derive(Debug)]
pub struct CoroContext {
    regs: [u64; 8],
}

impl CoroContext {
    /// Create a zeroed context (invalid until initialised by [`coro_init`]).
    #[must_use]
    pub fn new() -> Self {
        CoroContext { regs: [0; 8] }
    }
}

impl Default for CoroContext {
    fn default() -> Self {
        Self::new()
    }
}

// ── Coroutine ────────────────────────────────────────────────────────────────

/// A stackful coroutine.
#[derive(Debug)]
#[allow(
    dead_code,
    reason = "foundation struct — fields used once wired to scheduler"
)]
pub struct Coroutine {
    /// Saved context (registers).
    context: CoroContext,
    /// The stack this coroutine runs on.
    stack: CoroStack,
    /// Whether this coroutine has finished executing.
    pub finished: bool,
}

// ── Stack pool ───────────────────────────────────────────────────────────────

/// Maximum number of stacks kept in the per-thread pool.
const MAX_POOL_SIZE: usize = 64;

thread_local! {
    /// Per-worker stack pool for recycling coroutine stacks.
    static STACK_POOL: RefCell<Vec<CoroStack>> = const { RefCell::new(Vec::new()) };
}

/// Get a stack from the thread-local pool, or allocate a new one via `mmap`.
pub fn acquire_stack() -> Option<CoroStack> {
    STACK_POOL
        .with(|pool| pool.borrow_mut().pop())
        .or_else(CoroStack::new)
}

/// Return a stack to the thread-local pool for reuse.
///
/// If the pool is already at capacity the stack is dropped (and `munmap`-ed).
pub fn release_stack(stack: CoroStack) {
    STACK_POOL.with(|pool| {
        let mut pool = pool.borrow_mut();
        if pool.len() < MAX_POOL_SIZE {
            pool.push(stack);
        }
        // else: drop it (munmap via Drop)
    });
}

// ── Context switch — x86-64 ─────────────────────────────────────────────────

/// Switch from the current coroutine context to the target.
///
/// Saves callee-saved registers + `rsp` + return address of `from`, then
/// restores the same set from `to` and jumps to its saved `rip`.
///
/// # Safety
///
/// * Both pointers must point to valid, aligned `CoroContext` values.
/// * The stack referenced by `to.regs[6]` (rsp) must be valid and live.
/// * After the switch, `from`'s state is frozen and `to` resumes where it
///   last called `coro_switch`.
#[cfg(target_arch = "x86_64")]
#[inline(never)]
pub unsafe fn coro_switch(from: *mut CoroContext, to: *const CoroContext) {
    // SAFETY: Caller guarantees both pointers are valid. The inline assembly
    // saves/restores exactly the callee-saved register set required by the
    // System V AMD64 ABI, plus rsp and a synthetic return address.
    unsafe {
        std::arch::asm!(
            // Save callee-saved registers into `from`
            "mov [{from} + 0*8], rbx",
            "mov [{from} + 1*8], rbp",
            "mov [{from} + 2*8], r12",
            "mov [{from} + 3*8], r13",
            "mov [{from} + 4*8], r14",
            "mov [{from} + 5*8], r15",
            "mov [{from} + 6*8], rsp",
            "lea rax, [rip + 2f]",       // return address
            "mov [{from} + 7*8], rax",
            // Restore callee-saved registers from `to`
            "mov rbx, [{to} + 0*8]",
            "mov rbp, [{to} + 1*8]",
            "mov r12, [{to} + 2*8]",
            "mov r13, [{to} + 3*8]",
            "mov r14, [{to} + 4*8]",
            "mov r15, [{to} + 5*8]",
            "mov rsp, [{to} + 6*8]",
            "jmp [{to} + 7*8]",
            "2:",
            from = in(reg) from,
            to = in(reg) to,
            // Clobber every register not explicitly saved above.
            out("rax") _,
            out("rcx") _,
            out("rdx") _,
            out("rsi") _,
            out("rdi") _,
            out("r8") _,
            out("r9") _,
            out("r10") _,
            out("r11") _,
            options(nostack),
        );
    }
}

/// Fallback for non-x86-64 targets (not yet implemented).
#[cfg(not(target_arch = "x86_64"))]
pub unsafe fn coro_switch(_from: *mut CoroContext, _to: *const CoroContext) {
    unimplemented!("coro_switch is only implemented for x86_64");
}

/// Initialise a [`CoroContext`] so that switching to it starts executing
/// `entry(arg)` on the given stack.
///
/// # Safety
///
/// * `ctx` must point to a valid, writable `CoroContext`.
/// * `stack_top` must point to the top of a valid, mapped stack region with
///   enough space for the initial frame (at least 16 bytes below `stack_top`).
/// * `entry` must be a valid function pointer that follows the C calling
///   convention.
#[cfg(target_arch = "x86_64")]
pub unsafe fn coro_init(
    ctx: *mut CoroContext,
    stack_top: *mut u8,
    entry: unsafe extern "C" fn(*mut u8),
    arg: *mut u8,
) {
    // SAFETY: Caller guarantees `stack_top` has enough space. We set up the
    // initial stack frame so that when `coro_switch` jumps to `rip` the
    // coroutine starts executing `entry`. The argument is passed in `rdi`
    // via the r12 register (restored by coro_switch, then moved to rdi by
    // the trampoline — but for now we store it at a known stack slot).
    unsafe {
        // 16-byte align the stack pointer (System V ABI requirement).
        #[allow(
            clippy::cast_ptr_alignment,
            reason = "stack_top is page-aligned from mmap"
        )]
        let sp = stack_top.sub(16).cast::<u64>();

        (*ctx).regs = [0; 8];
        (*ctx).regs[6] = sp as u64; // rsp
        (*ctx).regs[7] = entry as usize as u64; // rip — entry point

        // Store arg in r12 slot so it is available after coro_switch restores
        // callee-saved registers. A trampoline can move r12 → rdi before
        // calling the real entry function.
        (*ctx).regs[2] = arg as u64; // r12 = arg
    }
}

/// Fallback for non-x86-64 targets (not yet implemented).
#[cfg(not(target_arch = "x86_64"))]
pub unsafe fn coro_init(
    _ctx: *mut CoroContext,
    _stack_top: *mut u8,
    _entry: unsafe extern "C" fn(*mut u8),
    _arg: *mut u8,
) {
    unimplemented!("coro_init is only implemented for x86_64");
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stack_alloc_and_free() {
        let stack = CoroStack::new().expect("mmap failed");
        assert!(!stack.base.is_null());
        assert_eq!(stack.alloc_size, GUARD_SIZE + STACK_SIZE);
        let top = stack.top();
        assert_eq!(top as usize, stack.base as usize + stack.alloc_size);
        // Drop frees via munmap
    }

    #[test]
    fn stack_pool_recycle() {
        let stack1 = acquire_stack().unwrap();
        let base1 = stack1.base;
        release_stack(stack1);
        let stack2 = acquire_stack().unwrap();
        assert_eq!(stack2.base, base1); // Got the same stack back
    }
}

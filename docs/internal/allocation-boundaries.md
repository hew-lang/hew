# Allocation boundaries

One allocator owns every allocation Hew makes (D463). This file is the
documented list of the places that are allowed to use `libc::malloc`,
`libc::realloc` and `libc::free` instead, and why.

## The rule

Generated code and the C ABI allocate through the runtime:

- `hew_alloc` / `hew_realloc` / `hew_dealloc` (`hew-runtime/src/mem.rs`) when
  the size and alignment are known at both the allocation and the release site.
  This is the `mem.*` intrinsic floor the compiler's container lowering uses:
  it threads the matching `(size, align)` through every free edge.
- `buf_alloc` / `buf_try_alloc` / `buf_realloc` / `buf_free`
  (`hew-cabi/src/mem.rs`, re-exported by `hew-runtime/src/mem.rs`) when the
  release site holds only a pointer: a mailbox payload, a reply value, an actor
  state wrapper, a serialized frame, a collection header, a string. These
  record the payload size in a 16-byte header before the returned pointer, so
  the release site can rebuild the exact `Layout`.

Rust code allocates through `Box`, `Vec` or `alloc::alloc` with a `Layout`.

Both runtime families and all three Rust forms end at the same
`#[global_allocator]`, which is `ProfilingAllocator`
(`hew-runtime/src/profiler/allocator.rs`). That is what makes `std.observe`'s
heap counters complete by construction rather than by instrumentation.

## Why it is not a style preference

`libc::malloc` and the Rust global allocator are one heap on POSIX only by
coincidence. On Windows the Rust `System` allocator is `HeapAlloc` while libc
is the UCRT heap, so releasing a block through the other family corrupts the
heap. Over-aligned blocks differ on every target.

The size header is also the oracle. On Linux the Rust `System` allocator
forwards to `libc::malloc` for alignments up to 16, so a plain family swap that
got a pair wrong would be invisible under ASan and would surface only on
Windows. Because a sized block's returned pointer is not an allocation base, a
stray `libc::free` on it, or a header read on a libc block, is an invalid free
that ASan reports on Linux.

## The boundary list

`hew-runtime/src`, `hew-std/src` and `hew-cabi/src` contain no `libc::malloc`,
`libc::realloc` or `libc::free` call site. Every allocation in those crates is
managed: Hew allocates it and Hew releases it.

Two boundaries remain, and neither is an allocation the runtime makes.

| Where                                                                          | What                                                                                                                        | Who frees                                                 | Why libc                                                                                                                                                                                                                              |
| ------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `hew-codegen-rs/src/sanitizer.rs`, `allocation_access_module` (`#[cfg(test)]`) | The fixture declares `malloc` and `free` in a synthetic LLVM module and calls them around a deliberate out-of-bounds access | The same generated module, on the next instruction        | The fixture proves that ASan instruments a generated body around an externally implemented allocator. It is never linked into a Hew program, and using the runtime's allocator would test the runtime instead of the instrumentation. |
| A native package's own staticlib                                               | A package that allocates a buffer with its own allocator and hands the pointer to Hew                                       | The same package, through the release function it exports | We do not own both sides. The package's allocation and its release must pair with each other; the runtime never frees a pointer a package allocated, and a package must never free a pointer the runtime allocated.                   |

The second row is a contract, not a site in this repository. A native package
that wants the runtime's allocator can call `hew_alloc` / `hew_dealloc`, which
are exported in the C ABI surface (`hew-cabi/include/hew_cabi_surface.h`).

## Adding a site

Do not. If a new allocation cannot use one of the forms above, the boundary is
real and this table gains a row naming the path, the function, who frees and
why. A row without an owner on the other side is a bug, not a boundary.

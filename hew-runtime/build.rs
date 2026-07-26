fn main() {
    // The WASM link line resolves this crate's staticlib directly instead of
    // going through hew-lib's umbrella archive, so it needs its own stamp: a
    // wasm program otherwise links whatever libhew_runtime.a happens to be on
    // disk, with nothing tying it to the driver that produced the object.
    hew_build_identity::scan::emit_stamp("hew-runtime", hew_build_identity::STAMP_SYMBOL_RUNTIME);

    // Pure Rust runtime — no C compilation needed.
    // Build scripts run on the host, so use Cargo's target cfg env vars rather
    // than `#[cfg(unix)]`/`#[cfg(windows)]` to avoid leaking native link flags
    // into cross-target WASM test binaries.
    if std::env::var_os("CARGO_CFG_UNIX").is_some() {
        println!("cargo:rustc-link-lib=pthread");
        println!("cargo:rustc-link-lib=m");
    } else if std::env::var_os("CARGO_CFG_WINDOWS").is_some() {
        println!("cargo:rustc-link-lib=ws2_32");
        println!("cargo:rustc-link-lib=userenv");
    }

    // `hew_tsan`: active when the crate is built with ThreadSanitizer. TSan
    // cannot observe the futex-based std `Mutex`/`RwLock` happens-before when
    // the instrumented crate links against a non-instrumented `std` (the
    // `make tsan` lane uses `-Cunsafe-allow-abi-mismatch=sanitizer`), so every
    // lock-protected critical section is reported as a data race. When this
    // cfg is set, `lifetime::poison_safe` reconstructs the lock ordering as an
    // explicit atomic edge TSan *does* see. `sanitize` is an unstable rustc
    // cfg, but Cargo still exports the target's cfgs to build scripts as
    // `CARGO_CFG_SANITIZE`, so detection needs no nightly feature gate here.
    println!("cargo:rustc-check-cfg=cfg(hew_tsan)");
    if std::env::var("CARGO_CFG_SANITIZE").is_ok_and(|v| v.split(',').any(|s| s == "thread")) {
        println!("cargo:rustc-cfg=hew_tsan");
    }
}

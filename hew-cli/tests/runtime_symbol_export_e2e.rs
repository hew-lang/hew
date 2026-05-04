mod support;

use std::collections::HashSet;
use std::process::Command;

use support::hew_binary;

/// Every symbol in `kStableJitHostSymbols` must appear in the dynamic export
/// table of the `hew` binary.
///
/// This is the regression sentinel for the JIT inprocess symbol-export fix.
/// If a future change reintroduces DCE of runtime symbols (e.g. removing the
/// `#[used]` anchor, dropping the `hew-runtime` dep, or omitting
/// `-export_dynamic` from the linker flags), this test fails before the JIT
/// smoke tests do, providing a clearer diagnostic.
///
/// Implementation: run `nm` on the hew binary and check that every symbol
/// from the known stable set is present as a T-class (text section) export.
/// The test uses the same 653-symbol list that build.rs reads from
/// `generated_jit_stable_symbols.h` — hard-coded as a representative sample
/// here to avoid a build-script dependency.  The full count check (≥ 653)
/// catches any systematic regression without enumerating all names.
///
/// On platforms where `nm` is unavailable, the test is skipped rather than
/// failed.
#[test]
fn all_stable_jit_symbols_present_in_dynamic_export_table() {
    // Representative sample from kStableJitHostSymbols — enough to catch a
    // systematic regression without embedding all 653 names.  The full count
    // check (>= 653 T-class hew_ symbols) catches the rest.
    //
    // All names here are confirmed present in generated_jit_stable_symbols.h.
    const SENTINEL_SYMBOLS: &[&str] = &[
        "hew_print_value",
        "hew_string_drop",
        "hew_string_clone",
        "hew_actor_ask",
        "hew_actor_send",
        "hew_actor_close",
        "hew_actor_free",
        "hew_actor_await",
    ];

    let binary = hew_binary();

    // Determine platform-appropriate nm flags and symbol prefix.
    // macOS: `nm -gU <binary>` — prints only external (exported) symbols,
    //   filtering out undefined ones.  Symbol names include a leading `_`.
    // Linux: `nm -D <binary>` — prints dynamic symbol table.
    //   Symbol names have no leading underscore.
    #[cfg(target_os = "macos")]
    let (nm_args, symbol_prefix) = (vec!["-g", "-U"], "_");
    #[cfg(not(target_os = "macos"))]
    let (nm_args, symbol_prefix) = (vec!["-D"], "");

    let nm_result = Command::new("nm").args(&nm_args).arg(&binary).output();

    let output = match nm_result {
        Ok(o) => o,
        Err(error) => {
            eprintln!(
                "skipping all_stable_jit_symbols_present_in_dynamic_export_table: \
                 nm not available: {error}"
            );
            return;
        }
    };

    if !output.status.success() {
        eprintln!(
            "skipping all_stable_jit_symbols_present_in_dynamic_export_table: \
             nm exited with non-zero status on {}: {}",
            binary.display(),
            String::from_utf8_lossy(&output.stderr),
        );
        return;
    }

    let nm_stdout = String::from_utf8_lossy(&output.stdout);

    // Collect T-class (text section) exported hew_ symbols.
    let exported: HashSet<&str> = nm_stdout
        .lines()
        .filter_map(|line| {
            // nm output format: "<addr> T <name>" or "<addr> t <name>"
            // We want uppercase T (globally exported symbols).
            let mut parts = line.split_whitespace();
            let _ = parts.next(); // address
            let class = parts.next()?;
            let name = parts.next()?;
            if class == "T" && name.starts_with(symbol_prefix) && name.contains("hew_") {
                // Strip the leading underscore on macOS so we compare bare names.
                Some(name.trim_start_matches('_'))
            } else {
                None
            }
        })
        .collect();

    // Verify representative sentinel symbols are present.
    let mut missing = Vec::new();
    for sym in SENTINEL_SYMBOLS {
        if !exported.contains(*sym) {
            missing.push(*sym);
        }
    }

    assert!(
        missing.is_empty(),
        "the following kStableJitHostSymbols sentinel symbols were not found in \
         the dynamic export table of {}:\n  {}\n\n\
         This means the JIT inprocess mode will fail with 'Symbols not found' \
         at runtime.  Ensure:\n\
         1. hew-runtime is in hew-cli's [dependencies]\n\
         2. `extern crate hew_runtime as _` is present in main.rs\n\
         3. -Wl,-export_dynamic (macOS) or -Wl,--export-dynamic (Linux) is \
            emitted by build.rs\n\
         4. The #[used] _RUNTIME_SYMBOL_ANCHORS static is not removed from \
            the generated runtime_export.rs",
        binary.display(),
        missing.join(", "),
    );

    // Verify the total count of exported hew_ symbols meets the expected floor.
    // 653 is the stable symbol count from kStableJitHostSymbols; the actual
    // count may be higher (e.g. internal symbols also happen to be exported).
    // A count below 653 indicates a systematic DCE regression.
    let count = exported.len();
    assert!(
        count >= 653,
        "expected at least 653 exported hew_ T-class symbols in {} (one per \
         kStableJitHostSymbols entry), but found only {}.\n\
         Run: nm {} | grep \" T {}hew_\" | wc -l",
        binary.display(),
        count,
        binary.display(),
        symbol_prefix,
    );
}

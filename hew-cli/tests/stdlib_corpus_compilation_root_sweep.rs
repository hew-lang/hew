//! The binder-shape sweep's property, applied to the STANDARD LIBRARY itself.
//!
//! # Why a corpus instrument was needed
//!
//! Round 9's sweep ran over synthetic fixtures only, and every fixture it
//! generated was a root compilation unit. That is precisely why it could not
//! see this round's defect: the defect was that a stdlib file's verdict
//! DEPENDED on being a root compilation unit.
//!
//! `ExternProvenance` is captured at HIR lowering from the enclosing module
//! name, so the same `std/process.hew` carries `Module("std.process")` when it
//! is reached through `import std::process` and `Root` when it is handed
//! directly to `hew check`. `ProvenForeignPolicy` read `Root` as "foreign
//! host", so the compiler classified its own runtime ABI as foreign and refused
//! shipped code — but only on the path the stdlib type-check ratchet uses, and
//! only for files whose error carrier crosses a callee-owned parameter.
//!
//! # The property
//!
//! > How a file is handed to the compiler is not a property of the values in
//! > it, so it must not change the verdict.
//!
//! This is the same metamorphic discipline as the release-count sweep, with
//! acceptance as the observable and the compilation root as the axis — and,
//! unlike the synthetic planes, the programs are the ones actually shipped.
//!
//! Both directions are pinned:
//!
//! * every stdlib module checks clean AS A ROOT UNIT (what the ratchet does),
//!   and
//! * checks clean THROUGH AN IMPORT (what every user program does),
//!
//! and neither may die by a signal, because a compiler that aborts on a
//! diagnosable condition gives the user nothing at all.

mod support;

use std::collections::BTreeMap;
use std::process::Command;
use support::{hew_binary, repo_root};

/// One file's verdict under one compilation root.
#[derive(Debug, PartialEq, Eq)]
struct Verdict {
    ok: bool,
    /// `Some(signal)` when the compiler died by a signal rather than exiting.
    /// Any value here is a defect on its own, whatever the exit status would
    /// have been.
    killed_by: Option<i32>,
    detail: String,
}

fn check(args: &[&std::path::Path]) -> Verdict {
    let output = Command::new(hew_binary())
        .arg("check")
        .args(args)
        .current_dir(repo_root())
        .output()
        .expect("run hew check");
    #[cfg(unix)]
    let killed_by = {
        use std::os::unix::process::ExitStatusExt;
        output.status.signal()
    };
    #[cfg(not(unix))]
    let killed_by = None;
    let stderr = String::from_utf8_lossy(&output.stderr);
    Verdict {
        ok: output.status.success(),
        killed_by,
        detail: stderr
            .lines()
            .find(|line| line.contains("error"))
            .unwrap_or("")
            .to_string(),
    }
}

fn compile_to_mir(file: &std::path::Path) -> Verdict {
    let output = Command::new(hew_binary())
        .arg("compile")
        .arg(file)
        .args(["--dump-mir", "elab"])
        .current_dir(repo_root())
        .output()
        .expect("run hew compile --dump-mir elab");
    #[cfg(unix)]
    let killed_by = {
        use std::os::unix::process::ExitStatusExt;
        output.status.signal()
    };
    #[cfg(not(unix))]
    let killed_by = None;
    Verdict {
        ok: output.status.success(),
        killed_by,
        detail: String::from_utf8_lossy(&output.stderr).into_owned(),
    }
}

/// Every `.hew` file under `std/`, which is exactly the set
/// `scripts/stdlib-ratchet.sh` walks.
fn corpus() -> Vec<std::path::PathBuf> {
    fn walk(dir: &std::path::Path, out: &mut Vec<std::path::PathBuf>) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                walk(&path, out);
            } else if path.extension().is_some_and(|ext| ext == "hew") {
                out.push(path);
            }
        }
    }
    let mut out = Vec::new();
    walk(&repo_root().join("std"), &mut out);
    out.sort();
    assert!(
        out.len() > 40,
        "the stdlib corpus must be discovered, not silently empty — found {}",
        out.len()
    );
    out
}

/// `std/net/tls.hew` → `std::net::tls`.
fn module_path(file: &std::path::Path) -> Option<String> {
    let rel = file.strip_prefix(repo_root()).ok()?;
    let stem = rel.with_extension("");
    let mut parts: Vec<String> = stem
        .components()
        .map(|c| c.as_os_str().to_string_lossy().to_string())
        .collect();
    // A directory module is named by its directory, not by the file that
    // happens to sit inside it (`std/time/cron/cron.hew` is `std::time::cron`).
    if parts.len() >= 2 && parts[parts.len() - 1] == parts[parts.len() - 2] {
        parts.pop();
    }
    Some(parts.join("::"))
}

#[test]
fn every_stdlib_file_checks_clean_as_a_root_compilation_unit() {
    let mut failures: BTreeMap<String, String> = BTreeMap::new();
    for file in corpus() {
        let verdict = check(&[&file]);
        if let Some(signal) = verdict.killed_by {
            failures.insert(
                file.display().to_string(),
                format!("KILLED BY SIGNAL {signal} — a diagnosable condition must not abort"),
            );
        } else if !verdict.ok {
            failures.insert(file.display().to_string(), verdict.detail);
        }
    }
    assert!(
        failures.is_empty(),
        "`hew check` must neither reject nor abort on the standard library it ships:\n{}",
        failures
            .iter()
            .map(|(f, d)| format!("  {f}: {d}"))
            .collect::<Vec<_>>()
            .join("\n")
    );
}

#[test]
fn prelude_compiles_to_mir_cleanly_as_a_root_compilation_unit() {
    let prelude = repo_root().join("std/prelude.hew");
    let verdict = compile_to_mir(&prelude);
    assert_eq!(
        verdict.killed_by, None,
        "prelude root compilation must diagnose rather than abort: {}",
        verdict.detail
    );
    assert!(
        verdict.ok,
        "prelude must compile through HIR and MIR as a root unit: {}",
        verdict.detail
    );
}

/// The axis this round's defect lived on, over the real corpus.
///
/// A driver that merely imports a module is enough: the import pulls the module
/// through HIR lowering with `ExternProvenance::Module(..)` instead of `Root`,
/// which is the single input that differed.
/// Modules that are not user-importable AT ALL, so the axis does not apply.
///
/// `std::builtins` declares inherent `impl` blocks on the builtin nominal types
/// (`Vec`, `HashMap`, `Option`, `Result`). That shape is reserved to the
/// standard library, and a driver written outside `std/` is user code by
/// definition, so the import is refused by an `E_HIR` rule about WHO may
/// declare an impl — nothing to do with ownership or provenance. The root-unit
/// case above still covers this file.
const NOT_USER_IMPORTABLE: &[&str] = &["std::builtins"];

#[test]
fn a_stdlib_module_checks_the_same_through_an_import_as_it_does_as_a_root_unit() {
    let dir = std::env::temp_dir().join(format!("hew-stdlib-root-axis-{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("driver dir");

    let mut failures: Vec<String> = Vec::new();
    for file in corpus() {
        let Some(module) = module_path(&file) else {
            continue;
        };
        if !module.starts_with("std::") || NOT_USER_IMPORTABLE.contains(&module.as_str()) {
            continue;
        }
        let driver = dir.join(format!("{}.hew", module.replace("::", "_")));
        std::fs::write(
            &driver,
            format!("import {module};\n\nfn main() -> i64 {{\n    0\n}}\n"),
        )
        .expect("write driver");

        let as_root = check(&[&file]);
        let as_import = check(&[&driver]);
        if let Some(signal) = as_import.killed_by {
            failures.push(format!(
                "  {module}: import driver KILLED BY SIGNAL {signal}"
            ));
        } else if as_root.ok != as_import.ok {
            failures.push(format!(
                "  {module}: root={} import={} ({} / {})",
                if as_root.ok { "OK" } else { "REJECTED" },
                if as_import.ok { "OK" } else { "REJECTED" },
                as_root.detail,
                as_import.detail
            ));
        }
    }

    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        failures.is_empty(),
        "how a file is handed to the compiler is not a property of the values in it, so it \
         must not change the verdict. A module that is accepted one way and rejected the \
         other is the provenance seam that refused `std/process.hew` and \
         `std/time/cron/cron.hew`:\n{}",
        failures.join("\n")
    );
}

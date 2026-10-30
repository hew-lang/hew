//! Diagnostics must not depend on hash-table order.
//!
//! Every hash map in the compiler gets a fresh seed, so checking the same
//! file twice in one process exposes any producer that reports in iteration
//! order.

use std::path::{Path, PathBuf};

use hew_compile::{
    run_file_frontend_to_typecheck, DiagnosticPolicy, FrontendDiagnostic, FrontendDiagnosticKind,
    FrontendOptions, Session, SessionTarget, COMPILER_STACK_SIZE,
};

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root")
        .to_path_buf()
}

/// Inference-variable numbers come from a process-wide counter, so a second
/// check in the same process numbers them differently even when allocation is
/// deterministic. `fresh_typevar_determinism` covers the allocation itself.
fn without_variable_numbers(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut rest = text;
    while let Some(index) = rest.find("?T") {
        out.push_str(&rest[..index + 2]);
        rest = rest[index + 2..].trim_start_matches(|ch: char| ch.is_ascii_digit());
    }
    out.push_str(rest);
    out
}

fn describe(diagnostics: &[FrontendDiagnostic]) -> String {
    diagnostics
        .iter()
        .map(|diagnostic| match &diagnostic.kind {
            FrontendDiagnosticKind::Message(message) => {
                format!("{} {} {:?}\n", message.code, message.message, message.span)
            }
            FrontendDiagnosticKind::Parse(error) => format!("{error:?}\n"),
            FrontendDiagnosticKind::Type(error) => format!(
                "{:?} {:?} {} {:?} {:?}\n",
                error.kind, error.span, error.message, error.notes, error.suggestions
            ),
            FrontendDiagnosticKind::Hir(diagnostic) => format!(
                "{:?} {:?} {}\n",
                diagnostic.kind, diagnostic.span, diagnostic.note
            ),
        })
        .collect()
}

/// The diagnostics the frontend and the shared semantic lowering report.
fn frontend_report(path: &Path) -> String {
    let report = match run_file_frontend_to_typecheck(
        path.to_str().expect("UTF-8 path"),
        &FrontendOptions::default(),
    ) {
        Err(failure) => format!("{}\n{}", failure.message, describe(&failure.diagnostics)),
        Ok(state) => {
            let mut report = describe(&state.diagnostics);
            if let Some(output) = state.typecheck_result.tco.as_ref() {
                if let Err(error) =
                    Session::new(SessionTarget::native(), DiagnosticPolicy::default())
                        .lower_program(&state.program, output)
                {
                    report.push_str(&error.to_string());
                }
            }
            report
        }
    };
    without_variable_numbers(&report)
}

fn hew_sources(directory: &Path) -> Vec<PathBuf> {
    let mut sources: Vec<PathBuf> = std::fs::read_dir(directory)
        .expect("source directory")
        .map(|entry| entry.expect("directory entry").path())
        .filter(|path| path.extension().is_some_and(|extension| extension == "hew"))
        .collect();
    sources.sort();
    sources
}

/// The refused vertical-slice programs, which carry the diagnostics, and
/// fixtures with many reports from each order-sensitive producer. Checking
/// every accepted program twice as well costs about half an hour of
/// debug-build CPU time and adds few reports.
fn checked_sources() -> Vec<PathBuf> {
    let mut sources = hew_sources(&repo_root().join("tests/vertical-slice/reject"));
    sources.extend(hew_sources(
        &Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures"),
    ));
    sources
}

#[test]
fn reports_are_identical_across_checks() {
    let sources = checked_sources();
    assert!(!sources.is_empty(), "vertical-slice sources");
    let workers = std::thread::available_parallelism().map_or(4, usize::from);
    let chunk = sources.len().div_ceil(workers);
    let mismatches: Vec<String> = std::thread::scope(|scope| {
        let handles: Vec<_> = sources
            .chunks(chunk)
            .map(|paths| {
                std::thread::Builder::new()
                    .stack_size(COMPILER_STACK_SIZE)
                    .spawn_scoped(scope, move || {
                        paths
                            .iter()
                            .filter(|path| frontend_report(path) != frontend_report(path))
                            .map(|path| path.display().to_string())
                            .collect::<Vec<_>>()
                    })
                    .expect("spawn checker thread")
            })
            .collect();
        handles
            .into_iter()
            .flat_map(|handle| handle.join().expect("worker"))
            .collect()
    });
    assert!(
        mismatches.is_empty(),
        "reports changed between two checks: {mismatches:#?}"
    );
}

#[test]
fn moved_handle_help_names_the_inferred_element() {
    let path =
        repo_root().join("tests/vertical-slice/reject/channel_for_loop_consumes_receiver.hew");
    let report = frontend_report(&path);
    assert!(
        report.contains("`Stream<string>` is a substrate handle"),
        "{report}"
    );
}

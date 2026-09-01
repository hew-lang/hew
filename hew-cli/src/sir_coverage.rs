//! `hew tool sir-coverage` — the dev-only SIR admission inventory.
//!
//! Until the legacy HIR→MIR body lowerer is deleted, every function in the
//! corpus is either taken by SIR or still routed through the legacy lowerer.
//! This tool asks that question of every root item in every `.hew` file it is
//! given and prints one line per item:
//!
//! ```text
//! <file> <qualified item> sir
//! <file> <qualified item> legacy: <reason>
//! sir-coverage: <admitted>/<total> functions (<pct>)
//! ```
//!
//! "Admitted" means the SIR lowering took the body under every-callable
//! demand and the SIR verifier accepted it: the same two facts the strict
//! `--sir-lower` route requires before it will build a component. A
//! non-function item (actor, machine, record, supervisor, impl block, extern
//! fn, type declaration, const) has no SIR route at all today and is counted
//! as `legacy: item-kind:<kind>` so the inventory never omits an item the
//! cutover still has to take.
//!
//! `--ratchet FILE` compares the percentage with the committed one and fails
//! when it dropped, so the number can only rise until it reaches 100 %.

use std::collections::BTreeSet;
use std::fmt::Write as _;
use std::path::{Path, PathBuf};

use serde::Serialize;

use crate::args::SirCoverageArgs;
use crate::{compile, target};

/// Number of decimals the ratchet file carries. Four decimals resolve one
/// function in a corpus of up to a million, so a one-function drop can never
/// round to equality.
const RATCHET_DECIMALS: usize = 4;

#[derive(Debug, Serialize)]
struct ItemReport {
    name: String,
    status: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    reason: Option<String>,
}

#[derive(Debug, Serialize)]
struct FileReport {
    file: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<&'static str>,
    items: Vec<ItemReport>,
}

#[derive(Debug, Serialize)]
struct CoverageReport {
    files: Vec<FileReport>,
    admitted: usize,
    total: usize,
    percent: f64,
    files_failed: usize,
}

pub fn cmd_sir_coverage(args: &SirCoverageArgs) -> i32 {
    let files = match collect_hew_files(&args.paths) {
        Ok(files) => files,
        Err(error) => {
            eprintln!("Error: {error}");
            return 2;
        }
    };
    if files.is_empty() {
        eprintln!("Error: sir-coverage: no .hew files under the given paths");
        return 2;
    }
    let target = match target::TargetSpec::from_requested(None) {
        Ok(target) => target,
        Err(error) => {
            eprintln!("Error: {error}");
            return 2;
        }
    };
    let options = compile::CompileOptions::default();

    let mut report = CoverageReport {
        files: Vec::with_capacity(files.len()),
        admitted: 0,
        total: 0,
        percent: 0.0,
        files_failed: 0,
    };
    for file in &files {
        let file_report = inventory_file(file, &target, &options);
        if file_report.error.is_some() {
            report.files_failed += 1;
        }
        for item in &file_report.items {
            report.total += 1;
            if item.status == "sir" {
                report.admitted += 1;
            }
        }
        report.files.push(file_report);
    }
    if report.total == 0 {
        eprintln!(
            "Error: sir-coverage: no items were inventoried across {} file(s); an empty corpus proves nothing",
            files.len()
        );
        return 2;
    }
    report.percent = percent(report.admitted, report.total);

    if args.json {
        match serde_json::to_string_pretty(&report) {
            Ok(json) => println!("{json}"),
            Err(error) => {
                eprintln!("Error: sir-coverage: cannot serialise report: {error}");
                return 2;
            }
        }
    } else {
        print!("{}", render_text(&report));
    }
    if report.files_failed > 0 {
        eprintln!(
            "sir-coverage: {} file(s) failed the frontend and contribute no items",
            report.files_failed
        );
    }

    match args.ratchet.as_deref() {
        Some(ratchet) => check_ratchet(ratchet, report.percent),
        None => 0,
    }
}

fn percent(admitted: usize, total: usize) -> f64 {
    // The u32 domain covers any corpus this tool will ever inventory, so the
    // conversion is exact where it matters.
    let admitted = f64::from(u32::try_from(admitted).unwrap_or(u32::MAX));
    let total = f64::from(u32::try_from(total).unwrap_or(u32::MAX));
    admitted * 100.0 / total
}

fn format_ratchet(percent: f64) -> String {
    format!("{percent:.RATCHET_DECIMALS$}")
}

fn render_text(report: &CoverageReport) -> String {
    let mut out = String::new();
    for file in &report.files {
        if let Some(error) = file.error {
            writeln!(out, "{} - {error}", file.file).expect("write to String");
        }
        for item in &file.items {
            match &item.reason {
                Some(reason) => {
                    writeln!(out, "{} {} {}: {reason}", file.file, item.name, item.status)
                }
                None => writeln!(out, "{} {} {}", file.file, item.name, item.status),
            }
            .expect("write to String");
        }
    }
    writeln!(
        out,
        "sir-coverage: {}/{} functions ({:.2}%)",
        report.admitted, report.total, report.percent
    )
    .expect("write to String");
    out
}

/// Compare the computed percentage with the committed one.
///
/// A drop fails. A rise is reported so the file can be raised; it fails only
/// under `RATCHET_STRICT_RECOVERIES=1`, the same accounting mode the corpus
/// ratchets use for unrecorded recoveries.
fn check_ratchet(path: &Path, computed: f64) -> i32 {
    let recorded = match std::fs::read_to_string(path) {
        Ok(text) => text,
        Err(error) => {
            eprintln!(
                "Error: sir-coverage: cannot read ratchet `{}`: {error}",
                path.display()
            );
            return 2;
        }
    };
    let Ok(recorded) = recorded.trim().parse::<f64>() else {
        eprintln!(
            "Error: sir-coverage: ratchet `{}` must hold one decimal percentage, got `{}`",
            path.display(),
            recorded.trim()
        );
        return 2;
    };
    // Both sides go through the same fixed-decimal rendering so equality is
    // a property of the recorded digits, never of float noise.
    let computed_text = format_ratchet(computed);
    let recorded_text = format_ratchet(recorded);
    let computed: f64 = computed_text.parse().expect("fixed-decimal render parses");
    let recorded: f64 = recorded_text.parse().expect("fixed-decimal render parses");
    if computed < recorded {
        eprintln!(
            "sir-coverage: ratchet dropped: `{}` records {recorded_text}%, this run measured {computed_text}%",
            path.display()
        );
        return 1;
    }
    if computed > recorded {
        eprintln!(
            "sir-coverage: ratchet can rise: `{}` records {recorded_text}%, this run measured {computed_text}%; update the file to {computed_text}",
            path.display()
        );
        if std::env::var("RATCHET_STRICT_RECOVERIES").as_deref() == Ok("1") {
            return 1;
        }
        return 0;
    }
    println!(
        "sir-coverage: ratchet holds at {recorded_text}% (`{}`)",
        path.display()
    );
    0
}

/// Every `.hew` file under the given paths, sorted and de-duplicated, so the
/// inventory is a function of the corpus and not of directory order.
fn collect_hew_files(paths: &[PathBuf]) -> Result<Vec<PathBuf>, String> {
    let mut files = BTreeSet::new();
    for path in paths {
        if path.is_dir() {
            collect_hew_directory(path, &mut files)?;
        } else if path.is_file() {
            files.insert(path.clone());
        } else {
            return Err(format!(
                "sir-coverage: `{}` is neither a file nor a directory",
                path.display()
            ));
        }
    }
    Ok(files.into_iter().collect())
}

fn collect_hew_directory(dir: &Path, files: &mut BTreeSet<PathBuf>) -> Result<(), String> {
    let entries = std::fs::read_dir(dir)
        .map_err(|error| format!("sir-coverage: cannot read `{}`: {error}", dir.display()))?;
    for entry in entries {
        let entry = entry.map_err(|error| {
            format!(
                "sir-coverage: cannot read an entry of `{}`: {error}",
                dir.display()
            )
        })?;
        let path = entry.path();
        let file_type = entry.file_type().map_err(|error| {
            format!("sir-coverage: cannot inspect `{}`: {error}", path.display())
        })?;
        if file_type.is_dir() {
            collect_hew_directory(&path, files)?;
        } else if file_type.is_file() && path.extension().is_some_and(|ext| ext == "hew") {
            files.insert(path);
        }
    }
    Ok(())
}

fn inventory_file(
    file: &Path,
    target: &target::TargetSpec,
    options: &compile::CompileOptions,
) -> FileReport {
    let display = file.display().to_string();
    let Ok(verified) = crate::lower_file_to_verified_hir(file, target, options) else {
        return FileReport {
            file: display,
            error: Some("frontend-failed"),
            items: Vec::new(),
        };
    };
    let module = &verified.lower_output.module;
    let sir = hew_sir::lower_module_with_demand(module, hew_sir::SirLoweringDemand::EveryCallable);
    let findings = VerifierFindings::new(&sir, hew_sir::verify_module(&sir.module));

    let mut items = Vec::new();
    for item in &module.items {
        let Some(report) = inventory_item(module, &sir, &findings, item) else {
            continue;
        };
        items.push(report);
    }
    FileReport {
        file: display,
        error: None,
        items,
    }
}

/// SIR verifier diagnostics keyed by the declaration they are about.
///
/// A diagnostic names the emitted symbol, and the callable table is the one
/// owner of the symbol → declaration join; a diagnostic that names no
/// callable is a module-level fact that applies to every lowered body.
struct VerifierFindings {
    by_declaration: Vec<(hew_types::DefId, String)>,
    module_level: Vec<String>,
}

impl VerifierFindings {
    fn new(sir: &hew_sir::LoweredModule, diagnostics: Vec<hew_sir::SirDiagnostic>) -> Self {
        let mut by_declaration = Vec::new();
        let mut module_level = Vec::new();
        for diagnostic in diagnostics {
            let owner = sir
                .module
                .callables
                .iter()
                .find(|callable| callable.symbol == diagnostic.function);
            let text = format!("sir-verifier:{:?}", diagnostic.kind);
            match owner {
                Some(callable) => by_declaration.push((callable.declaration.clone(), text)),
                None => module_level.push(text),
            }
        }
        Self {
            by_declaration,
            module_level,
        }
    }

    fn rejection(&self, declaration: &hew_types::DefId) -> Option<String> {
        self.by_declaration
            .iter()
            .find(|(candidate, _)| candidate == declaration)
            .map(|(_, text)| text.clone())
            .or_else(|| self.module_level.first().cloned())
    }
}

fn inventory_item(
    module: &hew_hir::HirModule,
    sir: &hew_sir::LoweredModule,
    verifier: &VerifierFindings,
    item: &hew_hir::HirItem,
) -> Option<ItemReport> {
    use hew_hir::HirItem;
    let (id, name, kind) = match item {
        HirItem::Function(function) => {
            // Functions carry a positive root record; imported module bodies
            // are inventoried when their own file is, never through an
            // importer.
            if !module.root_item_ids.contains(&function.id) {
                return None;
            }
            let (status, reason) = classify_function(sir, verifier, &function.declaration);
            return Some(ItemReport {
                name: function.declaration.full_path().to_string(),
                status,
                reason,
            });
        }
        HirItem::TypeDecl(decl) => (decl.id, decl.declaration.full_path().to_string(), "type"),
        HirItem::Machine(decl) => (decl.id, decl.declaration.full_path().to_string(), "machine"),
        HirItem::Record(decl) => (decl.id, decl.name.clone(), "record"),
        HirItem::Actor(decl) => (decl.id, decl.name.clone(), "actor"),
        HirItem::Supervisor(decl) => (decl.id, decl.name.clone(), "supervisor"),
        HirItem::Impl(block) => (
            block.id,
            match &block.trait_name {
                Some(trait_name) => format!("impl {trait_name} for {}", block.self_type_name),
                None => format!("impl {}", block.self_type_name),
            },
            "impl",
        ),
        HirItem::ExternFn(decl) => (
            decl.id,
            decl.declaration.full_path().to_string(),
            "extern-fn",
        ),
        HirItem::Const(decl) => (decl.id, decl.name.clone(), "const"),
    };
    // Non-function items have no positive root record; the source-module
    // attribution map names every imported one.
    if module.diagnostic_source_modules.contains_key(&id) {
        return None;
    }
    Some(ItemReport {
        name,
        status: "legacy",
        reason: Some(format!("item-kind:{kind}")),
    })
}

fn classify_function(
    sir: &hew_sir::LoweredModule,
    verifier: &VerifierFindings,
    declaration: &hew_types::DefId,
) -> (&'static str, Option<String>) {
    use hew_sir::SirLoweringStatus;
    let Some(status) = sir.status_for_declaration(declaration) else {
        return (
            "legacy",
            Some("no lowering status was recorded for this declaration".to_string()),
        );
    };
    match status {
        SirLoweringStatus::Lowered => match verifier.rejection(declaration) {
            Some(reason) => ("legacy", Some(reason)),
            None => ("sir", None),
        },
        SirLoweringStatus::Unsupported { reason } => ("legacy", Some(reason.clone())),
        SirLoweringStatus::GenericTemplate { instances: 0, .. } => (
            "legacy",
            Some("generic-template:no-instance-demanded".to_string()),
        ),
        SirLoweringStatus::GenericTemplate {
            failed_instances, ..
        } if *failed_instances > 0 => {
            let reason = sir
                .callable_statuses
                .iter()
                .find_map(|(callable, status)| {
                    let header = sir.module.callable(*callable)?;
                    if header.declaration != *declaration {
                        return None;
                    }
                    match status {
                        SirLoweringStatus::Unsupported { reason } => Some(reason.clone()),
                        _ => None,
                    }
                })
                .unwrap_or_else(|| "generic-template:instance-failed".to_string());
            ("legacy", Some(reason))
        }
        SirLoweringStatus::GenericTemplate { .. } => {
            // Every demanded instance lowered; the template body is proven
            // under those substitutions. Instances are verified as callables
            // of their own, so the verifier join above still applies.
            match verifier.rejection(declaration) {
                Some(reason) => ("legacy", Some(reason)),
                None => ("sir", None),
            }
        }
        // Every-callable demand asks for every header, so an unreached
        // status here is a demand bug, and a bug is not admission.
        SirLoweringStatus::NotReached => ("legacy", Some("not-reached".to_string())),
    }
}

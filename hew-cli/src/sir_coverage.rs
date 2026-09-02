//! `hew tool sir-coverage` — the dev-only SIR admission inventory.
//!
//! Until the legacy HIR→MIR body lowerer is deleted, every function body in
//! the corpus is either taken by SIR or still routed through the legacy
//! lowerer. This tool asks that question of every body the cutover must take
//! in every `.hew` file it is given and prints one line per row:
//!
//! ```text
//! <file> <qualified item> sir
//! <file> <qualified item> legacy: <reason>
//! sir-coverage: <admitted>/<total> functions (<pct>)
//! ```
//!
//! "Admitted" means the SIR lowering took the body under every-callable
//! demand and the SIR verifier accepted it: the same two facts the strict
//! `--sir-lower` route requires before it will build a component. Only
//! function bodies enter `<admitted>/<total>` — free functions, impl methods
//! (explicit and materialized trait defaults), and actor/machine handler
//! bodies, none of which SIR can route around today. A bodiless declaration
//! (record, type, const, extern fn, supervisor) or an impl-block header has
//! no SIR route to reach 100% under and is printed as an uncounted
//! `legacy: item-kind:<kind>` inventory line so the corpus is still fully
//! listed.
//!
//! `--ratchet FILE` compares the admitted body count with the committed one
//! and fails when it dropped, so the count can only rise.

use std::collections::{BTreeSet, HashSet};
use std::fmt::Write as _;
use std::path::{Path, PathBuf};

use hew_hir::ItemId;
use serde::Serialize;

use crate::args::SirCoverageArgs;
use crate::{compile, target};

#[derive(Debug, Serialize)]
struct ItemReport {
    name: String,
    status: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    reason: Option<String>,
    /// Whether this row is a function body counted toward `admitted`/`total`.
    /// `false` marks an inventory-only line (a bodiless declaration or an
    /// impl-block header) that has no SIR route to reach 100% under.
    counted: bool,
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
            if !item.counted {
                continue;
            }
            report.total += 1;
            if item.status == "sir" {
                report.admitted += 1;
            }
        }
        report.files.push(file_report);
    }
    if report.total == 0 {
        eprintln!(
            "Error: sir-coverage: no function bodies were inventoried across {} file(s); an empty corpus proves nothing",
            files.len()
        );
        return 2;
    }
    report.percent = percent(report.admitted, report.total);

    if args.json {
        match serde_json::to_string_pretty(&report) {
            Ok(json) => println!("{json}"),
            Err(error) => {
                eprintln!("Error: sir-coverage: cannot serialize report: {error}");
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
        Some(ratchet) => check_ratchet(ratchet, report.admitted),
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

/// Compare the computed admitted-body count with the committed one.
///
/// A drop fails. A rise is reported so the file can be raised; it fails only
/// under `RATCHET_STRICT_RECOVERIES=1`, the same accounting mode the corpus
/// ratchets use for unrecorded recoveries.
///
/// The ratchet is the raw admitted-function count, not a percentage: a
/// percentage's denominator moves every time the corpus gains or loses a
/// legacy fixture, so corpus growth alone can lower it with no compiler
/// regression at all. A count can only rise when a body that used to fail
/// admission starts passing, or fall when one that used to pass regresses —
/// exactly what a ratchet is for.
fn check_ratchet(path: &Path, computed: usize) -> i32 {
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
    let Ok(recorded) = recorded.trim().parse::<usize>() else {
        eprintln!(
            "Error: sir-coverage: ratchet `{}` must hold one non-negative integer, got `{}`",
            path.display(),
            recorded.trim()
        );
        return 2;
    };
    if computed < recorded {
        eprintln!(
            "sir-coverage: ratchet dropped: `{}` records {recorded}, this run measured {computed}",
            path.display()
        );
        return 1;
    }
    if computed > recorded {
        eprintln!(
            "sir-coverage: ratchet can rise: `{}` records {recorded}, this run measured {computed}; update the file to {computed}",
            path.display()
        );
        if std::env::var("RATCHET_STRICT_RECOVERIES").as_deref() == Ok("1") {
            return 1;
        }
        return 0;
    }
    // Stderr, like the other verdicts, so `--json` stdout stays one document.
    eprintln!(
        "sir-coverage: ratchet holds at {recorded} (`{}`)",
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

    // Positive record of which materialized trait-default method bodies
    // belong to a root-local impl block. Explicit impl methods are already
    // recorded into `root_item_ids` at lowering time (hew-hir/src/lower.rs);
    // a default method is deliberately excluded from that set because its
    // body's span indexes the trait's own source, not the root file — a fact
    // about fail-closed caret rendering that has nothing to do with whether
    // this compilation owns the body. `HirImplBlock::method_item_ids` lists
    // every method body (explicit and default) an impl block emits, so an
    // impl block with at least one explicit root method is proven root-local
    // and every id in its `method_item_ids` belongs to this compilation.
    //
    // WHY this is a shortcut: an impl block whose methods are ALL trait
    // defaults (no explicit override) has no `method_item_ids` entry in
    // `root_item_ids` to key off, so it is conservatively treated as
    // imported and its default bodies are skipped here (undercounting, never
    // overcounting).
    // WHEN obsolete: `HirImplBlock` gains its own `defining_module` field
    // (the same positive-record shape already carried by
    // `HirActorDecl`/`HirMachineDecl`/`HirRecordDecl`/`HirTypeDecl`).
    // WHAT the real fix looks like: thread that field through the impl's one
    // construction site (hew-hir/src/lower.rs) and read it directly here.
    let root_local_impl_bodies: HashSet<ItemId> = module
        .items
        .iter()
        .filter_map(|item| match item {
            hew_hir::HirItem::Impl(block) if impl_is_root(block, &module.root_item_ids) => {
                Some(block.method_item_ids.iter().copied())
            }
            _ => None,
        })
        .flatten()
        .collect();

    let mut items = Vec::new();
    for item in &module.items {
        inventory_item(
            module,
            &sir,
            &findings,
            item,
            &root_local_impl_bodies,
            &mut items,
        );
    }
    FileReport {
        file: display,
        error: None,
        items,
    }
}

/// Whether an impl block was lowered from the file being inventoried, proven
/// by at least one of its emitted method bodies appearing in the module's
/// positive `root_item_ids` record. See the shortcut note in `inventory_file`
/// for the one case (an impl with no explicit method override) this cannot
/// prove.
fn impl_is_root(block: &hew_hir::node::HirImplBlock, root_item_ids: &HashSet<ItemId>) -> bool {
    block
        .method_item_ids
        .iter()
        .any(|id| root_item_ids.contains(id))
}

/// SIR verifier diagnostics keyed by the declaration they are about.
///
/// A diagnostic carries the `CallableId` it is about, and the callable table
/// owns the callable → declaration join; a diagnostic about no callable is a
/// module-level fact that applies to every lowered body.
struct VerifierFindings {
    by_declaration: Vec<(hew_types::DefId, String)>,
    module_level: Vec<String>,
}

impl VerifierFindings {
    fn new(sir: &hew_sir::LoweredModule, diagnostics: Vec<hew_sir::SirDiagnostic>) -> Self {
        let mut by_declaration = Vec::new();
        let mut module_level = Vec::new();
        for diagnostic in diagnostics {
            let owner = diagnostic
                .callable
                .and_then(|callable| sir.module.callable(callable));
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

/// Push zero or more `ItemReport`s for one top-level HIR item onto `items`.
///
/// Function bodies (free fns, impl methods including root-local trait
/// defaults, and actor/machine handler bodies) are `counted: true` — they are
/// what `report.admitted`/`report.total` ratchet on. Everything else is an
/// uncounted inventory line: it has no SIR route to reach 100% under, but
/// the corpus is still fully listed so no declaration is silently omitted.
fn inventory_item(
    module: &hew_hir::HirModule,
    sir: &hew_sir::LoweredModule,
    verifier: &VerifierFindings,
    item: &hew_hir::HirItem,
    root_local_impl_bodies: &HashSet<ItemId>,
    items: &mut Vec<ItemReport>,
) {
    use hew_hir::HirItem;
    match item {
        HirItem::Function(function) => {
            // A free function or impl method belongs to this compilation
            // when it is either a root item outright (free fns and explicit
            // root impl methods) or a trait-default body proven root-local
            // through its owning impl block. Imported module bodies are
            // inventoried when their own file is, never through an importer.
            let is_local = module.root_item_ids.contains(&function.id)
                || root_local_impl_bodies.contains(&function.id);
            if !is_local {
                return;
            }
            let (status, reason) = classify_function(sir, verifier, &function.declaration);
            items.push(ItemReport {
                name: function.declaration.full_path().to_string(),
                status,
                reason,
                counted: true,
            });
        }
        HirItem::Actor(decl) if decl.defining_module.is_none() => {
            inventory_actor_bodies(decl, items);
            items.push(inventory_only(decl.name.clone(), "actor"));
        }
        HirItem::Machine(decl) if decl.defining_module.is_none() => {
            inventory_machine_bodies(decl, items);
            items.push(inventory_only(
                decl.declaration.full_path().to_string(),
                "machine",
            ));
        }
        HirItem::TypeDecl(decl) if decl.defining_module.is_none() => {
            items.push(inventory_only(
                decl.declaration.full_path().to_string(),
                "type",
            ));
        }
        HirItem::Record(decl) if decl.defining_module.is_none() => {
            items.push(inventory_only(decl.name.clone(), "record"));
        }
        HirItem::ExternFn(decl) if matches!(decl.provenance, hew_hir::ExternProvenance::Root) => {
            items.push(inventory_only(
                decl.declaration.full_path().to_string(),
                "extern-fn",
            ));
        }
        HirItem::Impl(block) if impl_is_root(block, &module.root_item_ids) => {
            let name = match &block.trait_name {
                Some(trait_name) => format!("impl {trait_name} for {}", block.self_type_name),
                None => format!("impl {}", block.self_type_name),
            };
            items.push(inventory_only(name, "impl"));
        }
        // Imported (non-root) actor/machine/type/record/extern-fn/impl:
        // inventoried when their own file is, never through an importer.
        HirItem::Actor(_)
        | HirItem::Machine(_)
        | HirItem::TypeDecl(_)
        | HirItem::Record(_)
        | HirItem::ExternFn(_)
        | HirItem::Impl(_) => {}
        // Supervisors are never emitted from an imported module today (their
        // cross-module lowering is a tracked follow-up, hew-hir/src/lower.rs)
        // and carry no executable body of their own — the Rust MIR producer
        // treats `HirItem::Supervisor` as a no-op tier. Always list, never
        // filter and never count.
        HirItem::Supervisor(decl) => {
            items.push(inventory_only(decl.name.clone(), "supervisor"));
        }
        // WHY this shortcut: `HirConst` carries no positive locality field
        // (unlike Type/Record/Actor/Machine's `defining_module` or
        // `HirExternFn`'s `ExternProvenance`), so it cannot be filtered by a
        // proven root/imported fact without reaching for the same
        // absence-in-a-diagnostics-side-table proxy this tool must not use
        // (see `HirModule::root_item_ids`'s doc comment). Every const in
        // scope is listed instead — an imported const's inventory line
        // repeats once per importing file, which is noise, never a
        // miscount, because this row is uncounted: it never enters
        // `report.total`/`admitted`.
        // WHEN obsolete: `HirConst` gains its own `defining_module` field.
        // WHAT the real fix looks like: thread it through const lowering the
        // same way the other declaration kinds carry theirs, then filter
        // like `HirItem::Record` does above.
        HirItem::Const(decl) => {
            items.push(inventory_only(decl.name.clone(), "const"));
        }
    }
}

fn inventory_only(name: String, kind: &str) -> ItemReport {
    ItemReport {
        name,
        status: "legacy",
        reason: Some(format!("item-kind:{kind}")),
        counted: false,
    }
}

/// SIR lowers no actor body at all today — no `HirItem::Actor` construct
/// appears anywhere in `hew-sir/src/lower.rs`. Every handler is therefore
/// `legacy` unconditionally; there is no lowering status to join on.
///
/// WHEN obsolete: hew-sir gains actor admission.
/// WHAT the real fix looks like: mint a `CallableId`/`SirLoweringStatus` per
/// handler the way free functions already have one, then classify each row
/// through `classify_function` exactly like an ordinary body.
fn inventory_actor_bodies(decl: &hew_hir::HirActorDecl, items: &mut Vec<ItemReport>) {
    const REASON: &str = "no-sir-route:actor-body";
    if decl.init.is_some() {
        items.push(no_sir_route(format!("{}::init", decl.name), REASON));
    }
    for handler in &decl.receive_handlers {
        items.push(no_sir_route(
            format!("{}::receive fn {}", decl.name, handler.name),
            REASON,
        ));
    }
    for method in &decl.methods {
        items.push(no_sir_route(
            format!("{}::{}", decl.name, method.name),
            REASON,
        ));
    }
    for hook in &decl.lifecycle_hooks {
        items.push(no_sir_route(
            format!("{}::on({:?}) {}", decl.name, hook.kind, hook.name),
            REASON,
        ));
    }
}

/// SIR lowers no machine body today (same absence of `HirItem::Machine` in
/// `hew-sir/src/lower.rs` as actors); see `inventory_actor_bodies` for the
/// WHEN/WHAT this shortcut resolves under.
fn inventory_machine_bodies(decl: &hew_hir::HirMachineDecl, items: &mut Vec<ItemReport>) {
    const REASON: &str = "no-sir-route:machine-body";
    let qualified = decl.qualified_name();
    for state in &decl.states {
        if state.entry.is_some() {
            items.push(no_sir_route(
                format!("{qualified}::{}.entry", state.name),
                REASON,
            ));
        }
        if state.exit.is_some() {
            items.push(no_sir_route(
                format!("{qualified}::{}.exit", state.name),
                REASON,
            ));
        }
    }
    for transition in &decl.transitions {
        items.push(no_sir_route(
            format!(
                "{qualified}::on {}: {}->{}",
                transition.event_name, transition.source_state, transition.target_state
            ),
            REASON,
        ));
    }
}

fn no_sir_route(name: String, reason: &str) -> ItemReport {
    ItemReport {
        name,
        status: "legacy",
        reason: Some(reason.to_string()),
        counted: true,
    }
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

//! Check generated selection for real machine declarations, for every state
//! configuration and every valuation of their pure guards, through Z3.
use std::path::PathBuf;
#[path = "verify_machines/compiled.rs"]
mod compiled;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf();
    let mut args = std::env::args_os().skip(1).peekable();
    let compiler = if args.peek().is_some_and(|arg| arg == "--compiler") {
        args.next();
        Some(PathBuf::from(
            args.next().ok_or("--compiler requires a path")?,
        ))
    } else {
        None
    };
    let mut files: Vec<PathBuf> = args.map(PathBuf::from).collect();
    let explicit_files = !files.is_empty();
    if files.is_empty() {
        let folder = root.join("tests/core-acceptance/cases");
        for entry in std::fs::read_dir(&folder)? {
            let path = entry?.path();
            if path.extension().is_some_and(|ext| ext == "hew")
                && path
                    .file_name()
                    .unwrap()
                    .to_string_lossy()
                    .starts_with("machine-")
            {
                let case = std::fs::read_to_string(path.with_extension("toml"))?;
                // Rejection fixtures concern the checker, not executable machines.
                if !case.contains("case.expected.diagnostics") {
                    files.push(path);
                }
            }
        }
    }
    files.sort();
    if files.is_empty() {
        return Err("empty machine corpus".into());
    }
    let mut charts = 0;
    let mut checked_files = 0;
    for file in &files {
        eprintln!("Validating {}", file.display());
        let source = std::fs::read_to_string(file)?;
        let parsed = hew_parser::parse(&source);
        if !parsed.errors.is_empty() {
            return Err(format!("{}: {:?}", file.display(), parsed.errors).into());
        }
        let normalized = hew_types::normalize_machine_declarations(&parsed.program)
            .map_err(|errors| format!("{}: {errors:?}", file.display()))?;
        let Some(normalized) = normalized else {
            if explicit_files && compiler.is_none() {
                return Err(format!("{} contains no machines", file.display()).into());
            }
            eprintln!("Import-only fixture excluded: {}", file.display());
            continue;
        };
        charts += normalized.hierarchies.len();
        checked_files += 1;
    }
    if charts == 0 {
        return Err("no machine declarations verified".into());
    }
    let evidence = hew_types::machine_verification_statistics();
    if evidence.selection_passes == 0
        || evidence.selection_claims == 0
        || evidence.execution_passes != evidence.selection_passes
        || evidence.charts != charts
        || evidence.steps != charts * 2
    {
        return Err(format!("incomplete machine validation: {evidence:?}").into());
    }
    if std::env::var_os("HEW_MACHINE_MUTATIONS").is_some()
        && (evidence.selection_defects != 7
            || evidence.execution_defects != 10
            || evidence.step_defects != 5
            || evidence.chart_defects != 5)
    {
        return Err(format!("machine defects escaped: {evidence:?}").into());
    }
    println!(
        "Verified {charts} machine declarations from {checked_files} source files: {evidence:?}"
    );
    if let Some(compiler) = compiler {
        compiled::verify(&compiler, &files, &root)?;
    }
    Ok(())
}

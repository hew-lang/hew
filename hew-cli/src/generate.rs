//! Execute declared source generators before compiling a local package.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::time::Duration;

use hew_pkg::generate::{self, GenerateStep, Stamp};
use hew_pkg::manifest::{DepSpec, HewManifest};

pub(crate) fn cmd_generate(args: &crate::args::GenerateArgs) {
    let result = package_root(args.input.as_deref()).and_then(|root| {
        let root = root.ok_or("no hew.toml found for generation")?;
        prepare_tree(&root, args.check, &mut BTreeSet::new())
    });
    if let Err(error) = result {
        eprintln!("Error: {error}");
        std::process::exit(1);
    }
}

pub(crate) fn prepare(input: Option<&Path>) -> Result<(), String> {
    if let Some(root) = package_root(input)? {
        prepare_tree(&root, false, &mut BTreeSet::new())?;
    }
    Ok(())
}

pub(crate) fn check_package(root: &Path) -> Result<(), String> {
    prepare_tree(root, true, &mut BTreeSet::new())
}

fn package_root(input: Option<&Path>) -> Result<Option<PathBuf>, String> {
    let cwd = std::env::current_dir().map_err(|error| error.to_string())?;
    let path = input.map_or_else(|| cwd.clone(), |path| cwd.join(path));
    let directory = if path.is_dir() {
        path.as_path()
    } else {
        path.parent().unwrap_or(&cwd)
    };
    hew_pkg::project::find_package_root(directory)
        .map(|root| root.canonicalize().map_err(|error| error.to_string()))
        .transpose()
}

fn prepare_tree(root: &Path, check: bool, visited: &mut BTreeSet<PathBuf>) -> Result<(), String> {
    let root = root.canonicalize().map_err(|error| error.to_string())?;
    if !visited.insert(root.clone()) {
        return Ok(());
    }
    let manifest_path = root.join("hew.toml");
    // A workspace manifest without [package] has no package generation steps.
    let text = std::fs::read_to_string(&manifest_path).map_err(|error| error.to_string())?;
    let table = text
        .parse::<toml::Table>()
        .map_err(|error| error.to_string())?;
    if !table.contains_key("package") {
        return Ok(());
    }
    let manifest =
        hew_pkg::manifest::parse_manifest(&manifest_path).map_err(|error| error.to_string())?;
    // Execution permission follows local manifest path dependencies. Installed
    // registry trees are deliberately absent from this traversal.
    for dependency in manifest
        .dependencies
        .values()
        .chain(manifest.dev_dependencies.values())
    {
        if let DepSpec::Table(table) = dependency {
            if let Some(path) = &table.path {
                prepare_tree(&root.join(path), check, visited)?;
            }
        }
    }
    if manifest.generate.is_empty() {
        return Ok(());
    }
    let cache = checked_subdirectory(&root, Path::new(".hew/generate"))?;
    std::fs::create_dir_all(&cache).map_err(|error| error.to_string())?;
    let lock = std::fs::OpenOptions::new()
        .create(true)
        .truncate(false)
        .write(true)
        .open(cache.join("lock"))
        .map_err(|error| error.to_string())?;
    lock.lock()
        .map_err(|error| format!("cannot lock package generation: {error}"))?;
    for index in generate::step_order(&manifest.generate)? {
        run_step(&root, &manifest, &manifest.generate[index], check)?;
    }
    Ok(())
}

#[expect(
    clippy::too_many_lines,
    reason = "keeps admission, staged execution, unchanged-input verification and publication in one transaction"
)]
fn run_step(
    root: &Path,
    manifest: &HewManifest,
    step: &GenerateStep,
    check: bool,
) -> Result<(), String> {
    let (entry, own_package) = generate::generator_entry(root, manifest, step)?;
    let options = crate::compile::CompileOptions {
        project_dir: Some(root.to_path_buf()),
        host_admission: hew_compile::HostAdmission::Generate,
        ..crate::compile::CompileOptions::default()
    };
    let frontend = crate::compile::frontend_options_for_check(&options);
    let checked =
        hew_compile::check_file(&entry.to_string_lossy(), &frontend).map_err(|failure| {
            crate::compile::render_frontend_diagnostics(&failure.diagnostics);
            format!("generator `{}`: {}", step.name, failure.message)
        })?;
    let mut source_files = checked
        .inputs
        .iter()
        .map(|input| input.path.clone())
        .collect::<BTreeSet<_>>();
    source_files.insert(entry.clone());
    refuse_native_packages(&source_files)?;
    let output_directories = manifest
        .generate
        .iter()
        .map(|step| root.join(&step.outputs))
        .collect::<Vec<_>>();
    let mut stamp = generate::stamp(
        root,
        &own_package,
        &output_directories,
        step,
        source_files,
        hew_compile::VERSION,
    )?;
    let cache = checked_subdirectory(root, Path::new(".hew/generate"))?;
    std::fs::create_dir_all(&cache).map_err(|error| error.to_string())?;
    let stamp_path = cache.join(format!("{}.stamp", step.name));
    let output = checked_subdirectory(root, Path::new(&step.outputs))?;
    let previous = generate::output_files(&output)?;
    if !check
        && std::fs::read(&stamp_path)
            .ok()
            .and_then(|bytes| serde_json::from_slice::<Stamp>(&bytes).ok())
            .is_some_and(|old| {
                old.digest == stamp.digest
                    && old.output_digest == generate::output_digest(&previous)
            })
    {
        return Ok(());
    }

    let scratch = tempfile::Builder::new()
        .prefix(&format!("{}-", step.name))
        .tempdir_in(&cache)
        .map_err(|error| error.to_string())?;
    let staging = scratch.path().join("output");
    std::fs::create_dir(&staging).map_err(|error| error.to_string())?;
    let binary = scratch
        .path()
        .join(format!("generator{}", std::env::consts::EXE_SUFFIX));
    crate::compile_native_binary(&entry, &binary, &options)
        .map_err(|_| format!("cannot compile generator `{}`", step.name))?;
    let mut readable = generate::input_files(root, step)?
        .into_iter()
        .collect::<BTreeSet<_>>();
    for file in readable.clone() {
        for directory in file
            .ancestors()
            .skip(1)
            .take_while(|path| path.starts_with(root))
        {
            readable.insert(directory.to_path_buf());
        }
    }
    for file in generate::package_files(&own_package, &output_directories)? {
        for path in file
            .ancestors()
            .take_while(|path| path.starts_with(&own_package))
        {
            readable.insert(path.to_path_buf());
        }
    }
    let policy = serde_json::json!({
        "read_files": readable,
        "read_roots": [],
        "output": output,
        "staging": staging,
    });
    let mut command = std::process::Command::new(&binary);
    command
        .current_dir(root)
        .args(&step.args)
        .env_clear()
        .env("HEW_DETERMINISTIC", "schedule=fifo,seed=0")
        .env("HEW_GENERATE_POLICY", policy.to_string());
    match crate::process::run_command_captured(&mut command, Duration::from_mins(5))? {
        crate::process::BinaryRunOutcome::Success { stdout } => {
            if !stdout.is_empty() {
                eprint!("{stdout}");
            }
        }
        crate::process::BinaryRunOutcome::Failed {
            stdout,
            stderr,
            exit_code,
            ..
        } => {
            return Err(format!(
                "generator `{}` exited with {exit_code}\n{stdout}{stderr}",
                step.name
            ));
        }
        crate::process::BinaryRunOutcome::Timeout => {
            return Err(format!("generator `{}` exceeded 300 seconds", step.name))
        }
    }
    let mut generated = generate::output_files(&staging)?;
    let expected_header = generate::header(step);
    for (path, bytes) in &mut generated {
        let source = std::str::from_utf8(bytes).map_err(|error| {
            format!("generator output {} is not UTF-8: {error}", path.display())
        })?;
        if source.lines().next() != Some(expected_header.as_str()) {
            return Err(format!(
                "generator output {} must start with `{expected_header}`",
                path.display()
            ));
        }
        let parsed = hew_parser::parse(source);
        if let Some(error) = parsed
            .errors
            .iter()
            .find(|error| error.severity == hew_parser::Severity::Error)
        {
            return Err(format!(
                "generator output {} does not parse: {}",
                path.display(),
                error.message
            ));
        }
        let formatted = hew_parser::fmt::format_checked(source, &parsed.program)
            .map_err(|error| format!("cannot format generated {}: {error:?}", path.display()))?;
        *bytes = formatted.into_bytes();
        std::fs::write(staging.join(path), bytes).map_err(|error| error.to_string())?;
    }
    let verified = generate::stamp(
        root,
        &own_package,
        &output_directories,
        step,
        stamp.sources.keys().cloned(),
        hew_compile::VERSION,
    )?;
    if verified.digest != stamp.digest {
        return Err(format!(
            "generator `{}` inputs changed during execution; run `hew generate` again",
            step.name
        ));
    }
    if check {
        if previous != generated {
            return Err(format!(
                "generated output is stale for `{}`; run `hew generate`",
                step.name
            ));
        }
        return Ok(());
    }
    for (path, bytes) in &previous {
        if !std::str::from_utf8(bytes).is_ok_and(generate::is_generated) {
            return Err(format!(
                "refusing to replace unmarked source {} in generator `{}` output",
                output.join(path).display(),
                step.name
            ));
        }
    }
    let parent = output.parent().ok_or("generator output has no parent")?;
    std::fs::create_dir_all(parent).map_err(|error| error.to_string())?;
    let backup = scratch.path().join("previous");
    if output.exists() {
        std::fs::rename(&output, &backup).map_err(|error| error.to_string())?;
    }
    if let Err(error) = std::fs::rename(&staging, &output) {
        if backup.exists() {
            let _ = std::fs::rename(&backup, &output);
        }
        return Err(format!(
            "cannot publish generator `{}` output: {error}",
            step.name
        ));
    }
    stamp.output_digest = generate::output_digest(&generated);
    let temporary_stamp = scratch.path().join("stamp");
    std::fs::write(
        &temporary_stamp,
        serde_json::to_vec(&stamp).map_err(|error| error.to_string())?,
    )
    .map_err(|error| error.to_string())?;
    std::fs::rename(temporary_stamp, stamp_path).map_err(|error| error.to_string())?;
    eprintln!("Generated {} ({})", step.outputs, step.name);
    Ok(())
}

fn refuse_native_packages(inputs: &BTreeSet<PathBuf>) -> Result<(), String> {
    let mut seen = BTreeSet::new();
    for input in inputs {
        let Some(root) = input.parent().and_then(hew_pkg::project::find_package_root) else {
            continue;
        };
        if seen.insert(root.clone())
            && hew_pkg::manifest::parse_native_manifest(&root.join("hew.toml"))
                .map_err(|error| error.to_string())?
                .is_some()
        {
            return Err(format!(
                "E_GENERATE_NATIVE: generator source {} belongs to a package declaring [native]",
                input.display()
            ));
        }
    }
    Ok(())
}

fn checked_subdirectory(root: &Path, relative: &Path) -> Result<PathBuf, String> {
    let path = root.join(relative);
    let existing = path
        .ancestors()
        .find(|ancestor| ancestor.exists())
        .ok_or("generator output has no existing ancestor")?;
    let canonical = existing.canonicalize().map_err(|error| error.to_string())?;
    if !canonical.starts_with(root) {
        return Err(format!(
            "generation path {} escapes its package",
            path.display()
        ));
    }
    if path
        .symlink_metadata()
        .is_ok_and(|metadata| metadata.file_type().is_symlink())
    {
        return Err(format!(
            "generation directory {} must not be a symlink",
            path.display()
        ));
    }
    Ok(path)
}

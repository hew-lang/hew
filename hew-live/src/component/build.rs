//! Build ordinary Hew source with the generated portable service boundary.

use std::path::{Path, PathBuf};
use std::process::Command;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

/// Toolchain locations for portable source compilation.
#[derive(Clone, Debug)]
pub struct Toolchain {
    pub compiler: PathBuf,
    pub wasi_sdk: PathBuf,
    pub wasm_lib_dir: PathBuf,
}

/// Compile an ordinary router source and its sibling Hew modules. Generated
/// services and lifecycle glue stay in a temporary build directory.
///
/// # Errors
/// Returns source, compiler, linker or component validation diagnostics.
pub fn build(source: &Path, toolchain: &Toolchain) -> Result<Vec<u8>> {
    let source = source.canonicalize()?;
    let directory = tempfile::tempdir()?;
    copy_sources(
        source.parent().ok_or("source has no parent")?,
        directory.path(),
    )?;
    super::write_guest_bindings(directory.path())?;
    run(Command::new(&toolchain.compiler)
        .args(["build", "--target", "wasm32-wasip1", "--emit-obj"])
        .args(["--export-c", "create=guest_create"])
        .args(["--export-c-async", "route=guest_route"])
        .args(["--export-c-async", "snapshot=guest_snapshot"])
        .args(["--export-c-async", "dispose=guest_dispose"])
        .arg(
            directory
                .path()
                .join(source.file_name().ok_or("source has no filename")?),
        )
        .arg("-o")
        .arg(directory.path().join("guest.o")))?;
    run(Command::new(toolchain.wasi_sdk.join("bin/clang"))
        .args(["-mexec-model=reactor", "-O2", "-Wall", "-Wextra", "-Werror"])
        .arg("-I")
        .arg(directory.path())
        .arg("-I")
        .arg(directory.path())
        .arg(directory.path().join("router.c"))
        .arg(directory.path().join("router_component_type.o"))
        .arg(directory.path().join("router_adapter.c"))
        .arg(directory.path().join("guest.o"))
        .arg("-L")
        .arg(&toolchain.wasm_lib_dir)
        .args(["-lhew_runtime", "-lhew_std", "-o"])
        .arg(directory.path().join("router.core.wasm")))?;
    Ok(wit_component::ComponentEncoder::default()
        .module(&std::fs::read(directory.path().join("router.core.wasm"))?)?
        .adapter(
            "wasi_snapshot_preview1",
            wasi_preview1_component_adapter_provider::WASI_SNAPSHOT_PREVIEW1_REACTOR_ADAPTER,
        )?
        .validate(true)
        .encode()?)
}

fn copy_sources(source: &Path, destination: &Path) -> Result<()> {
    for entry in std::fs::read_dir(source)? {
        let entry = entry?;
        let kind = entry.file_type()?;
        if kind.is_symlink() {
            continue;
        }
        let name = entry.file_name();
        if kind.is_dir() {
            if name.to_string_lossy().starts_with('.') || name == "target" || name == "node_modules"
            {
                continue;
            }
            let child = destination.join(name);
            std::fs::create_dir(&child)?;
            copy_sources(&entry.path(), &child)?;
        } else if entry
            .path()
            .extension()
            .is_some_and(|extension| extension == "hew")
        {
            std::fs::copy(entry.path(), destination.join(name))?;
        }
    }
    Ok(())
}

fn run(command: &mut Command) -> Result<()> {
    let output = command.output()?;
    if !output.status.success() {
        return Err(format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        )
        .into());
    }
    Ok(())
}

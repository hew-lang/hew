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

/// Compile ordinary router source in its existing project context. Generated
/// services use the compiler source overlay; lifecycle glue remains temporary.
///
/// # Errors
/// Returns source, compiler, linker or component validation diagnostics.
pub fn build(source: &Path, toolchain: &Toolchain) -> Result<Vec<u8>> {
    let source = source.canonicalize()?;
    let directory = tempfile::tempdir()?;
    super::write_guest_bindings(directory.path())?;
    let services = source
        .parent()
        .ok_or("source has no parent")?
        .join("router_services.hew");
    if services.exists() {
        return Err(format!(
            "generated service module would shadow {}",
            services.display()
        )
        .into());
    }
    run(Command::new(&toolchain.compiler)
        .args(["build", "--target", "wasm32-wasip1", "--emit-obj"])
        .args(["--export-c", "create=guest_create"])
        .args(["--export-c-async", "route=guest_route"])
        .args(["--export-c-async", "snapshot=guest_snapshot"])
        .args(["--export-c-async", "dispose=guest_dispose"])
        .arg("--source-overlay")
        .arg(format!(
            "{}={}",
            services.display(),
            directory.path().join("router_services.hew").display()
        ))
        .arg(&source)
        .arg("-o")
        .arg(directory.path().join("guest.o")))?;
    run(Command::new(toolchain.wasi_sdk.join("bin/clang"))
        .args(["-mexec-model=reactor", "-O2", "-Wall", "-Wextra", "-Werror"])
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

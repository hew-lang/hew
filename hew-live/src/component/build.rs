//! Build ordinary Hew source with the generated portable service boundary.

use std::path::{Path, PathBuf};
use std::process::Command;

use super::GuestContract;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

/// Toolchain locations for portable source compilation.
#[derive(Clone, Debug)]
pub struct Toolchain {
    pub compiler: PathBuf,
    pub wasi_sdk: PathBuf,
    pub wasm_lib_dir: PathBuf,
}

/// One compiler source overlay, shared verbatim with editor analysis.
#[derive(Clone, Debug, serde::Serialize)]
pub struct ContextSource {
    pub path: PathBuf,
    pub source: String,
}

/// The selected source's canonical, generated service context.
///
/// # Errors
/// Returns source path or binding generation diagnostics. Authored modules are
/// never silently replaced by the generated service contract.
pub fn source_context(source: &Path) -> Result<Vec<ContextSource>> {
    let source = source.canonicalize()?;
    Ok(vec![ContextSource {
        path: service_path(&source, GuestContract::Router)?,
        source: super::service_module_source()?,
    }])
}

fn service_path(source: &Path, contract: GuestContract) -> Result<PathBuf> {
    let path = source
        .parent()
        .ok_or("source has no parent")?
        .join(format!("{}_services.hew", contract.world()));
    if path.exists() {
        return Err(format!("generated service module would shadow {}", path.display()).into());
    }
    Ok(path)
}

/// Compile ordinary router source in its existing project context. Generated
/// services use the compiler source overlay; lifecycle glue remains temporary.
///
/// # Errors
/// Returns source, compiler, linker or component validation diagnostics.
pub fn build(source: &Path, toolchain: &Toolchain) -> Result<Vec<u8>> {
    build_for(source, toolchain, GuestContract::Router)
}

/// Compile the portable telemetry monitor and generate its calibrated service binding.
/// # Errors
/// Returns source, compiler, linker or component validation diagnostics.
pub fn build_telemetry(source: &Path, toolchain: &Toolchain) -> Result<Vec<u8>> {
    build_for(source, toolchain, GuestContract::Telemetry)
}

fn build_for(source: &Path, toolchain: &Toolchain, contract: GuestContract) -> Result<Vec<u8>> {
    let source = source.canonicalize()?;
    let services = service_path(&source, contract)?;
    let directory = tempfile::tempdir()?;
    super::write_guest_bindings_for(directory.path(), contract)?;
    let mut command = Command::new(&toolchain.compiler);
    command
        .args(["build", "--target", "wasm32-wasip1", "--emit-obj"])
        .args(["--export-c", "create=guest_create"])
        .args(["--export-c-async", "snapshot=guest_snapshot"])
        .args(["--export-c-async", "dispose=guest_dispose"]);
    match contract {
        GuestContract::Router => {
            command.args(["--export-c-async", "route=guest_route"]);
        }
        GuestContract::Telemetry => {
            command.args(["--export-c-async", "observe=guest_route"]);
            for getter in [
                "baseline", "ready", "anomaly", "accepted", "previous", "last",
            ] {
                command
                    .arg("--export-c")
                    .arg(format!("{getter}=guest_{getter}"));
            }
        }
    }
    run(command
        .arg("--source-overlay")
        .arg(format!(
            "{}={}",
            services.display(),
            directory
                .path()
                .join(
                    services
                        .file_name()
                        .ok_or("service module has no file name")?
                )
                .display()
        ))
        .arg(&source)
        .arg("-o")
        .arg(directory.path().join("guest.o")))?;
    link_guest_object_for(
        directory.path(),
        &toolchain.wasi_sdk,
        &toolchain.wasm_lib_dir,
        contract,
    )
}

/// Link a Hew guest object and its generated router bindings into a validated component.
///
/// # Errors
/// Returns toolchain, linker, file or component-encoding diagnostics.
pub fn link_guest_object(
    directory: &Path,
    wasi_sdk: &Path,
    wasm_lib_dir: &Path,
) -> Result<Vec<u8>> {
    link_guest_object_for(directory, wasi_sdk, wasm_lib_dir, GuestContract::Router)
}

fn link_guest_object_for(
    directory: &Path,
    wasi_sdk: &Path,
    wasm_lib_dir: &Path,
    contract: GuestContract,
) -> Result<Vec<u8>> {
    let world = contract.world();
    let core = directory.join(format!("{world}.core.wasm"));
    run(Command::new(wasi_sdk.join("bin/clang"))
        .args(["-mexec-model=reactor", "-O2", "-Wall", "-Wextra", "-Werror"])
        .args(
            hew_cabi::WASM_SHADOW_STACK_LINK_ARGS
                .into_iter()
                .flat_map(|arg| ["-Xlinker", arg]),
        )
        .arg("-I")
        .arg(directory)
        .arg(directory.join(format!("{world}.c")))
        .arg(directory.join(format!("{world}_component_type.o")))
        .arg(directory.join(format!("{world}_adapter.c")))
        .arg(directory.join("guest.o"))
        .arg("-L")
        .arg(wasm_lib_dir)
        .args(["-lhew_runtime", "-lhew_std", "-o"])
        .arg(&core))?;
    Ok(wit_component::ComponentEncoder::default()
        .module(&std::fs::read(core)?)?
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

#[cfg(test)]
mod tests {
    #[test]
    fn editor_context_matches_portable_bindings_without_authored_files() {
        let project = tempfile::tempdir().unwrap();
        let source = project.path().join("router.hew");
        std::fs::write(&source, "import router_services;\n").unwrap();
        let context = super::source_context(&source).unwrap();
        let generated = tempfile::tempdir().unwrap();
        crate::component::write_guest_bindings(generated.path()).unwrap();
        assert_eq!(
            context[0].source,
            std::fs::read_to_string(generated.path().join("router_services.hew")).unwrap()
        );
        assert_eq!(
            context[0].path,
            project
                .path()
                .canonicalize()
                .unwrap()
                .join("router_services.hew")
        );
        assert!(!context[0].path.exists());
        assert_eq!(
            std::fs::read_to_string(&source).unwrap(),
            "import router_services;\n"
        );
        std::fs::write(&context[0].path, "// authored module\n").unwrap();
        assert!(super::source_context(&source)
            .unwrap_err()
            .to_string()
            .contains("would shadow"));
        assert_eq!(
            std::fs::read_to_string(&context[0].path).unwrap(),
            "// authored module\n"
        );
    }
}

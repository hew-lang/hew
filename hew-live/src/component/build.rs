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
    let path = source
        .parent()
        .ok_or("source has no parent")?
        .join("router_services.hew");
    if path.exists() {
        return Err(format!("generated service module would shadow {}", path.display()).into());
    }
    Ok(vec![ContextSource {
        path,
        source: super::service_module_source()?,
    }])
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
    let context = source_context(&source)?;
    let services = &context[0];
    run(Command::new(&toolchain.compiler)
        .args(["build", "--target", "wasm32-wasip1", "--emit-obj"])
        .args(["--export-c", "create=guest_create"])
        .args(["--export-c-async", "route=guest_route"])
        .args(["--export-c-async", "snapshot=guest_snapshot"])
        .args(["--export-c-async", "dispose=guest_dispose"])
        .arg("--source-overlay")
        .arg(format!(
            "{}={}",
            services.path.display(),
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

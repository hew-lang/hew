//! Typed deployment control over ordinary authenticated Hew node services.

use std::path::{Path, PathBuf};
use std::process::Command;

use wit_bindgen_core::wit_parser::Resolve;

use crate::native::Manifest;

mod control;
mod native;

/// A committed deployment observation.
#[derive(Debug)]
pub struct Receipt {
    pub status: super::host::Status,
    pub accepted: u64,
}

pub use super::cache::format_digest;

fn parse_digest(value: &str) -> Result<[u8; 32]> {
    if value.len() != 64 || !value.is_ascii() {
        return Err("component identity must contain 64 hexadecimal digits".into());
    }
    let mut digest = [0; 32];
    for (index, byte) in digest.iter_mut().enumerate() {
        *byte = u8::from_str_radix(&value[index * 2..index * 2 + 2], 16)?;
    }
    Ok(digest)
}

/// An authenticated native node hosting an isolated portable deployment.
pub struct Server {
    control: std::sync::Arc<control::Control>,
}

impl std::fmt::Debug for Server {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.debug_struct("Server").finish_non_exhaustive()
    }
}

impl Server {
    /// Start the node described by the ordinary `HEW_NODE_CONFIG` configuration.
    ///
    /// # Safety
    /// Runtime and module files must be trusted, immutable native code.
    /// # Errors
    /// Returns native startup, identity or component engine errors.
    pub async unsafe fn open(
        runtime: PathBuf,
        module: Module,
        cache: super::cache::ArtifactCache,
        services: std::sync::Arc<dyn super::host::Services>,
    ) -> Result<Self> {
        use std::sync::{Arc, Mutex};
        // SAFETY: caller establishes trusted immutable files.
        let worker = unsafe { native::Worker::open(runtime, module).await? };
        let observer = worker.execute(|driver| Ok(driver.observer())).await?;
        let control = Arc::new(control::Control {
            host: Arc::new(super::host::Host::new(services)?.with_observer(observer)),
            cache: Arc::new(cache),
            worker: worker.clone(),
            handle: tokio::runtime::Handle::current(),
            lifecycle: Mutex::new(control::Lifecycle::default()),
            drained: tokio::sync::Notify::new(),
            deployment: tokio::sync::RwLock::new(None),
            prepared: Mutex::new(std::collections::HashMap::new()),
        });
        let context = Arc::clone(&control);
        if let Err(error) = worker
            .execute(move |driver| control::install(driver, context))
            .await
        {
            let _ = worker.execute(native::Driver::close).await;
            return Err(error);
        }
        Ok(Self { control })
    }

    /// Grant the typed control service to one exact authenticated node identity.
    /// # Errors
    /// Returns native peer or export errors.
    pub async fn grant(&self, identity: String) -> Result<()> {
        self.control
            .worker
            .execute(move |driver| driver.grant(&identity))
            .await
    }

    /// Revoke grants and drain native and portable resources.
    /// # Errors
    /// Returns cleanup failures.
    pub async fn close(self) -> Result<()> {
        self.control.close().await
    }
}

impl Drop for Server {
    fn drop(&mut self) {
        let control = std::sync::Arc::clone(&self.control);
        control.handle.clone().spawn(async move {
            let _ = control.close().await;
        });
    }
}

/// A client holding the ordinary typed control actor grant.
pub struct Client {
    worker: native::Worker,
}

impl std::fmt::Debug for Client {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.debug_struct("Client").finish_non_exhaustive()
    }
}

impl Client {
    /// Connect through the native node's configured credentials and peer grant.
    /// # Safety
    /// Runtime and module files must be trusted, immutable native code.
    /// # Errors
    /// Returns native startup, authentication or service-grant errors.
    pub async unsafe fn connect(
        runtime: PathBuf,
        module: Module,
        endpoint: String,
        identity: String,
    ) -> Result<Self> {
        // SAFETY: caller establishes trusted immutable files.
        let worker = unsafe { native::Worker::open(runtime, module).await? };
        worker
            .execute(move |driver| driver.connect(&endpoint, &identity))
            .await?;
        Ok(Self { worker })
    }

    /// Upload and validate immutable component bytes without changing activation.
    /// # Errors
    /// Returns file, transport or validation failures.
    pub async fn prepare(&self, path: PathBuf) -> Result<String> {
        let path = path
            .to_str()
            .ok_or("component path is not UTF-8")?
            .to_owned();
        self.worker
            .execute(move |driver| driver.prepare(&path))
            .await
    }

    /// Replace the expected predecessor within the requested fence duration.
    /// # Errors
    /// Returns transport, preparation or replacement failures.
    pub async fn replace(
        &self,
        digest: String,
        predecessor: u64,
        within_ms: u64,
    ) -> Result<Receipt> {
        self.worker
            .execute(move |driver| driver.replace(&digest, predecessor, within_ms))
            .await
    }

    /// Observe the current authoritative epoch and digest.
    /// # Errors
    /// Returns transport or service failures.
    pub async fn status(&self) -> Result<super::host::Status> {
        self.worker.execute(native::Driver::status).await
    }

    /// Route one event through the retained active instance.
    /// # Errors
    /// Returns transport or guest failures.
    pub async fn route(&self, event: super::host::Event) -> Result<bool> {
        self.worker
            .execute(move |driver| driver.route(&event.source, &event.body))
            .await
    }

    /// Execute an isolated candidate using explicit simulated service results.
    /// The active deployment and production services are not used.
    /// # Errors
    /// Returns transport, input, preview execution or deadline failures.
    pub async fn preview(
        &self,
        digest: String,
        preview: super::host::Preview,
        within_ms: u64,
    ) -> Result<super::host::PreviewOutcome> {
        self.worker
            .execute(move |driver| driver.preview(&digest, &preview, within_ms))
            .await
    }

    /// Release the client grant and shut down its native node.
    /// # Errors
    /// Returns native cleanup failures.
    pub async fn close(self) -> Result<()> {
        self.worker.execute(native::Driver::close).await
    }
}

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

/// Prepared native transport code importing the host's one native runtime.
#[derive(Clone, Debug)]
pub struct Module {
    pub path: PathBuf,
    pub manifest: Manifest,
}

/// Generate the ordinary Hew node service and its SDK callback adapter.
/// Event records come from the same WIT declarations as component bindings.
///
/// # Errors
/// Returns contract-generation or filesystem errors.
pub fn write_sources(directory: &Path) -> Result<()> {
    let mut resolve = Resolve::default();
    let package = resolve.push_str("router.wit", super::ROUTER_WIT)?;
    let world = resolve.select_world(&[package], Some("router"))?;
    let records = super::hew_bindings::wire_records(&resolve, world)?;
    super::write_host_headers(directory)?;
    std::fs::write(directory.join("node.hew"), include_str!("network/node.hew"))?;
    std::fs::write(directory.join("network_records.hew"), records)?;
    std::fs::write(directory.join("bridge.c"), include_str!("network/bridge.c"))?;
    Ok(())
}

/// Compile the transport module without executing its code or granting access.
/// Uses the same native generation linker and runtime identity checks as other
/// embedded Hew code. The caller retains the immutable output directory.
///
/// # Errors
/// Returns source, toolchain, generation-linker or filesystem errors.
pub fn prepare(
    directory: &Path,
    compiler: &Path,
    runtime: &Path,
    compiler_identity: &str,
    target: &str,
) -> Result<Module> {
    write_sources(directory)?;
    let mut compile = Command::new(compiler);
    compile.args(["build", "--emit-obj"]);
    for name in [
        "start",
        "status_epoch",
        "status_digest",
        "receipt_epoch",
        "receipt_digest",
        "receipt_accepted",
    ] {
        compile.args(["--export-c", &format!("{name}=network_{name}")]);
    }
    for name in [
        "grant",
        "revoke",
        "close_server",
        "connect",
        "close_client",
        "client_prepare",
        "client_replace",
        "client_status",
        "client_route",
        "client_preview",
    ] {
        compile.args(["--export-c", &format!("{name}=network_{name}")]);
    }
    run(compile
        .arg(directory.join("node.hew"))
        .arg("-o")
        .arg(directory.join("node.o")))?;
    run(Command::new("cc")
        .args(["-std=c11", "-fPIC", "-Wall", "-Wextra", "-Werror", "-c"])
        .arg("-I")
        .arg(directory)
        .arg(directory.join("bridge.c"))
        .arg("-o")
        .arg(directory.join("bridge.o")))?;
    run(Command::new("ld")
        .arg("-r")
        .arg(directory.join("node.o"))
        .arg(directory.join("bridge.o"))
        .arg("-o")
        .arg(directory.join("network.o")))?;
    let path = directory.join(if cfg!(target_os = "macos") {
        "network.dylib"
    } else {
        "network.so"
    });
    let manifest = crate::native::prepare(
        &directory.join("network.o"),
        runtime,
        &path,
        compiler_identity,
        target,
    )?;
    Ok(Module { path, manifest })
}

fn run(command: &mut Command) -> Result<()> {
    let output = command.output()?;
    if !output.status.success() {
        return Err(format!("{command:?}: {}", String::from_utf8_lossy(&output.stderr)).into());
    }
    Ok(())
}

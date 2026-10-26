//! Native node adapter for the portable component host.

use std::io::{BufRead, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::{Args, Parser, Subcommand};
use hew_live::component::{build, cache::ArtifactCache, host, network};
use serde::Deserialize;
use serde_json::{json, Value};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

#[derive(Parser)]
struct Options {
    #[command(subcommand)]
    command: Operation,
}

#[derive(Args, Clone)]
struct Native {
    #[arg(long)]
    module: Option<PathBuf>,
    #[arg(long)]
    runtime: Option<PathBuf>,
}

impl Native {
    fn module(&self) -> Result<PathBuf> {
        configured(
            self.module.as_ref(),
            "HEW_LIVE_NODE_MODULE",
            &[
                sdk_bin()?.join("../lib/hew/live-node"),
                sdk_bin()?.join("live-node"),
            ],
        )
    }
    fn runtime(&self) -> Result<PathBuf> {
        let name = if cfg!(target_os = "windows") {
            "hew.dll"
        } else if cfg!(target_os = "macos") {
            "libhew.dylib"
        } else {
            "libhew.so"
        };
        configured(
            self.runtime.as_ref(),
            "HEW_LIVE_RUNTIME",
            &[
                sdk_bin()?.join(name),
                sdk_bin()?.join("../lib/hew").join(name),
            ],
        )
    }
}

fn sdk_bin() -> Result<PathBuf> {
    Ok(std::env::current_exe()?
        .parent()
        .ok_or("companion has no directory")?
        .to_owned())
}
fn executable(name: &str) -> String {
    if cfg!(windows) {
        format!("{name}.exe")
    } else {
        name.to_owned()
    }
}
fn configured(
    explicit: Option<&PathBuf>,
    variable: &str,
    candidates: &[PathBuf],
) -> Result<PathBuf> {
    if let Some(path) = explicit
        .cloned()
        .or_else(|| std::env::var_os(variable).map(PathBuf::from))
    {
        return Ok(path);
    }
    candidates
        .iter()
        .find(|path| path.exists())
        .cloned()
        .ok_or_else(|| {
            format!("SDK artefact missing; set {variable} or its command-line option").into()
        })
}

#[derive(Args, Clone)]
struct BuildTools {
    #[arg(long)]
    compiler: Option<PathBuf>,
    #[arg(long)]
    wasi_sdk: Option<PathBuf>,
    #[arg(long)]
    wasm_lib_dir: Option<PathBuf>,
}

impl BuildTools {
    fn toolchain(&self) -> Result<build::Toolchain> {
        Ok(build::Toolchain {
            compiler: configured(
                self.compiler.as_ref(),
                "HEW_LIVE_COMPILER",
                &[sdk_bin()?.join(executable("hew"))],
            )?,
            wasi_sdk: configured(
                self.wasi_sdk.as_ref(),
                "HEW_WASI_SDK",
                &[sdk_bin()?.join("../lib/hew/wasi-sdk")],
            )?,
            wasm_lib_dir: configured(
                self.wasm_lib_dir.as_ref(),
                "HEW_LIVE_WASM_LIB_DIR",
                &[
                    sdk_bin()?.join("../lib/wasm32-wasip1"),
                    sdk_bin()?.join("../wasm32-wasip1/debug"),
                ],
            )?,
        })
    }
}

#[derive(Subcommand)]
enum Operation {
    /// Prepare the ordinary Hew transport module against one native runtime.
    PrepareTransport {
        #[arg(long)]
        compiler: PathBuf,
        #[arg(long)]
        runtime: PathBuf,
        #[arg(long)]
        output: PathBuf,
    },
    /// Build ordinary Hew source with generated portable service bindings.
    Build {
        #[arg(long)]
        source: PathBuf,
        #[arg(long)]
        output: PathBuf,
        #[command(flatten)]
        tools: BuildTools,
    },
    /// Host portable components behind an authenticated typed node grant.
    Serve {
        #[command(flatten)]
        native: Native,
        #[arg(long)]
        cache: PathBuf,
        #[arg(long)]
        allow_peer: String,
        /// Prefix destinations provided by this reference node's service grant.
        #[arg(long, default_value = "")]
        destination_prefix: String,
    },
    /// Attach an editor using local JSON lines and the typed remote node service.
    Attach {
        #[command(flatten)]
        native: Native,
        #[arg(long)]
        endpoint: String,
        #[arg(long)]
        node: String,
        #[command(flatten)]
        tools: BuildTools,
    },
}

#[derive(Deserialize)]
struct Request {
    id: Value,
    #[serde(flatten)]
    operation: RequestOperation,
}

#[derive(Deserialize)]
#[serde(tag = "op", rename_all = "snake_case")]
enum RequestOperation {
    Prepare {
        component: PathBuf,
    },
    BuildPrepare {
        source: PathBuf,
    },
    Context {
        source: PathBuf,
    },
    Replace {
        digest: String,
        predecessor: u64,
        within_ms: u64,
    },
    Status,
    Preview {
        digest: String,
        preview: host::Preview,
        within_ms: u64,
    },
    Route {
        source: String,
        body: String,
    },
}

struct Services {
    destination_prefix: String,
}
impl host::Services for Services {
    fn destination(&self, source: String) -> host::ServiceCall<Option<String>> {
        let destination = format!("{}{source}", self.destination_prefix);
        Box::pin(async move { Ok(Some(destination)) })
    }
    fn emit(&self, message: host::Delivery) -> host::ServiceCall<()> {
        Box::pin(async move {
            eprintln!(
                "{}",
                json!({"event":"delivery", "destination":message.destination, "body":message.body})
            );
            Ok(())
        })
    }
}

fn module(directory: &Path) -> Result<network::Module> {
    let manifest = serde_json::from_slice(&std::fs::read(directory.join("network.json"))?)?;
    Ok(network::Module {
        path: directory.join(if cfg!(target_os = "macos") {
            "network.dylib"
        } else {
            "network.so"
        }),
        manifest,
    })
}

fn status(value: host::Status) -> Value {
    json!({"epoch":value.epoch, "digest":network::format_digest(value.digest)})
}

async fn request(
    client: &network::Client,
    operation: RequestOperation,
    tools: &BuildTools,
) -> Result<Value> {
    match operation {
        RequestOperation::Context { source } => {
            let compiler = configured(
                tools.compiler.as_ref(),
                "HEW_LIVE_COMPILER",
                &[sdk_bin()?.join(executable("hew"))],
            )?
            .canonicalize()?;
            Ok(json!({"version":1, "compiler":compiler, "sources":build::source_context(&source)?}))
        }
        RequestOperation::Prepare { component } => {
            Ok(json!({"digest":client.prepare(component).await?}))
        }
        RequestOperation::BuildPrepare { source } => {
            let toolchain = tools.toolchain()?;
            let bytes =
                tokio::task::spawn_blocking(move || build::build(&source, &toolchain)).await??;
            let file = tempfile::Builder::new().suffix(".wasm").tempfile()?;
            std::fs::write(file.path(), bytes)?;
            Ok(json!({"digest":client.prepare(file.path().to_owned()).await?}))
        }
        RequestOperation::Replace {
            digest,
            predecessor,
            within_ms,
        } => {
            let receipt = client.replace(digest, predecessor, within_ms).await?;
            Ok(
                json!({"epoch":receipt.status.epoch, "digest":network::format_digest(receipt.status.digest), "accepted":receipt.accepted}),
            )
        }
        RequestOperation::Preview {
            digest,
            preview,
            within_ms,
        } => Ok(serde_json::to_value(
            client.preview(digest, preview, within_ms).await?,
        )?),
        RequestOperation::Status => Ok(status(client.status().await?)),
        RequestOperation::Route { source, body } => {
            Ok(json!({"accepted":client.route(host::Event { source, body }).await?}))
        }
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<()> {
    match Options::parse().command {
        Operation::PrepareTransport {
            compiler,
            runtime,
            output,
        } => {
            let version = std::process::Command::new(&compiler)
                .arg("--version")
                .output()?;
            if !version.status.success() {
                return Err("could not read compiler identity".into());
            }
            let identity = String::from_utf8(version.stdout)?;
            let target = format!("{}-{}", std::env::consts::ARCH, std::env::consts::OS);
            let module = network::prepare(&output, &compiler, &runtime, identity.trim(), &target)?;
            std::fs::write(
                output.join("network.json"),
                serde_json::to_vec_pretty(&module.manifest)?,
            )?;
            println!("{}", module.path.display());
        }
        Operation::Build {
            source,
            output,
            tools,
        } => {
            std::fs::write(output, build::build(&source, &tools.toolchain()?)?)?;
        }
        Operation::Serve {
            native,
            cache,
            allow_peer,
            destination_prefix,
        } => {
            // SAFETY: explicitly selected local SDK artefacts are trusted native code.
            let server = unsafe {
                network::Server::open(
                    native.runtime()?,
                    module(&native.module()?)?,
                    ArtifactCache::open(cache)?,
                    Arc::new(Services { destination_prefix }),
                )
                .await?
            };
            eprintln!("node started; waiting for authorized peer {allow_peer}");
            server.grant(allow_peer).await?;
            eprintln!("authorized peer watcher active");
            // EOF is an orderly service shutdown, convenient for parent process ownership.
            tokio::task::spawn_blocking(|| {
                for line in std::io::stdin().lock().lines() {
                    if line.is_err() {
                        break;
                    }
                }
            })
            .await?;
            server.close().await?;
        }
        Operation::Attach {
            native,
            endpoint,
            node,
            tools,
        } => {
            // SAFETY: explicitly selected local SDK artefacts are trusted native code.
            let client = unsafe {
                network::Client::connect(
                    native.runtime()?,
                    module(&native.module()?)?,
                    endpoint,
                    node,
                )
                .await?
            };
            let (send, mut receive) = tokio::sync::mpsc::channel(8);
            std::thread::spawn(move || {
                for line in std::io::stdin().lock().lines() {
                    if send.blocking_send(line).is_err() {
                        break;
                    }
                }
            });
            while let Some(line) = receive.recv().await {
                let parsed: Request = serde_json::from_str(&line?)?;
                let result = request(&client, parsed.operation, &tools).await;
                let reply = match result {
                    Ok(result) => json!({"id":parsed.id,"ok":true,"result":result}),
                    Err(error) => json!({"id":parsed.id,"ok":false,"error":error.to_string()}),
                };
                println!("{reply}");
                std::io::stdout().flush()?;
            }
            client.close().await?;
        }
    }
    Ok(())
}

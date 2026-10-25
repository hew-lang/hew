//! Fenced replacement of retained component instances.

use std::fmt;
use std::future::{poll_fn, Future};
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::task::Poll;
use std::time::Duration;

use sha2::{Digest, Sha256};
use tokio::sync::{mpsc, oneshot, OwnedRwLockReadGuard, OwnedRwLockWriteGuard, RwLock};
use tokio::task::JoinHandle;
use tokio::time::Instant;
use wasmtime::component::{Accessor, AccessorTask, Component, Linker, ResourceTable};
use wasmtime::{Config, Engine, Store};
use wasmtime_wasi::{WasiCtx, WasiCtxBuilder, WasiCtxView, WasiView};

use super::bindings;
pub use bindings::hew::live_router::events::{Delivery, Event, Snapshot};

/// An asynchronous operation on a granted service.
pub type ServiceCall<T> = Pin<Box<dyn Future<Output = Result<T, String>> + Send>>;

/// Explicit production services. Staged instances cannot call these providers.
pub trait Services: Send + Sync + 'static {
    fn destination(&self, source: String) -> ServiceCall<Option<String>>;
    fn emit(&self, message: Delivery) -> ServiceCall<()>;
}

/// Failure to execute or replace a portable deployment.
#[derive(Debug)]
pub enum Error {
    Engine(wasmtime::Error),
    Guest(String),
    Closed,
    Expired(Stage),
    PredecessorChanged,
    InvalidDeadline,
}

/// Work that owned the fenced interval when its deadline expired.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Stage {
    Admission,
    Snapshot,
    Initialization,
}

impl fmt::Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Engine(error) => error.fmt(formatter),
            Self::Guest(error) => formatter.write_str(error),
            Self::Closed => formatter.write_str("component host closed"),
            Self::Expired(stage) => {
                write!(formatter, "replacement attempt expired during {stage:?}")
            }
            Self::PredecessorChanged => formatter.write_str("deployment predecessor changed"),
            Self::InvalidDeadline => {
                formatter.write_str("replacement duration exceeds the host clock")
            }
        }
    }
}

impl std::error::Error for Error {}

impl From<wasmtime::Error> for Error {
    fn from(error: wasmtime::Error) -> Self {
        Self::Engine(error)
    }
}

/// Immutable component preparation. Construction does not execute guest code.
#[derive(Clone)]
pub struct Prepared {
    component: Component,
    digest: [u8; 32],
}

impl fmt::Debug for Prepared {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("Prepared")
            .field("digest", &self.digest)
            .finish_non_exhaustive()
    }
}

/// A host with explicit service grants and no inherited WASI environment.
pub struct Host {
    engine: Engine,
    services: Arc<dyn Services>,
}

impl fmt::Debug for Host {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.debug_struct("Host").finish_non_exhaustive()
    }
}

impl Host {
    /// Create a host. Fuel yields to the executor without expiring the guest.
    ///
    /// # Errors
    /// Returns engine configuration failures.
    pub fn new(services: Arc<dyn Services>) -> Result<Self, Error> {
        let mut config = Config::new();
        config.wasm_component_model_async(true).consume_fuel(true);
        Ok(Self {
            engine: Engine::new(&config)?,
            services,
        })
    }

    /// Compile immutable bytes before fencing any active deployment.
    ///
    /// # Errors
    /// Returns component validation or compilation failures.
    pub fn prepare(&self, bytes: &[u8]) -> Result<Prepared, Error> {
        Ok(Prepared {
            component: Component::new(&self.engine, bytes)?,
            digest: Sha256::digest(bytes).into(),
        })
    }

    /// Initialize a deployment before enabling its production services.
    ///
    /// # Errors
    /// Returns instantiation or initialization failures.
    pub async fn activate(
        self: &Arc<Self>,
        prepared: Prepared,
        state: Snapshot,
    ) -> Result<Deployment, Error> {
        let instance = self.instantiate(prepared).await?;
        instance.initialize(state).await?;
        instance.granted.store(true, Ordering::Release);
        Ok(Deployment {
            host: Arc::clone(self),
            active: Arc::new(RwLock::new(Arc::new(Activation { epoch: 1, instance }))),
        })
    }

    async fn instantiate(&self, prepared: Prepared) -> Result<Arc<Instance>, Error> {
        let (commands, receiver) = mpsc::channel(64);
        let (ready, initialized) = oneshot::channel();
        let granted = Arc::new(AtomicBool::new(false));
        let engine = self.engine.clone();
        let services = Arc::clone(&self.services);
        let worker_grant = Arc::clone(&granted);
        let worker = tokio::spawn(async move {
            match guest(&engine, &prepared.component, services, worker_grant).await {
                Ok((mut store, router)) => {
                    let _ = ready.send(Ok(()));
                    let _ = store
                        .run_concurrent(async |accessor| {
                            serve(accessor, Arc::new(router), receiver).await
                        })
                        .await;
                }
                Err(error) => {
                    let _ = ready.send(Err(error));
                }
            }
        });
        let instance = Arc::new(Instance {
            commands,
            granted,
            worker,
            digest: prepared.digest,
            snapshot_gate: Arc::default(),
        });
        initialized.await.map_err(|_| Error::Closed)??;
        Ok(instance)
    }
}

/// The one authority for an active instance and its admission epoch.
#[derive(Debug)]
pub struct Deployment {
    host: Arc<Host>,
    active: Arc<RwLock<Arc<Activation>>>,
}

impl Deployment {
    /// Admit a call to one activation and retain that admission through release.
    /// Cancelling the caller's Rust future does not release a running guest call.
    ///
    /// # Errors
    /// Returns guest faults, service refusals or a closed host.
    pub async fn route(&self, event: Event) -> Result<bool, Error> {
        let lease = Arc::clone(&self.active).read_owned().await;
        let instance = Arc::clone(&lease.instance);
        let (reply, result) = oneshot::channel();
        instance
            .commands
            .send(Command::Route {
                event,
                reply,
                lease,
            })
            .await
            .map_err(|_| Error::Closed)?;
        result.await.map_err(|_| Error::Closed)?
    }

    /// Observe the current admission epoch.
    pub async fn epoch(&self) -> u64 {
        self.active.read().await.epoch
    }

    /// Replace the expected activation within one fenced interval. Candidate
    /// instantiation happens first without grants. The deadline covers admission
    /// drain, snapshot and candidate initialization; expiry leaves the old Store
    /// running and queues resume before reopening host admission.
    ///
    /// # Errors
    /// Returns expiry, a changed predecessor, or guest/engine failures.
    pub async fn replace(
        &self,
        prepared: Prepared,
        predecessor: u64,
        within: Duration,
    ) -> Result<Committed, Error> {
        let candidate = self.host.instantiate(prepared).await?;
        let deadline = Instant::now()
            .checked_add(within)
            .ok_or(Error::InvalidDeadline)?;
        let mut stage = Stage::Admission;
        tokio::time::timeout_at(deadline, async {
            let active = Arc::clone(&self.active).write_owned().await;
            if active.epoch != predecessor {
                return Err(Error::PredecessorChanged);
            }
            let epoch = predecessor
                .checked_add(1)
                .ok_or(Error::PredecessorChanged)?;
            let attempt = Arc::new(Mutex::new(Attempt::Pending));
            let mut fence = Fence {
                active,
                attempt: Arc::clone(&attempt),
                committed: false,
            };
            let old = Arc::clone(&fence.active.instance);
            stage = Stage::Snapshot;
            let snapshot = old.snapshot(attempt).await?;
            stage = Stage::Initialization;
            candidate.initialize(snapshot).await?;
            if Instant::now() >= deadline {
                return Err(Error::Expired(stage));
            }
            candidate.granted.store(true, Ordering::Release);
            old.granted.store(false, Ordering::Release);
            *fence.active = Arc::new(Activation {
                epoch,
                instance: Arc::clone(&candidate),
            });
            fence.committed = true;
            let retirement = tokio::spawn(async move { old.dispose().await });
            Ok(Committed {
                epoch,
                digest: candidate.digest,
                snapshot,
                retirement,
            })
        })
        .await
        .map_err(|_| Error::Expired(stage))?
    }

    /// Consume this deployment and retain its cleanup task until completion,
    /// even if the caller drops the returned join handle.
    ///
    /// # Panics
    /// Requires a current Tokio runtime, like activation and replacement.
    #[must_use]
    pub fn dispose(self) -> JoinHandle<Result<(), Error>> {
        tokio::spawn(async move {
            let active = self.active.write().await;
            let attempt = Arc::new(Mutex::new(Attempt::Pending));
            active.instance.snapshot(attempt).await?;
            active.instance.dispose().await
        })
    }
}

/// Activation succeeded; old-instance cleanup is accounted for separately.
#[derive(Debug)]
pub struct Committed {
    pub epoch: u64,
    pub digest: [u8; 32],
    pub snapshot: Snapshot,
    pub retirement: JoinHandle<Result<(), Error>>,
}

#[derive(Debug)]
struct Activation {
    epoch: u64,
    instance: Arc<Instance>,
}

#[derive(Debug)]
struct Instance {
    commands: mpsc::Sender<Command>,
    granted: Arc<AtomicBool>,
    worker: JoinHandle<()>,
    digest: [u8; 32],
    snapshot_gate: Arc<tokio::sync::Mutex<()>>,
}

impl Drop for Instance {
    fn drop(&mut self) {
        self.worker.abort();
    }
}

impl Instance {
    async fn initialize(&self, state: Snapshot) -> Result<(), Error> {
        let (reply, result) = oneshot::channel();
        self.commands
            .send(Command::Initialize { state, reply })
            .await
            .map_err(|_| Error::Closed)?;
        result.await.map_err(|_| Error::Closed)?
    }

    async fn snapshot(&self, attempt: Arc<Mutex<Attempt>>) -> Result<Snapshot, Error> {
        let (reply, result) = oneshot::channel();
        self.commands
            .send(Command::Snapshot {
                attempt,
                reply,
                gate: Arc::clone(&self.snapshot_gate),
            })
            .await
            .map_err(|_| Error::Closed)?;
        result.await.map_err(|_| Error::Closed)?
    }

    async fn dispose(&self) -> Result<(), Error> {
        let (reply, result) = oneshot::channel();
        self.commands
            .send(Command::Dispose { reply })
            .await
            .map_err(|_| Error::Closed)?;
        result.await.map_err(|_| Error::Closed)?
    }
}

enum Attempt {
    Pending,
    Staged,
    Revoked,
}

struct Fence {
    active: OwnedRwLockWriteGuard<Arc<Activation>>,
    attempt: Arc<Mutex<Attempt>>,
    committed: bool,
}

impl Drop for Fence {
    fn drop(&mut self) {
        if self.committed {
            return;
        }
        let mut attempt = self
            .attempt
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let staged = matches!(*attempt, Attempt::Staged);
        *attempt = Attempt::Revoked;
        if staged {
            // The exclusive admission fence has drained every route, leaving
            // room for this control message before new route leases can enter.
            let _ = self.active.instance.commands.try_send(Command::Resume);
        }
    }
}

enum Command {
    Route {
        event: Event,
        reply: oneshot::Sender<Result<bool, Error>>,
        lease: OwnedRwLockReadGuard<Arc<Activation>>,
    },
    Snapshot {
        attempt: Arc<Mutex<Attempt>>,
        reply: oneshot::Sender<Result<Snapshot, Error>>,
        gate: Arc<tokio::sync::Mutex<()>>,
    },
    Initialize {
        state: Snapshot,
        reply: oneshot::Sender<Result<(), Error>>,
    },
    Dispose {
        reply: oneshot::Sender<Result<(), Error>>,
    },
    Resume,
}

struct Context {
    wasi: WasiCtx,
    resources: ResourceTable,
}

impl WasiView for Context {
    fn ctx(&mut self) -> WasiCtxView<'_> {
        WasiCtxView {
            ctx: &mut self.wasi,
            table: &mut self.resources,
        }
    }
}

async fn guest(
    engine: &Engine,
    component: &Component,
    services: Arc<dyn Services>,
    granted: Arc<AtomicBool>,
) -> Result<(Store<Context>, bindings::Router), Error> {
    let mut linker = Linker::new(engine);
    wasmtime_wasi::p2::add_to_linker_async(&mut linker)?;
    let mut imports = linker.instance("hew:live-router/services@0.1.0")?;
    let destinations = Arc::clone(&services);
    let destination_grant = Arc::clone(&granted);
    imports.func_wrap_concurrent("destination", move |_accessor, (source,): (String,)| {
        let provider = Arc::clone(&destinations);
        let granted = destination_grant.load(Ordering::Acquire);
        Box::pin(async move {
            Ok((if granted {
                provider.destination(source).await
            } else {
                Err("production services are not granted".into())
            },))
        })
    })?;
    imports.func_wrap_concurrent("emit", move |_accessor, (message,): (Delivery,)| {
        let provider = Arc::clone(&services);
        let granted = granted.load(Ordering::Acquire);
        Box::pin(async move {
            Ok((if granted {
                provider.emit(message).await
            } else {
                Err("production services are not granted".into())
            },))
        })
    })?;
    let mut store = Store::new(
        engine,
        Context {
            wasi: WasiCtxBuilder::new().build(),
            resources: ResourceTable::new(),
        },
    );
    store.set_fuel(u64::MAX)?;
    store.fuel_async_yield_interval(Some(100_000))?;
    let router = bindings::Router::instantiate_async(&mut store, component, &linker).await?;
    Ok((store, router))
}

async fn serve(
    accessor: &Accessor<Context>,
    router: Arc<bindings::Router>,
    mut commands: mpsc::Receiver<Command>,
) -> wasmtime::Result<()> {
    while let Some(command) = commands.recv().await {
        match command {
            Command::Route {
                event,
                reply,
                lease,
            } => {
                accessor.spawn(Route {
                    router: Arc::clone(&router),
                    event,
                    reply,
                    _lease: lease,
                })?;
            }
            Command::Snapshot {
                attempt,
                reply,
                gate,
            } => {
                accessor.spawn(SnapshotCall {
                    router: Arc::clone(&router),
                    attempt,
                    reply,
                    gate,
                })?;
            }
            Command::Initialize { state, reply } => {
                let result = router
                    .hew_live_router_handler()
                    .call_initialize(accessor, state)
                    .await;
                let _ = reply.send(
                    result
                        .map_err(Error::Engine)
                        .and_then(|value| value.map_err(Error::Guest)),
                );
            }
            Command::Resume => {
                // Complete this control call before dispatching later routes.
                let _ = router.hew_live_router_handler().call_resume(accessor).await;
            }
            Command::Dispose { reply } => {
                let result = router
                    .hew_live_router_handler()
                    .call_dispose(accessor)
                    .await;
                let _ = reply.send(
                    result
                        .map_err(Error::Engine)
                        .and_then(|value| value.map_err(Error::Guest)),
                );
                break;
            }
        }
    }
    Ok(())
}

struct Route {
    router: Arc<bindings::Router>,
    event: Event,
    reply: oneshot::Sender<Result<bool, Error>>,
    _lease: OwnedRwLockReadGuard<Arc<Activation>>,
}

impl AccessorTask<Context> for Route {
    async fn run(self, accessor: &Accessor<Context>) -> wasmtime::Result<()> {
        let result = self
            .router
            .hew_live_router_handler()
            .call_route(accessor, self.event)
            .await;
        let _ = self.reply.send(
            result
                .map_err(Error::Engine)
                .and_then(|value| value.map_err(Error::Guest)),
        );
        Ok(())
    }
}

struct SnapshotCall {
    router: Arc<bindings::Router>,
    attempt: Arc<Mutex<Attempt>>,
    reply: oneshot::Sender<Result<Snapshot, Error>>,
    gate: Arc<tokio::sync::Mutex<()>>,
}

impl AccessorTask<Context> for SnapshotCall {
    async fn run(self, accessor: &Accessor<Context>) -> wasmtime::Result<()> {
        // Expiry reopens routes immediately, but a new snapshot must wait for
        // the previous borrowed call's cooperative release to finish.
        let _snapshot = self.gate.lock().await;
        let mut call = Box::pin(self.router.hew_live_router_handler().call_quiesce(accessor));
        // Stage the component call under the same attempt lock that expiry
        // revokes. A late host task cannot close admission after resume.
        let initial = poll_fn(|context| {
            let mut attempt = self
                .attempt
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if matches!(*attempt, Attempt::Revoked) {
                return Poll::Ready(None);
            }
            let result = call.as_mut().poll(context);
            *attempt = Attempt::Staged;
            Poll::Ready(Some(result))
        })
        .await;
        let result = match initial {
            None => Err(Error::Expired(Stage::Snapshot)),
            Some(Poll::Ready(result)) => result
                .map_err(Error::Engine)
                .and_then(|value| value.map_err(Error::Guest)),
            Some(Poll::Pending) => call
                .await
                .map_err(Error::Engine)
                .and_then(|value| value.map_err(Error::Guest)),
        };
        let _ = self.reply.send(result);
        Ok(())
    }
}

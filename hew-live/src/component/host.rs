//! Fenced replacement of retained component instances.

use std::fmt;
use std::future::{poll_fn, Future};
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::task::Poll;
use std::time::Duration;

use tokio::sync::{
    mpsc, oneshot, OwnedRwLockReadGuard, OwnedRwLockWriteGuard, OwnedSemaphorePermit, RwLock,
};
use tokio::task::JoinHandle;
use tokio::time::Instant;
use wasmtime::component::{Accessor, AccessorTask, Component, Linker, ResourceTable};
use wasmtime::{Config, Engine, Store, StoreLimits};
use wasmtime_wasi::{WasiCtx, WasiCtxBuilder, WasiCtxView, WasiView};

use super::bindings;
use super::cache::{self, ArtifactCache};
pub use bindings::hew::live_router::events::{Delivery, Event, Snapshot};

mod inspection;
mod policy;
pub use inspection::{Observer, Preview, PreviewOutcome, ServiceFixture};
pub use policy::{HostPolicy, PolicyLimit};

static NEXT_IDENTITY: AtomicU64 = AtomicU64::new(1);

fn next_identity() -> Result<u64, Error> {
    NEXT_IDENTITY
        .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |value| {
            value.checked_add(1)
        })
        .map_err(|_| Error::IdentityExhausted)
}

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
    Cache(std::io::Error),
    Guest(String),
    Closed,
    Expired(Stage),
    PredecessorChanged,
    InvalidDeadline,
    IdentityExhausted,
    Refused(PolicyLimit),
    InvalidPolicy(&'static str),
}

/// Work that owned the fenced interval when its deadline expired.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Stage {
    Admission,
    Snapshot,
    Initialization,
    Preview,
}

impl fmt::Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Engine(error) => error.fmt(formatter),
            Self::Cache(error) => error.fmt(formatter),
            Self::Guest(error) => formatter.write_str(error),
            Self::Closed => formatter.write_str("component host closed"),
            Self::Expired(Stage::Preview) => {
                formatter.write_str("preview deadline expired; guest cleanup is unconfirmed")
            }
            Self::Expired(stage) => {
                write!(formatter, "replacement attempt expired during {stage:?}")
            }
            Self::PredecessorChanged => formatter.write_str("deployment predecessor changed"),
            Self::InvalidDeadline => {
                formatter.write_str("replacement duration exceeds the host clock")
            }
            Self::IdentityExhausted => formatter.write_str("host execution identity exhausted"),
            Self::Refused(limit) => {
                write!(formatter, "component host refused admission: {limit:?}")
            }
            Self::InvalidPolicy(field) => {
                write!(formatter, "invalid component host policy: {field}")
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
    retention: Arc<OwnedSemaphorePermit>,
}

impl Prepared {
    pub(super) fn cache_exclusive(&self) -> bool {
        Arc::strong_count(&self.retention) == 1
    }

    /// The content identity of the validated portable component bytes.
    #[must_use]
    pub fn digest(&self) -> [u8; 32] {
        self.digest
    }
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
    observer: Option<Arc<dyn Observer>>,
    mode: &'static str,
    policy: Arc<HostPolicy>,
    budgets: Arc<policy::Budgets>,
}

impl fmt::Debug for Host {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.debug_struct("Host").finish_non_exhaustive()
    }
}

impl Host {
    /// Create a host with finite resource bounds and no inherited WASI grants.
    ///
    /// # Errors
    /// Returns engine configuration failures.
    pub fn new(services: Arc<dyn Services>) -> Result<Self, Error> {
        Self::with_policy(services, HostPolicy::default())
    }

    /// Create a host with explicit finite limits. Instance fuel and service
    /// traffic include all descendant work for that instance's lifetime.
    ///
    /// # Errors
    /// Returns invalid policy or engine configuration failures.
    pub fn with_policy(services: Arc<dyn Services>, policy: HostPolicy) -> Result<Self, Error> {
        policy.validate()?;
        let mut config = Config::new();
        config.wasm_component_model_async(true).consume_fuel(true);
        Ok(Self {
            engine: Engine::new(&config)?,
            services,
            observer: None,
            mode: "production",
            budgets: Arc::new(policy::Budgets::new(&policy)),
            policy: Arc::new(policy),
        })
    }

    /// The policy shared by this host and its isolated previews.
    #[must_use]
    pub fn policy(&self) -> &HostPolicy {
        &self.policy
    }

    /// Attach metadata-only observation without granting control or services.
    #[must_use]
    pub fn with_observer(mut self, observer: Arc<dyn Observer>) -> Self {
        self.observer = Some(observer);
        self
    }

    /// Compile immutable bytes before fencing any active deployment.
    ///
    /// # Errors
    /// Returns component validation or compilation failures.
    pub fn prepare(&self, bytes: &[u8]) -> Result<Prepared, Error> {
        self.policy.component(bytes.len())?;
        let _preparing = policy::reserve(&self.budgets.preparing, PolicyLimit::Preparations)?;
        let retention = Arc::new(policy::reserve(
            &self.budgets.prepared,
            PolicyLimit::PreparedComponents,
        )?);
        Ok(Prepared {
            component: Component::new(&self.engine, bytes)?,
            digest: cache::digest(bytes),
            retention,
        })
    }

    /// Validate and prepare a component, then persist its immutable bytes.
    /// Failed validation never publishes an artefact to the cache.
    ///
    /// # Errors
    /// Returns validation, compilation or cache publication failures.
    pub fn prepare_cached(&self, cache: &ArtifactCache, bytes: &[u8]) -> Result<Prepared, Error> {
        let prepared = self.prepare(bytes)?;
        cache.insert(bytes, prepared.digest).map_err(Error::Cache)?;
        Ok(prepared)
    }

    /// Verify cached bytes and prepare them with this host's current engine.
    ///
    /// # Errors
    /// Returns cache verification, validation or compilation failures.
    pub fn restore(&self, cache: &ArtifactCache, digest: [u8; 32]) -> Result<Prepared, Error> {
        let bytes = cache
            .load_limited(digest, self.policy.max_component_bytes)
            .map_err(|error| {
                if error.kind() == std::io::ErrorKind::FileTooLarge {
                    Error::Refused(PolicyLimit::ComponentBytes)
                } else {
                    Error::Cache(error)
                }
            })?;
        self.prepare(&bytes)
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
        instance.activate();
        let deployment = Deployment {
            host: Arc::clone(self),
            id: next_identity()?,
            active: Arc::new(RwLock::new(Arc::new(Activation { epoch: 1, instance }))),
        };
        deployment.record(deployment.status().await, None, "active");
        Ok(deployment)
    }

    async fn instantiate(&self, prepared: Prepared) -> Result<Arc<Instance>, Error> {
        let instance_permit = policy::reserve(&self.budgets.instances, PolicyLimit::Instances)?;
        let (commands, receiver) = mpsc::channel(self.policy.queue_capacity);
        let (ready, initialized) = oneshot::channel();
        let (failed, failure) = tokio::sync::watch::channel(None);
        let granted = Arc::new(AtomicBool::new(false));
        let engine = self.engine.clone();
        let services = Arc::clone(&self.services);
        let worker_grant = Arc::clone(&granted);
        let wake = Arc::new(tokio::sync::Notify::new());
        let worker_wake = Arc::clone(&wake);
        let policy = Arc::clone(&self.policy);
        let budgets = Arc::clone(&self.budgets);
        let worker = tokio::spawn(async move {
            // These permits belong to the Store owner, not the caller or its
            // JoinHandle. Abort releases them only when that future is dropped.
            let _instance_permit = instance_permit;
            let _preparation = Arc::clone(&prepared.retention);
            match guest(
                &engine,
                &prepared.component,
                services,
                worker_grant,
                policy,
                budgets,
            )
            .await
            {
                Ok((mut store, router)) => {
                    let _ = ready.send(Ok(()));
                    let result = store
                        .run_concurrent(async |accessor| {
                            serve(accessor, Arc::new(router), receiver, worker_wake).await
                        })
                        .await
                        .and_then(std::convert::identity);
                    if let Err(error) = result {
                        let _ = failed.send(Some(format!("{error:#}")));
                    }
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
            wake,
            failure,
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
    id: u64,
}

/// One consistent observation of the active deployment authority.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Status {
    pub epoch: u64,
    pub digest: [u8; 32],
}

impl Deployment {
    /// Admit a call to one activation and retain that admission through release.
    /// Cancelling the caller's Rust future does not release a running guest call.
    ///
    /// # Errors
    /// Returns guest faults, service refusals or a closed host.
    pub async fn route(&self, event: Event) -> Result<bool, Error> {
        self.host
            .policy
            .payload(&[event.source.len(), event.body.len()])?;
        let admission = policy::reserve(&self.host.budgets.routes, PolicyLimit::Routes)?;
        let lease = Arc::clone(&self.active).read_owned().await;
        let instance = Arc::clone(&lease.instance);
        let (reply, result) = oneshot::channel();
        // Reserve command capacity before assigning the immutable admission.
        // Cancellation while waiting leaves this work unadmitted.
        let Ok(permit) = instance.commands.reserve().await else {
            return Err(instance.closed_error().await);
        };
        let invocation = next_identity()?;
        let identity = self.identity(
            Status {
                epoch: lease.epoch,
                digest: instance.digest,
            },
            Some(invocation),
            "admitted",
        );
        let observation =
            inspection::InvocationObservation::new(self.host.observer.clone(), identity);
        permit.send(Command::Route {
            event,
            reply,
            lease,
            observation,
            admission,
        });
        instance.receive(result).await
    }

    /// Observe the current admission epoch.
    pub async fn epoch(&self) -> u64 {
        self.status().await.epoch
    }

    /// Observe the active epoch and component identity under one admission read.
    pub async fn status(&self) -> Status {
        let active = self.active.read().await;
        Status {
            epoch: active.epoch,
            digest: active.instance.digest,
        }
    }

    /// Replace the expected activation within one fenced interval. Candidate
    /// instantiation happens first without grants. The deadline covers admission
    /// drain, snapshot and candidate initialization; expiry queues resume of
    /// the old Store before reopening host admission. Failed resume terminates
    /// that instance and reports its failure to subsequent callers.
    ///
    /// # Errors
    /// Returns expiry, a changed predecessor, or guest/engine failures.
    pub async fn replace(
        &self,
        prepared: Prepared,
        predecessor: u64,
        within: Duration,
    ) -> Result<Committed, Error> {
        let candidate_status = Status {
            epoch: predecessor,
            digest: prepared.digest,
        };
        self.record(candidate_status, None, "preparing");
        let candidate = match self.host.instantiate(prepared).await {
            Ok(candidate) => candidate,
            Err(error) => {
                self.record(candidate_status, None, "rejected");
                return Err(error);
            }
        };
        let deadline = Instant::now()
            .checked_add(within)
            .ok_or(Error::InvalidDeadline)?;
        let mut stage = Stage::Admission;
        self.record(candidate_status, None, "waiting");
        let result = tokio::time::timeout_at(deadline, async {
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
            self.record(candidate_status, None, "snapshot");
            let snapshot = old.snapshot(attempt).await?;
            stage = Stage::Initialization;
            self.record(candidate_status, None, "initializing");
            candidate.initialize(snapshot).await?;
            if Instant::now() >= deadline {
                return Err(Error::Expired(stage));
            }
            old.granted.store(false, Ordering::Release);
            *fence.active = Arc::new(Activation {
                epoch,
                instance: Arc::clone(&candidate),
            });
            fence.committed = true;
            candidate.activate();
            self.record(
                Status {
                    epoch,
                    digest: candidate.digest,
                },
                None,
                "committed",
            );
            let retirement = tokio::spawn(async move { old.dispose().await });
            Ok(Committed {
                epoch,
                digest: candidate.digest,
                snapshot,
                retirement,
            })
        })
        .await
        .unwrap_or_else(|_| Err(Error::Expired(stage)));
        if result.is_err() {
            self.record(candidate_status, None, "rejected");
        }
        result
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
    wake: Arc<tokio::sync::Notify>,
    failure: tokio::sync::watch::Receiver<Option<String>>,
}

impl Drop for Instance {
    fn drop(&mut self) {
        self.worker.abort();
    }
}

impl Instance {
    async fn closed_error(&self) -> Error {
        let mut failure = self.failure.clone();
        loop {
            if let Some(error) = failure.borrow_and_update().as_ref() {
                return Error::Engine(wasmtime::Error::msg(error.clone()));
            }
            if failure.changed().await.is_err() {
                return Error::Closed;
            }
        }
    }

    async fn receive<T>(&self, result: oneshot::Receiver<Result<T, Error>>) -> Result<T, Error> {
        match result.await {
            Ok(result) => result,
            Err(_) => Err(self.closed_error().await),
        }
    }

    fn activate(&self) {
        self.granted.store(true, Ordering::Release);
        self.wake.notify_one();
    }

    async fn initialize(&self, state: Snapshot) -> Result<(), Error> {
        let (reply, result) = oneshot::channel();
        if self
            .commands
            .send(Command::Initialize { state, reply })
            .await
            .is_err()
        {
            return Err(self.closed_error().await);
        }
        self.receive(result).await
    }

    async fn snapshot(&self, attempt: Arc<Mutex<Attempt>>) -> Result<Snapshot, Error> {
        let (reply, result) = oneshot::channel();
        if self
            .commands
            .send(Command::Snapshot {
                attempt,
                reply,
                gate: Arc::clone(&self.snapshot_gate),
            })
            .await
            .is_err()
        {
            return Err(self.closed_error().await);
        }
        self.receive(result).await
    }

    async fn dispose(&self) -> Result<(), Error> {
        let (reply, result) = oneshot::channel();
        if self
            .commands
            .send(Command::Dispose { reply })
            .await
            .is_err()
        {
            return Err(self.closed_error().await);
        }
        self.receive(result).await
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
        observation: inspection::InvocationObservation,
        admission: OwnedSemaphorePermit,
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
    limits: StoreLimits,
    output: Arc<policy::ServicesBudget>,
}

fn guest_result<T>(
    accessor: &Accessor<Context>,
    result: wasmtime::Result<Result<T, String>>,
) -> Result<T, Error> {
    match result.map_err(Error::Engine)? {
        Ok(value) => Ok(value),
        Err(message) => {
            accessor
                .with(|mut access| access.get().output.response(message.len()))
                .map_err(Error::Engine)?;
            Err(Error::Guest(message))
        }
    }
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
    policy: Arc<HostPolicy>,
    budgets: Arc<policy::Budgets>,
) -> Result<(Store<Context>, bindings::Router), Error> {
    let mut linker = Linker::new(engine);
    wasmtime_wasi::p2::add_to_linker_async(&mut linker)?;
    let mut imports = linker.instance("hew:live-router/services@0.1.0")?;
    let destinations = Arc::clone(&services);
    let destination_grant = Arc::clone(&granted);
    let service_budget = Arc::new(policy::ServicesBudget::new(Arc::clone(&policy), &budgets));
    let destination_budget = Arc::clone(&service_budget);
    imports.func_wrap_concurrent("destination", move |_accessor, (source,): (String,)| {
        let provider = Arc::clone(&destinations);
        let budget = Arc::clone(&destination_budget);
        let granted = destination_grant.load(Ordering::Acquire);
        // Reserve while constructing the host future, before it can become
        // queued work. Refusal drops lifted input immediately.
        let admitted = if granted {
            budget
                .begin(&[source.len()])
                .map(|permit| Some((permit, source)))
        } else {
            Ok(None)
        };
        Box::pin(async move {
            Ok((if let Some((_operation, source)) = admitted? {
                let result = budget.complete(provider.destination(source)).await?;
                let bytes = match &result {
                    Ok(Some(text)) | Err(text) => text.len(),
                    Ok(None) => 0,
                };
                budget.response(bytes)?;
                result
            } else {
                Err("production services are not granted".into())
            },))
        })
    })?;
    let emit_budget = Arc::clone(&service_budget);
    imports.func_wrap_concurrent("emit", move |_accessor, (message,): (Delivery,)| {
        let provider = Arc::clone(&services);
        let budget = Arc::clone(&emit_budget);
        let granted = granted.load(Ordering::Acquire);
        let admitted = if granted {
            budget
                .begin(&[message.destination.len(), message.body.len()])
                .map(|permit| Some((permit, message)))
        } else {
            Ok(None)
        };
        Box::pin(async move {
            Ok((if let Some((_operation, message)) = admitted? {
                let result = budget.complete(provider.emit(message)).await?;
                if let Err(text) = &result {
                    budget.response(text.len())?;
                }
                result
            } else {
                Err("production services are not granted".into())
            },))
        })
    })?;
    let mut resources = ResourceTable::new();
    resources.set_max_capacity(policy.max_host_resources);
    let mut wasi = WasiCtxBuilder::new();
    wasi.max_random_size(policy.max_payload_bytes as u64);
    let mut store = Store::new(
        engine,
        Context {
            wasi: wasi.build(),
            resources,
            limits: policy.store_limits(),
            output: service_budget,
        },
    );
    store.limiter(|context| &mut context.limits);
    store.set_fuel(policy.fuel)?;
    store.fuel_async_yield_interval(Some(100_000))?;
    let router = bindings::Router::instantiate_async(&mut store, component, &linker).await?;
    Ok((store, router))
}

#[allow(
    clippy::too_many_lines,
    reason = "one Store owner coordinates request admission and idle driver wakeups"
)]
async fn serve(
    accessor: &Accessor<Context>,
    router: Arc<bindings::Router>,
    mut commands: mpsc::Receiver<Command>,
    wake: Arc<tokio::sync::Notify>,
) -> wasmtime::Result<()> {
    let (driven, mut results) = mpsc::channel(1);
    let mut enabled = false;
    let mut driving = false;
    let mut redrive = false;
    let mut deadline = None;
    loop {
        let command = tokio::select! {
            command = commands.recv() => {
                let Some(command) = command else { break };
                if enabled {
                    redrive |= driving;
                    deadline = Some(Instant::now());
                }
                command
            }
            () = wake.notified() => {
                enabled = true;
                redrive |= driving;
                deadline = Some(Instant::now());
                continue;
            }
            result = results.recv(), if driving => {
                let delay: Option<u64> = result.ok_or_else(|| wasmtime::Error::msg("guest driver closed"))??;
                driving = false;
                deadline = if redrive {
                    redrive = false;
                    Some(Instant::now())
                } else {
                    delay.and_then(|delay| Instant::now().checked_add(Duration::from_millis(delay)))
                };
                continue;
            }
            () = async {
                if let Some(deadline) = deadline {
                    tokio::time::sleep_until(deadline).await;
                } else {
                    std::future::pending::<()>().await;
                }
            }, if enabled && !driving => {
                driving = true;
                deadline = None;
                accessor.spawn(Drive { router: Arc::clone(&router), reply: driven.clone() })?;
                continue;
            }
        };
        match command {
            Command::Route {
                event,
                reply,
                lease,
                observation,
                admission,
            } => {
                accessor.spawn(Route {
                    router: Arc::clone(&router),
                    event,
                    reply,
                    _lease: lease,
                    wake: Arc::clone(&wake),
                    observation,
                    _admission: admission,
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
                let _ = reply.send(guest_result(accessor, result));
            }
            Command::Resume => {
                // Complete this control call before dispatching later routes.
                let result = router.hew_live_router_handler().call_resume(accessor).await;
                guest_result(accessor, result).map_err(wasmtime::Error::new)?;
            }
            Command::Dispose { reply } => {
                let result = router
                    .hew_live_router_handler()
                    .call_dispose(accessor)
                    .await;
                let _ = reply.send(guest_result(accessor, result));
                break;
            }
        }
    }
    Ok(())
}

struct Drive {
    router: Arc<bindings::Router>,
    reply: mpsc::Sender<wasmtime::Result<Option<u64>>>,
}

impl AccessorTask<Context> for Drive {
    async fn run(self, accessor: &Accessor<Context>) -> wasmtime::Result<()> {
        let result = self
            .router
            .hew_live_router_handler()
            .call_drive(accessor)
            .await;
        let _ = self.reply.send(result).await;
        Ok(())
    }
}

struct Route {
    router: Arc<bindings::Router>,
    event: Event,
    reply: oneshot::Sender<Result<bool, Error>>,
    _lease: OwnedRwLockReadGuard<Arc<Activation>>,
    wake: Arc<tokio::sync::Notify>,
    observation: inspection::InvocationObservation,
    _admission: OwnedSemaphorePermit,
}

impl AccessorTask<Context> for Route {
    async fn run(mut self, accessor: &Accessor<Context>) -> wasmtime::Result<()> {
        let result = self
            .router
            .hew_live_router_handler()
            .call_route(accessor, self.event)
            .await;
        self.wake.notify_one();
        self.observation.finish(if matches!(&result, Ok(Ok(_))) {
            "completed"
        } else {
            "faulted"
        });
        let _ = self.reply.send(guest_result(accessor, result));
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
            Some(Poll::Ready(result)) => guest_result(accessor, result),
            Some(Poll::Pending) => guest_result(accessor, call.await),
        };
        let _ = self.reply.send(result);
        Ok(())
    }
}

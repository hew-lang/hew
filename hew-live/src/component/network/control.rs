//! The typed control actor's component operations and consumed reply owners.

use std::collections::HashMap;
use std::ffi::c_void;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use tokio::sync::{Notify, RwLock};

use super::super::cache::ArtifactCache;
use super::super::host::{Deployment, Event, Host, Prepared, Snapshot, Status};
use super::native::{Api, Driver, Pointer, Worker};
use super::{format_digest, parse_digest, Receipt, Result};

pub(super) struct Control {
    pub host: Arc<Host>,
    pub cache: Arc<ArtifactCache>,
    pub worker: Worker,
    pub handle: tokio::runtime::Handle,
    pub lifecycle: Mutex<Lifecycle>,
    pub drained: Notify,
    pub deployment: RwLock<Option<Deployment>>,
    pub prepared: Mutex<HashMap<[u8; 32], Prepared>>,
}

impl Control {
    async fn prepare(self: &Arc<Self>, bytes: Vec<u8>) -> Result<String> {
        self.host.policy().component(bytes.len())?;
        let digest = super::super::cache::digest(&bytes);
        if self
            .prepared
            .lock()
            .expect("prepared cache poisoned")
            .contains_key(&digest)
        {
            return Ok(format_digest(digest));
        }
        let control = Arc::clone(self);
        let prepared = tokio::task::spawn_blocking(move || {
            prepare_with_eviction(&control.host, &control.cache, &control.prepared, &bytes)
        })
        .await??;
        let digest = prepared.digest();
        self.prepared
            .lock()
            .expect("prepared cache poisoned")
            .insert(digest, prepared);
        Ok(format_digest(digest))
    }

    async fn candidate(self: &Arc<Self>, digest: [u8; 32]) -> Result<Prepared> {
        if let Some(prepared) = self
            .prepared
            .lock()
            .expect("prepared cache poisoned")
            .get(&digest)
            .cloned()
        {
            return Ok(prepared);
        }
        let control = Arc::clone(self);
        // Reclaim unused compiled entries before restoring a durable digest,
        // just as for a new save. Active/inflight clones keep their own lease.
        Ok(tokio::task::spawn_blocking(move || {
            let bytes = control
                .cache
                .load_limited(digest, control.host.policy().max_component_bytes)
                .map_err(super::super::host::Error::Cache)?;
            prepare_with_eviction(&control.host, &control.cache, &control.prepared, &bytes)
        })
        .await??)
    }

    async fn replace(
        self: &Arc<Self>,
        digest: String,
        predecessor: u64,
        within_ms: u64,
    ) -> Result<Receipt> {
        let digest = parse_digest(&digest)?;
        let prepared = self.candidate(digest).await?;
        let within = Duration::from_millis(within_ms);
        let active = self.deployment.read().await;
        if let Some(deployment) = active.as_ref() {
            let committed = deployment.replace(prepared, predecessor, within).await?;
            return Ok(Receipt {
                status: Status {
                    epoch: committed.epoch,
                    digest: committed.digest,
                },
                accepted: committed.snapshot.accepted,
            });
        }
        drop(active);
        let mut active = self.deployment.write().await;
        if active.is_some() || predecessor != 0 {
            return Err("deployment predecessor changed".into());
        }
        let deployment = tokio::time::timeout(
            within,
            self.host.activate(prepared, Snapshot { accepted: 0 }),
        )
        .await??;
        let status = deployment.status().await;
        *active = Some(deployment);
        Ok(Receipt {
            status,
            accepted: 0,
        })
    }

    async fn status(&self) -> Result<Status> {
        let active = self.deployment.read().await;
        match active.as_ref() {
            Some(deployment) => Ok(deployment.status().await),
            None => Ok(Status {
                epoch: 0,
                digest: [0; 32],
            }),
        }
    }

    async fn route(&self, event: Event) -> Result<bool> {
        let active = self.deployment.read().await;
        Ok(active
            .as_ref()
            .ok_or("no active deployment")?
            .route(event)
            .await?)
    }

    async fn preview(
        self: &Arc<Self>,
        digest: String,
        input: String,
        within_ms: u64,
    ) -> Result<String> {
        self.host.policy().payload(&[input.len()])?;
        let prepared = self.candidate(parse_digest(&digest)?).await?;
        let preview = serde_json::from_str(&input)?;
        let outcome = self
            .host
            .preview(prepared, preview, Duration::from_millis(within_ms))
            .await?;
        Ok(serde_json::to_string(&outcome)?)
    }

    pub async fn close(&self) -> Result<()> {
        {
            let mut lifecycle = self.lifecycle.lock().expect("control lifecycle poisoned");
            if lifecycle.closing {
                return Ok(());
            }
            lifecycle.closing = true;
        }
        loop {
            let notified = self.drained.notified();
            if self
                .lifecycle
                .lock()
                .expect("control lifecycle poisoned")
                .inflight
                == 0
            {
                break;
            }
            notified.await;
        }
        let native = self.worker.execute(Driver::close).await;
        let portable = if let Some(deployment) = self.deployment.write().await.take() {
            deployment.dispose().await?.map_err(Into::into)
        } else {
            Ok(())
        };
        native.and(portable)
    }

    fn dispatch<F, Fut>(self: Arc<Self>, replies: Replies, operation: F)
    where
        F: FnOnce(Arc<Self>) -> Fut + Send + 'static,
        Fut: std::future::Future<Output = Result<Vec<Value>>> + Send + 'static,
    {
        {
            let mut lifecycle = self.lifecycle.lock().expect("control lifecycle poisoned");
            if lifecycle.closing {
                drop(lifecycle);
                replies.finish(Err("component host closed".into()));
                return;
            }
            if lifecycle.inflight >= self.host.policy().max_control_operations {
                drop(lifecycle);
                replies.finish(Err(super::super::host::Error::Refused(
                    super::super::host::PolicyLimit::ControlOperations,
                )
                .into()));
                return;
            }
            lifecycle.inflight += 1;
        }
        let handle = self.handle.clone();
        let pending = Pending(Arc::clone(&self));
        handle.spawn(async move {
            replies.finish(operation(self).await);
            drop(pending);
        });
    }
}

fn prepare_with_eviction(
    host: &Host,
    cache: &ArtifactCache,
    prepared: &Mutex<HashMap<[u8; 32], Prepared>>,
    bytes: &[u8],
) -> std::result::Result<Prepared, super::super::host::Error> {
    use super::super::host::{Error, PolicyLimit};
    let mut remaining = prepared.lock().expect("prepared cache poisoned").len();
    loop {
        match host.prepare_cached(cache, bytes) {
            Err(error @ Error::Refused(PolicyLimit::PreparedComponents)) => {
                let mut entries = prepared.lock().expect("prepared cache poisoned");
                let unused = entries
                    .iter()
                    .find_map(|(digest, entry)| entry.cache_exclusive().then_some(*digest));
                if remaining == 0 || unused.is_none() {
                    return Err(error);
                }
                entries.remove(&unused.expect("unused entry identified"));
                remaining -= 1;
            }
            result => return result,
        }
    }
}

#[derive(Default)]
pub(super) struct Lifecycle {
    closing: bool,
    inflight: usize,
}

struct Pending(Arc<Control>);
impl Drop for Pending {
    fn drop(&mut self) {
        self.0
            .lifecycle
            .lock()
            .expect("control lifecycle poisoned")
            .inflight -= 1;
        self.0.drained.notify_one();
    }
}

// A consumed capacity-one Sink is moved between the callback and native owner
// thread. Channel-backed endpoints explicitly support transfer between threads.
struct Replies {
    sinks: Vec<usize>,
    api: Api,
}

enum Value {
    Number(u64),
    Boolean(bool),
    Text(String),
}

impl Replies {
    fn new(control: &Control, sinks: &[Pointer]) -> Self {
        Self {
            sinks: sinks.iter().map(|sink| *sink as usize).collect(),
            api: control.worker.api,
        }
    }

    fn finish(mut self, result: Result<Vec<Value>>) {
        let sinks = std::mem::take(&mut self.sinks);
        let api = self.api;
        {
            let result = result.map(|values| {
                for (sink, value) in sinks[1..sinks.len() - 1].iter().zip(values) {
                    let sink = *sink as Pointer;
                    // SAFETY: each capacity-one endpoint has its sole writer here.
                    unsafe {
                        match value {
                            Value::Number(value) => {
                                (api.sink_write)(sink, (&raw const value).cast(), size_of::<u64>());
                            }
                            Value::Boolean(value) => {
                                (api.sink_write)(
                                    sink,
                                    (&raw const value).cast(),
                                    size_of::<bool>(),
                                );
                            }
                            Value::Text(value) => {
                                (api.sink_write)(sink, value.as_ptr().cast(), value.len());
                            }
                        }
                    }
                }
            });
            let status: i64 = if let Err(error) = result {
                {
                    let text = error.to_string();
                    // SAFETY: last endpoint carries the independent error text.
                    unsafe {
                        (api.sink_write)(
                            *sinks.last().expect("error sink") as Pointer,
                            text.as_ptr().cast(),
                            text.len(),
                        );
                    }
                }
                1
            } else {
                0
            };
            // SAFETY: write completion after payload, then consume every endpoint.
            unsafe {
                (api.sink_write)(
                    sinks[0] as Pointer,
                    (&raw const status).cast(),
                    size_of::<i64>(),
                );
                for sink in sinks {
                    (api.sink_close)(sink as Pointer);
                }
            }
        }
    }
}

impl Drop for Replies {
    fn drop(&mut self) {
        let sinks = std::mem::take(&mut self.sinks);
        {
            for sink in sinks {
                // SAFETY: cancellation consumes the endpoints without a value.
                unsafe {
                    (self.api.sink_close)(sink as Pointer);
                }
            }
        }
    }
}

#[repr(C)]
struct Bytes {
    pointer: *const u8,
    offset: u32,
    length: u32,
}

#[repr(C)]
struct Callbacks {
    context: *const Control,
    prepare: unsafe extern "C" fn(*const Control, *const Bytes, Pointer, Pointer, Pointer),
    replace: unsafe extern "C" fn(
        *const Control,
        *const c_void,
        u64,
        u64,
        Pointer,
        Pointer,
        Pointer,
        Pointer,
        Pointer,
    ),
    status: unsafe extern "C" fn(*const Control, Pointer, Pointer, Pointer, Pointer),
    route: unsafe extern "C" fn(
        *const Control,
        *const c_void,
        *const c_void,
        Pointer,
        Pointer,
        Pointer,
    ),
    preview: unsafe extern "C" fn(
        *const Control,
        *const c_void,
        *const c_void,
        u64,
        Pointer,
        Pointer,
        Pointer,
    ),
}

unsafe fn read_text(pointer: *const c_void, maximum: usize) -> Result<String> {
    // SAFETY: ordinary extern callback borrows a compiler-owned immutable string.
    unsafe {
        let bytes = hew_cabi::string::string_as_bytes(pointer.cast());
        if bytes.len() > maximum {
            return Err("component host refused oversized control input".into());
        }
        Ok(std::str::from_utf8(bytes)?.to_owned())
    }
}

unsafe fn context(pointer: *const Control) -> Arc<Control> {
    // SAFETY: Driver retains the context until its server actor has drained.
    unsafe {
        Arc::increment_strong_count(pointer);
        Arc::from_raw(pointer)
    }
}

unsafe extern "C" fn prepare(
    pointer: *const Control,
    bytes: *const Bytes,
    status: Pointer,
    digest: Pointer,
    error: Pointer,
) {
    // SAFETY: bridge borrows immutable inputs for this callback only.
    let control = unsafe { context(pointer) };
    let replies = Replies::new(&control, &[status, digest, error]);
    // SAFETY: bridge borrows this complete Bytes carrier for the callback.
    let bytes = unsafe { &*bytes };
    if let Err(error) = control.host.policy().component(bytes.length as usize) {
        replies.finish(Err(error.into()));
        return;
    }
    // SAFETY: the bridge borrows the immutable slice for this callback.
    let bytes = unsafe {
        if bytes.length == 0 {
            Vec::new()
        } else {
            std::slice::from_raw_parts(
                bytes.pointer.add(bytes.offset as usize),
                bytes.length as usize,
            )
            .to_vec()
        }
    };
    control.dispatch(replies, move |control| async move {
        control
            .prepare(bytes)
            .await
            .map(|digest| vec![Value::Text(digest)])
    });
}

unsafe extern "C" fn replace(
    pointer: *const Control,
    digest: *const c_void,
    predecessor: u64,
    within_ms: u64,
    status: Pointer,
    epoch: Pointer,
    identity: Pointer,
    accepted: Pointer,
    error: Pointer,
) {
    // SAFETY: bridge retains the immutable input during this callback.
    let control = unsafe { context(pointer) };
    // SAFETY: bridge retains this immutable string during the callback.
    let digest = unsafe { read_text(digest, 64) };
    let replies = Replies::new(&control, &[status, epoch, identity, accepted, error]);
    control.dispatch(replies, move |control| async move {
        control
            .replace(digest?, predecessor, within_ms)
            .await
            .map(|receipt| {
                vec![
                    Value::Number(receipt.status.epoch),
                    Value::Text(format_digest(receipt.status.digest)),
                    Value::Number(receipt.accepted),
                ]
            })
    });
}

unsafe extern "C" fn status(
    pointer: *const Control,
    status: Pointer,
    epoch: Pointer,
    digest: Pointer,
    error: Pointer,
) {
    // SAFETY: Driver retains this context for the callback.
    let control = unsafe { context(pointer) };
    let replies = Replies::new(&control, &[status, epoch, digest, error]);
    control.dispatch(replies, move |control| async move {
        control.status().await.map(|status| {
            vec![
                Value::Number(status.epoch),
                Value::Text(format_digest(status.digest)),
            ]
        })
    });
}

unsafe extern "C" fn route(
    pointer: *const Control,
    source: *const c_void,
    body: *const c_void,
    status: Pointer,
    accepted: Pointer,
    error: Pointer,
) {
    // SAFETY: Driver retains context; bridge borrows immutable input text.
    let control = unsafe { context(pointer) };
    // SAFETY: bridge retains both immutable strings during the callback.
    let event = unsafe {
        (|| -> Result<Event> {
            Ok(Event {
                source: read_text(source, control.host.policy().max_payload_bytes)?,
                body: read_text(body, control.host.policy().max_payload_bytes)?,
            })
        })()
    };
    let replies = Replies::new(&control, &[status, accepted, error]);
    control.dispatch(replies, move |control| async move {
        control
            .route(event?)
            .await
            .map(|accepted| vec![Value::Boolean(accepted)])
    });
}

unsafe extern "C" fn preview(
    pointer: *const Control,
    digest: *const c_void,
    input: *const c_void,
    within_ms: u64,
    status: Pointer,
    outcome: Pointer,
    error: Pointer,
) {
    // SAFETY: Driver and the bridge retain this context and both immutable texts
    // until the synchronous callback has copied its local SDK inputs.
    let control = unsafe { context(pointer) };
    // SAFETY: bridge retains immutable inputs for this callback.
    let (digest, input) = unsafe {
        (
            read_text(digest, 64),
            read_text(input, control.host.policy().max_payload_bytes),
        )
    };
    let replies = Replies::new(&control, &[status, outcome, error]);
    control.dispatch(replies, move |control| async move {
        control
            .preview(digest?, input?, within_ms)
            .await
            .map(|outcome| vec![Value::Text(outcome)])
    });
}

pub(super) fn install(driver: &mut Driver, control: Arc<Control>) -> Result<()> {
    type Install = unsafe extern "C" fn(*const Callbacks);
    let callbacks = Callbacks {
        context: Arc::as_ptr(&control),
        prepare,
        replace,
        status,
        route,
        preview,
    };
    // SAFETY: same-build SDK bridge copies this complete callback table.
    unsafe {
        driver.symbol::<Install>(b"live_control_install")?(&raw const callbacks);
    }
    driver.context = Some(control);
    driver.start()
}

#[cfg(test)]
mod tests {
    use super::super::super::host::{Error, HostPolicy, PolicyLimit, ServiceCall, Services};
    use super::*;

    struct NoServices;
    impl Services for NoServices {
        fn destination(&self, _: String) -> ServiceCall<Option<String>> {
            panic!("cache lifecycle must not issue services")
        }
        fn emit(&self, _: super::super::super::host::Delivery) -> ServiceCall<()> {
            panic!("cache lifecycle must not emit")
        }
    }

    #[tokio::test]
    async fn policy_compiled_cache_evicts_unused_saves_without_releasing_live_roots() {
        let core = std::fs::read(
            std::env::var_os("HEW_LIVE_FOREIGN_COMPONENT")
                .expect("make test-live-component supplies the independent guest"),
        )
        .unwrap();
        let original = wit_component::ComponentEncoder::default()
            .module(&core)
            .unwrap()
            .adapter(
                "wasi_snapshot_preview1",
                wasi_preview1_component_adapter_provider::WASI_SNAPSHOT_PREVIEW1_REACTOR_ADAPTER,
            )
            .unwrap()
            .validate(true)
            .encode()
            .unwrap();
        let variant = |revision: u8| {
            let mut bytes = original.clone();
            // A valid custom section changes identity without changing code.
            bytes.extend_from_slice(&[0, 9, 7, b'v', b'e', b'r', b's', b'i', b'o', b'n', revision]);
            bytes
        };
        let host = Arc::new(
            Host::with_policy(
                Arc::new(NoServices),
                HostPolicy {
                    max_prepared_components: 2,
                    ..HostPolicy::default()
                },
            )
            .unwrap(),
        );
        let directory = tempfile::tempdir().unwrap();
        let cache = ArtifactCache::open(directory.path()).unwrap();
        let entries = Mutex::new(HashMap::new());
        let save = |bytes: &[u8]| {
            let value = prepare_with_eviction(&host, &cache, &entries, bytes)?;
            entries
                .lock()
                .unwrap()
                .insert(value.digest(), value.clone());
            Ok::<_, Error>(value)
        };
        let first = save(&variant(0)).unwrap();
        let first_digest = first.digest();
        let deployment = host
            .activate(first, Snapshot { accepted: 7 })
            .await
            .unwrap();
        for revision in 1..5 {
            let prepared = save(&variant(revision)).unwrap();
            let committed = deployment
                .replace(prepared, u64::from(revision), Duration::from_secs(2))
                .await
                .unwrap();
            assert_eq!(committed.snapshot.accepted, 7);
            committed.retirement.await.unwrap().unwrap();
            tokio::task::yield_now().await;
            assert!(entries.lock().unwrap().len() <= 2);
        }
        let held = save(&variant(10)).unwrap();
        assert!(matches!(
            save(&variant(11)),
            Err(Error::Refused(PolicyLimit::PreparedComponents))
        ));
        drop(held);
        drop(save(&variant(11)).unwrap());
        assert!(!entries.lock().unwrap().contains_key(&first_digest));
        let restored = save(&cache.load(first_digest).unwrap()).unwrap();
        let committed = deployment
            .replace(restored, 5, Duration::from_secs(2))
            .await
            .unwrap();
        assert_eq!(committed.snapshot.accepted, 7);
        committed.retirement.await.unwrap().unwrap();
        deployment.dispose().await.unwrap().unwrap();
    }
}

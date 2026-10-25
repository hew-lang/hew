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
    async fn prepare(&self, bytes: Vec<u8>) -> Result<String> {
        let host = Arc::clone(&self.host);
        let cache = Arc::clone(&self.cache);
        let prepared =
            tokio::task::spawn_blocking(move || host.prepare_cached(&cache, &bytes)).await??;
        let digest = prepared.digest();
        self.prepared
            .lock()
            .expect("prepared cache poisoned")
            .insert(digest, prepared);
        Ok(format_digest(digest))
    }

    async fn candidate(&self, digest: [u8; 32]) -> Result<Prepared> {
        if let Some(prepared) = self
            .prepared
            .lock()
            .expect("prepared cache poisoned")
            .get(&digest)
            .cloned()
        {
            return Ok(prepared);
        }
        let host = Arc::clone(&self.host);
        let cache = Arc::clone(&self.cache);
        Ok(tokio::task::spawn_blocking(move || host.restore(&cache, digest)).await??)
    }

    async fn replace(&self, digest: String, predecessor: u64, within_ms: u64) -> Result<Receipt> {
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

    async fn preview(&self, digest: String, input: String, within_ms: u64) -> Result<String> {
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

unsafe fn read_text(pointer: *const c_void) -> String {
    // SAFETY: ordinary extern callback borrows a compiler-owned immutable string.
    unsafe {
        String::from_utf8_lossy(hew_cabi::string::string_as_bytes(pointer.cast())).into_owned()
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
    let (control, bytes) = unsafe {
        let bytes = &*bytes;
        (
            context(pointer),
            if bytes.length == 0 {
                Vec::new()
            } else {
                std::slice::from_raw_parts(
                    bytes.pointer.add(bytes.offset as usize),
                    bytes.length as usize,
                )
                .to_vec()
            },
        )
    };
    let replies = Replies::new(&control, &[status, digest, error]);
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
    let digest = unsafe { read_text(digest) };
    let replies = Replies::new(&control, &[status, epoch, identity, accepted, error]);
    control.dispatch(replies, move |control| async move {
        control
            .replace(digest, predecessor, within_ms)
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
        Event {
            source: read_text(source),
            body: read_text(body),
        }
    };
    let replies = Replies::new(&control, &[status, accepted, error]);
    control.dispatch(replies, move |control| async move {
        control
            .route(event)
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
    let (control, digest, input) =
        unsafe { (context(pointer), read_text(digest), read_text(input)) };
    let replies = Replies::new(&control, &[status, outcome, error]);
    control.dispatch(replies, move |control| async move {
        control
            .preview(digest, input, within_ms)
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

//! A portable vibration monitor with a typed calibration service and snapshot.

use std::sync::Arc;
use std::time::Duration;

use super::host::{
    self, Error, Invocation, Observer, Prepared, Reply, RetainedState, ServiceCall, Status,
};

pub(super) mod bindings {
    wasmtime::component::bindgen!({
        path: "wit/telemetry",
        world: "telemetry",
        imports: { default: async | store | trappable },
        exports: { default: async | store },
    });
}

pub use bindings::hew::live_telemetry::readings::{Observation, Sample, Snapshot};

/// Supply the calibrated vibration limit associated with a sample sequence.
pub trait Calibration: Send + Sync + 'static {
    fn limit(&self, sequence: u64) -> ServiceCall<u32>;
}

/// A portable monitor host using the SDK's deployment lifecycle.
#[derive(Debug)]
pub struct Host(host::Host);

impl Host {
    /// Configure a calibration grant for active instances.
    ///
    /// # Errors
    /// Returns engine configuration errors.
    pub fn new(calibration: Arc<dyn Calibration>) -> Result<Self, Error> {
        Ok(Self(host::Host::telemetry(calibration)?))
    }

    /// Attach a metadata-only observer.
    #[must_use]
    pub fn with_observer(self, observer: Arc<dyn Observer>) -> Self {
        Self(self.0.with_observer(observer))
    }

    /// Validate and compile a component before admission is fenced.
    ///
    /// # Errors
    /// Returns validation, compilation or policy errors.
    pub fn prepare(&self, bytes: &[u8]) -> Result<Prepared, Error> {
        self.0.prepare(bytes)
    }

    /// Initialize a monitor with the supplied rolling window.
    ///
    /// # Errors
    /// Returns instantiation or initialization errors.
    pub async fn activate(self, prepared: Prepared, state: Snapshot) -> Result<Deployment, Error> {
        Ok(Deployment(
            Arc::new(self.0)
                .activate_with_state(prepared, RetainedState::Telemetry(state))
                .await?,
        ))
    }
}

/// A single portable monitor and its active generation.
#[derive(Debug)]
pub struct Deployment(host::Deployment);

impl Deployment {
    /// Admit a reading to the currently active monitor.
    ///
    /// # Errors
    /// Returns calibration, guest or admission errors.
    pub async fn observe(&self, sample: Sample) -> Result<Observation, Error> {
        match self.0.invoke(Invocation::Observe(sample)).await? {
            Reply::Observed(value) => Ok(value),
            Reply::Routed(_) => Err(Error::Engine(wasmtime::Error::msg(
                "unexpected router result",
            ))),
        }
    }

    /// Observe the active code identity and epoch together.
    pub async fn status(&self) -> Status {
        self.0.status().await
    }

    /// Replace the observed generation while retaining its rolling window.
    ///
    /// # Errors
    /// Returns expiry, a changed predecessor or guest errors.
    pub async fn replace(
        &self,
        prepared: Prepared,
        predecessor: u64,
        within: Duration,
    ) -> Result<Committed, Error> {
        let replacement = self
            .0
            .replace_with_state(prepared, predecessor, within)
            .await?;
        let RetainedState::Telemetry(snapshot) = replacement.snapshot else {
            return Err(Error::Engine(wasmtime::Error::msg(
                "unexpected router snapshot",
            )));
        };
        Ok(Committed {
            epoch: replacement.epoch,
            digest: replacement.digest,
            snapshot,
            retirement: replacement.retirement,
        })
    }

    /// Stop the active monitor and return its cleanup task.
    #[must_use]
    pub fn dispose(self) -> tokio::task::JoinHandle<Result<(), Error>> {
        self.0.dispose()
    }
}

/// A committed monitoring generation and separately tracked retirement.
#[derive(Debug)]
pub struct Committed {
    pub epoch: u64,
    pub digest: [u8; 32],
    pub snapshot: Snapshot,
    pub retirement: tokio::task::JoinHandle<Result<(), Error>>,
}

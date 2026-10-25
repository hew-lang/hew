//! Metadata attribution and explicitly simulated service responses.

use std::collections::VecDeque;
use std::fmt::Write as _;

use hew_observe_protocol::LiveExecution;
use serde::{Deserialize, Serialize};

use super::{
    Arc, Delivery, Deployment, Error, Event, Host, Mutex, Prepared, ServiceCall, Services,
    Snapshot, Stage, Status,
};

/// Metadata-only observation. Implementations must not re-enter this host.
/// This callback conveys no service grants and cannot authorize replacement.
pub trait Observer: Send + Sync + 'static {
    fn record(&self, event: &LiveExecution);
}

/// Admission survives caller loss. A terminated host task cannot assert that
/// the guest completed or that already-issued effects were rolled back.
pub(super) struct InvocationObservation {
    observer: Option<Arc<dyn Observer>>,
    identity: Box<LiveExecution>,
    terminal: bool,
}

impl InvocationObservation {
    pub(super) fn new(observer: Option<Arc<dyn Observer>>, identity: LiveExecution) -> Self {
        if let Some(observer) = &observer {
            observer.record(&identity);
        }
        Self {
            observer,
            identity: Box::new(identity),
            terminal: false,
        }
    }

    pub(super) fn finish(&mut self, phase: &str) {
        self.identity.phase = phase.into();
        self.terminal = true;
        if let Some(observer) = &self.observer {
            observer.record(&self.identity);
        }
    }
}

impl Drop for InvocationObservation {
    fn drop(&mut self) {
        if !self.terminal {
            self.finish("uncertain");
        }
    }
}

impl Deployment {
    pub(super) fn identity(
        &self,
        status: Status,
        invocation: Option<u64>,
        phase: &str,
    ) -> LiveExecution {
        let mut code_id = String::with_capacity(64);
        for byte in status.digest {
            write!(code_id, "{byte:02x}").expect("write into String");
        }
        LiveExecution {
            deployment_id: self.id,
            invocation_id: invocation,
            activation_epoch: status.epoch,
            code_id,
            phase: phase.into(),
            mode: self.host.mode.into(),
            semantic_profile: "hew:live-router@0.1.0".into(),
            coverage: if self.host.mode == "preview" {
                "explicit_services"
            } else {
                "metadata_only"
            }
            .into(),
        }
    }

    pub(super) fn record(&self, status: Status, invocation: Option<u64>, phase: &str) {
        if let Some(observer) = &self.host.observer {
            observer.record(&self.identity(status, invocation, phase));
        }
    }
}

/// One ordered response to the reference component's existing service imports.
/// An emit fixture simulates acknowledgement; it never sends a delivery.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ServiceFixture {
    Destination {
        source: String,
        result: Result<Option<String>, String>,
    },
    Emit {
        message: Delivery,
        result: Result<(), String>,
    },
}

/// Operator-supplied input and state, independent of the production instance.
/// Ordinary Observe traces do not contain these values or imply replay coverage.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Preview {
    pub state: Snapshot,
    pub input: Event,
    pub fixtures: Vec<ServiceFixture>,
}

/// A missing or mismatched fixture takes precedence over the guest's result.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PreviewOutcome {
    pub accepted: Option<bool>,
    pub needs_input: Option<String>,
    pub used_fixtures: usize,
    pub supplied_fixtures: usize,
    pub cleanup_complete: bool,
}

#[derive(Default)]
struct Simulation {
    fixtures: VecDeque<ServiceFixture>,
    supplied: usize,
    needs_input: Option<String>,
    changed: Arc<tokio::sync::Notify>,
}

struct SimulatedServices(Arc<Mutex<Simulation>>);

impl Services for SimulatedServices {
    fn destination(&self, source: String) -> ServiceCall<Option<String>> {
        let mut simulation = self
            .0
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let reply = if let Some(reason) = &simulation.needs_input {
            Err(reason.clone())
        } else if matches!(simulation.fixtures.front(), Some(ServiceFixture::Destination { source: expected, .. }) if *expected == source)
        {
            let Some(ServiceFixture::Destination { result, .. }) = simulation.fixtures.pop_front()
            else {
                unreachable!()
            };
            result
        } else {
            let request = format!(
                "destination({source:?}) requires an explicit matching response at fixture {}",
                simulation.supplied - simulation.fixtures.len()
            );
            simulation.needs_input = Some(request.clone());
            simulation.changed.notify_one();
            Err(request)
        };
        Box::pin(async move { reply })
    }

    fn emit(&self, message: Delivery) -> ServiceCall<()> {
        let mut simulation = self
            .0
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let reply = if let Some(reason) = &simulation.needs_input {
            Err(reason.clone())
        } else if matches!(simulation.fixtures.front(), Some(ServiceFixture::Emit { message: expected, .. }) if expected.destination == message.destination && expected.body == message.body)
        {
            let Some(ServiceFixture::Emit { result, .. }) = simulation.fixtures.pop_front() else {
                unreachable!()
            };
            result
        } else {
            let request = format!(
                "emit({:?}, {:?}) requires an explicit matching response at fixture {}",
                message.destination,
                message.body,
                simulation.supplied - simulation.fixtures.len()
            );
            simulation.needs_input = Some(request.clone());
            simulation.changed.notify_one();
            Err(request)
        };
        Box::pin(async move { reply })
    }
}

struct PreviewInstance(Arc<super::Instance>);
impl Drop for PreviewInstance {
    fn drop(&mut self) {
        self.0.worker.abort();
    }
}

impl Host {
    /// Execute prepared code with explicit state, input and ordered simulated
    /// services. This shares the host engine but never its production grants.
    /// No input or service body is sent to the metadata observer.
    ///
    /// # Errors
    /// Returns guest or lifecycle failures, or deadline expiry. Expiry aborts
    /// the isolated guest; cooperative guest cleanup is then unconfirmed.
    pub async fn preview(
        &self,
        prepared: Prepared,
        input: Preview,
        within: std::time::Duration,
    ) -> Result<PreviewOutcome, Error> {
        let changed = Arc::new(tokio::sync::Notify::new());
        let simulation = Arc::new(Mutex::new(Simulation {
            supplied: input.fixtures.len(),
            fixtures: input.fixtures.into(),
            needs_input: None,
            changed: Arc::clone(&changed),
        }));
        let isolated = Arc::new(Self {
            engine: self.engine.clone(),
            services: Arc::new(SimulatedServices(Arc::clone(&simulation))),
            observer: self.observer.clone(),
            mode: "preview",
        });
        tokio::time::timeout(within, async {
            let deployment = isolated.activate(prepared, input.state).await?;
            // Unlike a production caller's expiry, this boundary owns the
            // entire isolated preview. Retain explicit abort authority so a
            // missing fixture or deadline cannot leave a detached guest running.
            let instance = PreviewInstance(Arc::clone(&deployment.active.read().await.instance));
            let result = tokio::select! {
                biased;
                () = changed.notified() => None,
                result = deployment.route(input.input) => Some(result),
            };
            let cleanup_complete = result.is_some();
            if cleanup_complete {
                deployment.dispose().await.map_err(|_| Error::Closed)??;
            }
            drop(instance);
            let simulation = simulation
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let accepted = if simulation.needs_input.is_some() {
                None
            } else {
                Some(result.ok_or(Error::Closed)??)
            };
            Ok(PreviewOutcome {
                accepted,
                needs_input: simulation.needs_input.clone(),
                used_fixtures: simulation.supplied - simulation.fixtures.len(),
                supplied_fixtures: simulation.supplied,
                cleanup_complete,
            })
        })
        .await
        .map_err(|_| Error::Expired(Stage::Preview))?
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Default)]
    struct Recorded(Mutex<Vec<LiveExecution>>);

    impl Observer for Recorded {
        fn record(&self, event: &LiveExecution) {
            self.0.lock().unwrap().push(event.clone());
        }
    }

    #[test]
    fn interrupted_admission_keeps_its_identity_and_reports_uncertainty() {
        let recorded = Arc::new(Recorded::default());
        let identity = LiveExecution {
            deployment_id: 4,
            invocation_id: Some(9),
            activation_epoch: 3,
            code_id: "ab".repeat(32),
            phase: "admitted".into(),
            mode: "production".into(),
            semantic_profile: "hew:live-router@0.1.0".into(),
            coverage: "metadata_only".into(),
        };
        drop(InvocationObservation::new(
            Some(recorded.clone()),
            identity.clone(),
        ));
        let mut completed = InvocationObservation::new(Some(recorded.clone()), identity);
        completed.finish("completed");
        drop(completed);
        let events = recorded.0.lock().unwrap();
        assert_eq!(
            events
                .iter()
                .map(|event| event.phase.as_str())
                .collect::<Vec<_>>(),
            ["admitted", "uncertain", "admitted", "completed"]
        );
        assert!(events.iter().all(|event| event.deployment_id == 4
            && event.invocation_id == Some(9)
            && event.activation_epoch == 3));
    }
}
